let magic_version      = '\131'

let magic_small_int     = '\097'
let magic_large_int     = '\098'
let magic_float         = '\099'
let magic_atom_or_bool  = '\100'
let magic_pid           = '\103'
let magic_small_tuple   = '\104'
let magic_large_tuple   = '\105'
let magic_nil           = '\106'
let magic_string        = '\107'
let magic_list          = '\108'
let magic_new_reference = '\114'


module EBinary =
    struct
        type t = string

        let make l = Tools.implode l

        let to_string b = Tools.dump_dec b "<<" ">>"
    end


module ETuple =
    struct
        type 'a t = 'a array

        let make l = Array.of_list l

        let fold_left f i t =
            Array.fold_left f i t

        let to_string f tuple =
            fold_left (fun s e -> s ^ (f e) ^ ",") "" tuple
        
        let to_chars f tuple =
            match (Array.length tuple) < 256 with
                | true ->
                    magic_small_tuple :: ( (char_of_int (Array.length tuple)) :: (
                        fold_left (fun acc e -> acc @ (f e)) [] tuple
                    ))
                | false ->
                    magic_large_tuple :: ( (Tools.chars_of_int (Array.length tuple) 4) @ (
                        fold_left (fun acc e -> acc @ (f e)) [] tuple
                    ))
    end

module ETupler = functor(ET: sig
        type t
        val to_string : t -> string
        val to_chars : t -> char list
    end ) ->
    struct
        type t = ET.t array

        let make l = Array.of_list l

        let fold_left f i t =
            Array.fold_left f i t

        let to_string tuple =
            fold_left (fun s e -> s ^ (ET.to_string e) ^ ",") "" tuple
        
        let to_chars tuple =
            match (Array.length tuple) < 256 with
                | true ->
                    magic_small_tuple :: ( (char_of_int (Array.length tuple)) :: (
                        fold_left (fun acc e -> acc @ (ET.to_chars e)) [] tuple
                    ))
                | false ->
                    magic_large_tuple :: ( (Tools.chars_of_int (Array.length tuple) 4) @ (
                        fold_left (fun acc e -> acc @ (ET.to_chars e)) [] tuple
                    ))
    end


type eterm =
    | ET_int of Int32.t
    | ET_float of float
    | ET_atom of e_atom
    | ET_bool of bool
    | ET_tuple of e_tuple
    | ET_string of string
    | ET_list of eterm list
    | ET_improper_list of eterm list * eterm
    | ET_pid of e_pid
    | ET_ref of
          e_atom
        * Int32.t list
        * int          (* creation *)
and e_atom = string
and e_tuple = eterm ETuple.t
    (*TODO should be possible to define Tuple with a functor:
    Tupler(struct type eterm val to_string: eterm -> string end)
    so that using the tuple is easier: no need to pass the
    to_string or to_chars functions each time
    ... but then to_string and to_chars need to be define first! argh!!!
    *)
and e_pid = 
        e_atom (* node name *)
        * int  (* pid number *)
        * int  (* serial number *)
        * int  (* node creation ID *)

let et_pid_node_name pid = match pid with
    | ET_pid (name, _, _, _) -> name

let e_pid_to_string pid = match pid with
    (nodeName, pidNum, pidSerial, nodeCreation) ->
        Printf.sprintf
            "PID(%s, %i, %i, %i)"
            nodeName
            pidNum
            pidSerial
            nodeCreation

let make_e_pid nodeName pid_num serial creation =
    (
        nodeName,
        pid_num,
        serial,
        creation
    )

let rec to_string t = match t with
    | ET_int n   -> Int32.to_string n
    | ET_float n -> string_of_float n
    | ET_atom a  -> a
    | ET_bool b  -> string_of_bool b
    | ET_tuple l -> "{" ^ (ETuple.to_string to_string l) ^ "}"
    | ET_string s -> "\"" ^ s ^ "\""
    | ET_list s ->
        (List.fold_left (fun acc e -> acc ^ (to_string e) ^ ",") "[" s) ^ "]"
    | ET_improper_list (head, tail) ->
        (List.fold_left (fun acc e -> acc ^ to_string e) "[" head) ^ (to_string tail) ^ "]"
    | ET_pid pid -> e_pid_to_string pid
    | ET_ref (node, idList, creation) ->
        Printf.sprintf
            "Ref(%s, %s, %i)"
            node
            (List.fold_left
                (fun acc e -> Printf.sprintf "%s%lu" acc e)
                ""
                idList
            )
            creation

let rec to_chars t =
    match t with
        | ET_int n when n < 256l ->
            magic_small_int :: [char_of_int (Int32.to_int n)]
        | ET_int n ->
            magic_large_int :: (Tools.chars_of_int32 n 4)
        | ET_float f ->
            let s = Printf.sprintf "%.20e" f in
            let pad = String.make (31 - (String.length s)) '\000' in
            magic_float :: (Tools.explode s) @ (Tools.explode pad)
        | ET_atom a  ->
            magic_atom_or_bool
            :: (Tools.chars_of_int (String.length a) 2)
            @ (Tools.explode a)
        | ET_bool b ->
            to_chars (ET_atom (string_of_bool b))
        | ET_tuple l ->
            ETuple.to_chars to_chars l
        | ET_string s ->
            magic_string
            :: (Tools.chars_of_int (String.length s) 2)
            @ (Tools.explode s)
        | ET_list l ->
            magic_list
            :: (Tools.chars_of_int (List.length l) 4)
            @ (List.fold_left (fun acc e -> to_chars e) [] l)
        | ET_improper_list (l, tail) ->
            magic_list
            :: (Tools.chars_of_int (List.length l) 4)
            @ (List.fold_left (fun acc e -> to_chars e) [] l)
            @ (to_chars tail)
        | ET_pid (node, id, serial, creation) ->
            magic_pid
            :: (to_chars (ET_atom node))
            @  (Tools.chars_of_int id 4)
            @  (Tools.chars_of_int serial 4)
            @  (Tools.chars_of_int creation 1)
        | ET_ref (node, ids, creation) ->
            magic_new_reference
            :: (Tools.chars_of_int (List.length ids) 2)
            @  (to_chars (ET_atom node))
            @  (Tools.chars_of_int creation 1)
            @  (List.fold_left
                (fun acc e -> (acc @ Tools.chars_of_int32 e 4))
                []
                ids
            )


let to_binary t =
    EBinary.make (magic_version :: (to_chars t))

let rec of_stream =
    parser [< 'i ; stream >] ->
            match i with
                | n when n = magic_version ->
                    (parser [< t = term >] -> t) stream
                | _ -> failwith "magic version not recognized"

and term =
    parser [< 'magic; r = magic_parse magic >] -> r

and terms n l =
    match n with
        | 0 -> begin parser [< >] -> List.rev l end
        | _ -> begin parser [< t = term ; r = terms (n - 1) (t::l) >] -> r end

and magic_parse tag =
    match tag with
        | n when n = magic_small_int     -> parse_small_int
        | n when n = magic_large_int     -> parse_large_int
        | n when n = magic_float         -> parse_float
        | n when n = magic_atom_or_bool  -> parse_atom_or_bool
        | n when n = magic_small_tuple   -> parse_small_tuple
        | n when n = magic_large_tuple   -> parse_large_tuple
        (* TODO reference *)
        | n when n = magic_new_reference -> parse_new_reference
        (* TODO port *)
        | n when n = magic_pid      -> parse_pid
        | n when n = magic_nil     -> parse_nil
        | n when n = magic_string  -> parse_string
        | n when n = magic_list    -> parse_list
        (* TODO small big *)
        (* TODO large big *)
        (* TODO new cache *)
        (* TODO cached atom *)
        (* TODO fun *)
        | n -> failwith (Printf.sprintf 
            "Eterm.of_stream: term magic tag not recognized: %c"
            n
        )

and parse_small_int =
    parser [< i = Tools.eint32_n 1 >] ->
        ET_int i

and parse_large_int =
    parser [< n = Tools.eint32_n 4 >] ->
        ET_int n

and parse_float =
    parser [< s = Tools.string_n 31 >] ->
        ET_float (Tools.float_of_padded_string s)

and parse_atom_or_bool =
    parser [< n = Tools.eint_n 2; atom = Tools.string_n n >] ->
        match atom with
            | "true"  -> ET_bool true
            | "false" -> ET_bool false
            | str     -> ET_atom str 

and parse_small_tuple =
    parser [< n = Tools.eint_n 1; a = terms n [] >] ->
        ET_tuple (ETuple.make a)

and parse_large_tuple =
    parser [< n = Tools.eint_n 4; a = terms n [] >] ->
        ET_tuple (ETuple.make a)

and parse_string =
    parser [< n = Tools.eint_n 2; data = Tools.string_n n >] ->
        ET_string data

and parse_nil =
    parser [< >] ->
        ET_list []

and parse_list =
    parser [< n = Tools.eint_n 4; head = terms n []; tail = term >] ->
        match tail with
            | ET_list [] -> ET_list head
            | _ -> ET_improper_list (head, tail)

and parse_pid =
    parser [<
        t = term; (* TODO specify atom? *)
        idBytes = Tools.next_n 4;
        serial = Tools.eint_n 4;
        creationBytes = Tools.next_n 1
    >] ->
        match t with
            | ET_atom node ->
                let id = Tools.int_x_of_chars 28 idBytes in
                let creation = Tools.int_x_of_chars 2 creationBytes in
                ET_pid (node, id, serial, creation)
            | _ ->
                failwith "Eterm.of_stream: bad pid construct"

and parse_new_reference =
    parser [<
        idLen = Tools.eint_n 2;
        t = term; (* TODO specify atom? *)
        creationBytes = Tools.next_n 1;
        id0Bytes = Tools.next_n 4;
        stream
    >] ->
        match t with
            | ET_atom node ->
                let creation = Tools.int_x_of_chars 2 creationBytes in
                let id0 = Int32.of_int (Tools.int_x_of_chars 18 id0Bytes) in
                let ids = parse_ids (idLen - 1) [id0] stream in
                ET_ref (node, ids, creation)
            | _ ->
                failwith "Eterm.of_stream: bad new reference construct"

and parse_ids n acc stream =
    match n with
        | 0 ->
            List.rev acc
        | n when n > 0 ->
            begin
            parser [< id = Tools.eint32_n 4; s >] -> (* TODO no 18 bits mask? *)
                parse_ids (n-1) (id :: acc) s
            end
            stream
