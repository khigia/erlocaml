let implode chars =
    let s = String.create (List.length chars) in
    let rec loop i = function
        | x :: rest ->
            s.[i] <- x;
            loop (i+1) rest
        | [] ->
            s
    in
    loop 0 chars

let explode s =
    let rec exp i l =
        if i < 0 then l else exp (i - 1) (s.[i] :: l)
    in
    exp (String.length s - 1) []

let rec nnext n acc stream =
    match n with
        | 0 ->
            List.rev acc
        | _ ->
            nnext (n - 1) ((Stream.next stream) :: acc) stream

let float_of_padded_string s =
    try
        let pos = String.index s '0' in
        float_of_string (String.sub s 0 pos)
    with
        Not_found ->
            float_of_string s

let rec int_of_chars chars r =
    match chars with
        | b::rest ->
            int_of_chars rest (r * 256 + (int_of_char b))
        | [] ->
            r

let rec chars_of_int v chars n =
    match n > 0 with
        | true ->
            chars_of_int (v / 256) ((char_of_int (v mod 256))::chars) (n - 1)
        | false ->
            if v == 0 then chars else failwith "integer too big"


let magic_version      = '\131'
let magic_small_int    = '\097'
let magic_large_int    = '\098'
let magic_float        = '\099'
let magic_atom_or_bool = '\100'
let magic_small_tuple  = '\104'
let magic_large_tuple  = '\105'
let magic_nil          = '\106'
let magic_string       = '\107'
let magic_list         = '\108'


module EBinary =
    struct
        type t = string

        let make l = implode l

        let to_string b =
            let l = explode b in
            let rec l_to_s chars s =
                match chars with
                    | h::t -> l_to_s t (s ^ (string_of_int (int_of_char h)) ^ ",")
                    | [] -> s
            in
            (l_to_s l "<<") ^ ">>"
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
                    magic_large_tuple :: ( (chars_of_int (Array.length tuple) [] 4) @ (
                        fold_left (fun acc e -> acc @ (f e)) [] tuple
                    ))
    end


type eterm =
    | ET_int of Int32.t
    | ET_float of float
    | ET_atom of string
    | ET_bool of bool
    | ET_tuple of eterm ETuple.t
    | ET_string of string
    | ET_list of eterm list
    | ET_improper_list of eterm list * eterm

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

let rec to_chars t =
    match t with
        | ET_int n when n < 256l ->
            magic_small_int :: [char_of_int (Int32.to_int n)]
        | ET_int n ->
            magic_large_int :: (chars_of_int (Int32.to_int n) [] 4)
        | ET_float f ->
            let s = Printf.sprintf "%.20e" f in
            let pad = String.make (31 - (String.length s)) '\000' in
            magic_float :: (explode s) @ (explode pad)
        | ET_atom a  ->
            magic_atom_or_bool
            :: (chars_of_int (String.length a) [] 2)
            @ (explode a)
        | ET_bool b ->
            to_chars (ET_atom (string_of_bool b))
        | ET_tuple l ->
            ETuple.to_chars to_chars l
        | ET_string s ->
            magic_string
            :: (chars_of_int (String.length s) [] 2)
            @ (explode s)
        | ET_list l ->
            magic_list
            :: (chars_of_int (List.length l) [] 4)
            @ (List.fold_left (fun acc e -> to_chars e) [] l)
        | ET_improper_list (l, tail) ->
            magic_list
            :: (chars_of_int (List.length l) [] 4)
            @ (List.fold_left (fun acc e -> to_chars e) [] l)
            @ (to_chars tail)

let to_binary t =
    EBinary.make (magic_version :: (to_chars t))

let rec of_stream =
    parser [< 'i ; stream >] ->
        match i with
            | n when n = magic_version ->
                (parser [< t = term >] -> t) stream
            | _ -> failwith "magic version not recognize"

and term =
    parser [< 'magic; r = magic_parse magic >] -> r

and terms n l =
    match n with
        | 0 -> begin parser [< >] -> List.rev l end
        | _ -> begin parser [< t = term ; r = terms (n - 1) (t::l) >] -> r end

and magic_parse tag =
    match tag with
        | n when n = magic_small_int    -> parse_small_int
        | n when n = magic_large_int    -> parse_large_int
        | n when n = magic_float        -> parse_float
        | n when n = magic_atom_or_bool -> parse_atom_or_bool
        | n when n = magic_small_tuple  -> parse_small_tuple
        | n when n = magic_large_tuple  -> parse_large_tuple
        (* TODO reference *)
        (* TODO port *)
        (* TODO pid *)
        | n when n = magic_nil     -> parse_nil
        | n when n = magic_string  -> parse_string
        | n when n = magic_list    -> parse_list
        (* TODO small big *)
        (* TODO large big *)
        (* TODO new cache *)
        (* TODO cached atom *)
        (* TODO new reference *)
        (* TODO fun *)
        | _ -> failwith "magic tag not recognized"

and parse_small_int =
    parser [< i = eint 1 >] ->
        ET_int (Int32.of_int i)

and parse_large_int =
    parser [< n = eint 4 >] ->
        ET_int (Int32.of_int n)

and parse_float =
    parser [< s = string_n 31 >] ->
        ET_float (float_of_padded_string s)

and parse_atom_or_bool =
    parser [< n = eint 2; atom = string_n n >] ->
        match atom with
            | "true"  -> ET_bool true
            | "false" -> ET_bool false
            | str     -> ET_atom str 

and parse_small_tuple =
    parser [< n = eint 1; a = terms n [] >] ->
        ET_tuple (ETuple.make a)

and parse_large_tuple =
    parser [< n = eint 4; a = terms n [] >] ->
        ET_tuple (ETuple.make a)

and parse_string =
    parser [< n = eint 2; data = string_n n >] ->
        ET_string data

and parse_nil =
    parser [< >] ->
        ET_list []

and parse_list =
    parser [< n = eint 4; head = terms n []; tail = term >] ->
        match tail with
            | ET_list [] -> ET_list head
            | _ -> ET_improper_list (head, tail)

and string_n n =
  parser [< s = nnext n [] >] -> implode s

and eint n =
  parser [< s = nnext n [] >] -> int_of_chars s 0
