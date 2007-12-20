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

let dump_dec str initStr endStr =
    let l = explode str in
    let rec l_to_s chars s =
        match chars with
            | h::t -> l_to_s t (s ^ (string_of_int (int_of_char h)) ^ ",")
            | [] -> s
    in
    (l_to_s l initStr) ^ endStr


