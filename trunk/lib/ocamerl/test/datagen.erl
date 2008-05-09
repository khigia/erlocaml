-module(datagen).
-export([doit/0]).

data_set() -> [
    {42,         "ET_int 42l"},
    {3.14,       "ET_float 3.14"},
    {bea,        "ET_atom \"bea\""},
    {true,       "ET_bool true"},
    {<<"test">>, "ET_bin [|'t'; 'e'; 's'; 't';|]"},
    {{true, 42}, "ET_tuple [| ET_bool true; ET_int 42l; |]"},
    {"bea",      "ET_string \"bea\""},
    {[true, 42], "ET_list [ ET_bool true; ET_int 42l; ]"},
    {undefined,  "ET_improper_list ([ ET_bool true; ET_int 42l; ], ET_bool false)"},
    {list_to_pid("<0.4.1>"), {undefined, "ET_pid (_,_,_,_)"}},
    {undefined, "ET_pid (\"crazy\",1,2,3)"},
    {make_ref(), {undefined, "ET_ref (_,_,_)"}},
    {undefined,  "ET_ref (\"thisnode\", [1l; 2l;], 42)"}
].

term_to_ocaml_list_string(undefined) ->
    "None";
term_to_ocaml_list_string(Term) ->
    B = term_to_binary(Term),
    [H|T] = binary_to_list(B),
    P = fun(I) -> io_lib:format("'\\~3.10.0B'", [I]) end,
    "Some [" ++
    lists:foldl(
        fun(I, Acc) -> Acc ++ ";" ++ P(I) end,
        P(H),
        T
    ) ++
    "]".

data_set_to_ocaml_list_string(DataSet) ->
    "[" ++
    lists:foldl(
        fun
        ({undefined, S}, Acc) ->
            io_lib:format(
                "~s(Exact (~s), None);~n",
                [Acc, S]
            );
        ({T, {undefined, M}}, Acc) ->
            io_lib:format(
                "~s(Match (fun x -> match x with ~s -> true | _ -> false), ~s);~n",
                [Acc, M, term_to_ocaml_list_string(T)]
            );
        ({T, S}, Acc) ->
            io_lib:format(
                "~s(Exact (~s), ~s);~n",
                [Acc, S, term_to_ocaml_list_string(T)]
            )
        end,
        "",
        DataSet
    ) ++ "]".

doit() ->
    io:format("
(* !!!!!!!!!!!!!!!!!!!!!!!! *)
(* WARNING: generated code! *)
(* !!!!!!!!!!!!!!!!!!!!!!!! *)

open Ocamerl
open Eterm

type ocaml_check_t =
    | Exact of Eterm.t
    | Match of (Eterm.t -> bool)

(* (ocaml_check_t * char list option) list *)
let dataset = ~s
",
        [data_set_to_ocaml_list_string(data_set())]
    ).
