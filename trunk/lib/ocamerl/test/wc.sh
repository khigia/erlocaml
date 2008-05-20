#!/bin/bash

EXIT=0
OCAML_NODE_NAME=o1
ERLANG_NODE_NAME=e1
NODE_COOKIE=cookie

echo; echo "Test $0: Run the ocaml server."
./ex_node_wc.byte -name $OCAML_NODE_NAME -cookie $NODE_COOKIE &
OCAML_PID=$!
sleep 0.1

echo; echo "Test $0: Run erlang node to interact with ocaml node."
erl -pa beam -sname $ERLANG_NODE_NAME -setcookie $NODE_COOKIE -noshell -eval "
    OcamlNodes = [list_to_atom(\"$OCAML_NODE_NAME@\" ++ net_adm:localhost())],
    ErlangNodes = [list_to_atom(\"$ERLANG_NODE_NAME@\" ++ net_adm:localhost())],
    D = ex_wc:doit([\"test/f1.txt\", \"test/f2.txt\"], OcamlNodes, 2, ErlangNodes, 2),
    1 = dict:fetch(\"one1\", D),
    1 = dict:fetch(\"three1\", D),
    1 = dict:fetch(\"one2\", D),
    1 = dict:fetch(\"three2\", D),
    2 = dict:fetch(\"two\", D),
    2 = dict:fetch(\"four\", D),
    ok.
" -s init stop
EXIT=$?

echo; echo "Test $0: Kill ocaml server"
kill $OCAML_PID

echo; echo "Test $0: RESULT=${EXIT} (0=ok)"
exit ${EXIT}


