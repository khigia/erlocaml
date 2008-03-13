#!/bin/bash
EXIT=0

# start erlang first (ensure epmd is running)
echo; echo "Test $0: Run and stop an erlang node to ensure epmd is running."
erl -sname erl_test -setcookie cookie -noshell -s init stop

echo; echo "Test $0: Run the ocaml server."
# TODO hard coded cookie "cookie" and name "ocaml@devhost" :(
../ex/node_double &
OCAML_PID=$!
sleep 0.5

echo; echo "Test $0: Run erlang node to interact with ocaml node."
erl -sname erl_test -setcookie cookie -noshell -eval "
    OcamlNode = list_to_atom(\"ocaml@\" ++ net_adm:localhost()),
    pong = net_adm:ping(OcamlNode),
    {bytwo, OcamlNode} ! {self(), 21},
    42 = receive X -> X after 1 -> error end.
" -s init stop
EXIT=$?

echo; echo "Test $0: Kill ocaml server"
kill $OCAML_PID

echo; echo "Test $0: RESULT=${EXIT} (0=ok)"
exit ${EXIT}
