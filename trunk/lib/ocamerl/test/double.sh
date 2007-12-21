EXIT=0

echo; echo "Test $0: Run an erlang node which will interact with ocaml server"
erl -sname erl_test_server -pa ../erl/ebin -cookie testing.cookie -noshell -detached
sleep 1

echo; echo "Test $0: Init a server on erlang node to enable connection with ocaml server"
erl -sname erl_test_client -cookie testing.cookie -noshell -eval '
    Server = list_to_atom("erl_test_server@" ++ net_adm:localhost()),
    pong = net_adm:ping(Server),
    Receiver = rpc:call(Server, erlang, spawn, [oe, print_mailbox_in_file, ["tmp/server.mb"]]),
    Sender = rpc:call(Server, oe, messenger, [12345, 54321, Receiver]),
    rpc:call(Server, erlang, register, [ocaml_receiver, Receiver]),
    R = rpc:call(Server, erlang, whereis, [ocaml_receiver]),
    rpc:call(Server, erlang, register, [ocaml_sender, Sender]),
    S = rpc:call(Server, erlang, whereis, [ocaml_sender]),
    io:format("Created ocaml receiver ~w and ocaml sender ~w~n", [R, S]).
' -s init stop

echo; echo "Test $0: Run the ocaml server"
../ocaml/ex/double -recv 12345 -send 54321 &
OCAML_PID=$!
sleep 0.5

DATATEST="{1,2,true,{3,4},3.14}"
EXPECTED="{2,4,true,{6,8},6.28000}"
echo; echo "Test $0: Send $DATATEST to erlang ocaml sender"
erl -sname erl_test_client -cookie testing.cookie -noshell -eval "
    Server = list_to_atom(\"erl_test_server@\" ++ net_adm:localhost()),
    pong = net_adm:ping(Server),
    % R = rpc:call(Server, erlang, whereis, [ocaml_receiver]),
    % S = rpc:call(Server, erlang, whereis, [ocaml_sender]),
    % io:format(\"Recap: ocaml receiver ~w and ocaml sender ~w~n\", [R, S]),
    In = $DATATEST,
    erlang:send({ocaml_sender, Server}, In).
" -s init stop

echo; echo "Test $0: Check erlang received expected data from ocaml"
sleep 1
cat "tmp/server.mb" | while read LINE; do
    if [ $EXPECTED == $LINE ] ;
    then
        echo "OK : $LINE"
    else
        echo "BAD : $LINE";
        exit 1
    fi
done
EXIT=$?

echo; echo "Test $0: Stop erlang server node"
erl -sname erl_test_client -noshell -cookie testing.cookie -eval '
    Server = list_to_atom("erl_test_server@" ++ net_adm:localhost()),
    pong = net_adm:ping(Server),
    rpc:call(Server, erlang, send, [ocaml_receiver, stop]),
    rpc:call(Server, init, stop, []).
' -s init stop

echo; echo "Test $0: Kill ocaml server"
kill $OCAML_PID

exit ${EXIT}
