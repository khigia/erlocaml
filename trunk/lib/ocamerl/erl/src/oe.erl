-module(oe).

-compile(export_all).

go() ->
    go(12345, 54321).

go(SendPort, RecvPort) ->
    go(SendPort, RecvPort, fun() -> print_mailbox() end).

go(SendPort, RecvPort, InCB) when is_function(InCB) ->
    Receiver = spawn_link(InCB),
    go(SendPort, RecvPort, Receiver);
go(SendPort, RecvPort, Receiver) when is_pid(Receiver) ->
    Sender = messenger(SendPort, RecvPort, Receiver),
    {messenger, Receiver, Sender}.

messenger(SendPort, RecvPort, Dest) ->
    {ok, LSock} = gen_tcp:listen(RecvPort, [binary, {packet, 0}, {active, false}]),
    spawn_link(?MODULE, receiver_acceptor, [LSock, Dest]),
    Sender = spawn_link(?MODULE, sender_loop, [SendPort]),
    Sender.


send(Sock, M) when is_port(Sock) ->
    Data = term_to_binary(M),
    io:format("Sending data: ~w~n", [Data]),
    ok = gen_tcp:send(Sock, Data),
    Sock;
send(Port, M) when is_integer(Port) ->
    {ok, Sock} = gen_tcp:connect("127.0.1.1", Port, [binary, {packet, 0}]),
    send(Sock, M).

close(Sock) ->
    ok = gen_tcp:close(Sock).

% wow


receiver_acceptor(LSock, Dest) ->
    {ok, Sock} = gen_tcp:accept(LSock),
    {ok, Bin} = do_recv(Sock, []),
    io:format("Received binary: ~w~n", [Bin]),
    Data = binary_to_term(Bin),
    io:format("Converted to term: ~w~n", [Data]),
    ok = gen_tcp:close(Sock),
    Dest ! Data,
    receiver_acceptor(LSock, Dest).

do_recv(Sock, Bs) ->
    case gen_tcp:recv(Sock, 0) of
        {ok, B} ->
            do_recv(Sock, [Bs, B]);
        {error, closed} ->
            {ok, list_to_binary(Bs)}
    end.


sender_loop(PortOrSock) ->
    receive
        M ->
            State = send(PortOrSock, M)
    end,
    sender_loop(State).

print_mailbox() ->
    receive
        M ->
            io:format("Received: ~w~n", [M])
    end,
    print_mailbox().
