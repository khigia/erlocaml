-module(oe).

-compile(export_all).

go() ->
    go(12345, 54321).

go(SendPort, RecvPort) ->
    go(SendPort, RecvPort, fun() -> print_mailbox() end).

go(SendPort, RecvPort, InCB) when is_function(InCB) ->
    Receiver = spawn(InCB),
    go(SendPort, RecvPort, Receiver);
go(SendPort, RecvPort, Receiver) when is_pid(Receiver) ->
    Sender = messenger(SendPort, RecvPort, Receiver),
    {messenger, Receiver, Sender}.

messenger(SendPort, RecvPort, Dest) ->
    spawn(?MODULE, receiver_loop, [RecvPort, Dest]),
    Sender = spawn(?MODULE, sender_loop, [SendPort]),
    Sender.


send(Sock, M) when is_port(Sock) ->
    Data = term_to_binary(M),
    %io:format("Sending data: ~w~n", [Data]),
    ok = gen_tcp:send(Sock, Data),
    Sock;
send(Port, M) when is_integer(Port) ->
    %io:format("Opening connection to ~w~n", [Port]),
    {ok, Sock} = gen_tcp:connect(net_adm:localhost(), Port, [binary, {packet, 0}]),
    send(Sock, M).

close(Sock) ->
    ok = gen_tcp:close(Sock).


receiver_loop(RecvPort, Dest) ->
    {ok, LSock} = gen_tcp:listen(RecvPort, [binary, {packet, 0}, {active, false}]),
    receiver_acceptor(LSock, Dest).

receiver_acceptor(LSock, Dest) ->
    {ok, Sock} = gen_tcp:accept(LSock),
    {ok, Bin} = do_recv(Sock, []),
    %io:format("Received binary: ~w~n", [Bin]),
    Data = binary_to_term(Bin),
    %io:format("Converted to term: ~w~n", [Data]),
    ok = gen_tcp:close(Sock),
    Dest ! {msg, Data},
    receiver_acceptor(LSock, Dest).

do_recv(Sock, Bs) ->
    case gen_tcp:recv(Sock, 0) of
        {ok, B} ->
            do_recv(Sock, [Bs, B]);
        {error, closed} ->
            {ok, list_to_binary(Bs)}
    end.


sender_loop(PortOrSock) ->
    %receive _ -> ok end, sender_loop(PortOrSock).
    receive
        M ->
            %io:format("Sending ~w on ~w~n", [M, PortOrSock]),
            State = send(PortOrSock, M)
    end,
    sender_loop(State).

print_mailbox() ->
    receive
        {msg, M} ->
            io:format("Received: ~w~n", [M])
    end,
    print_mailbox().

print_mailbox_in_file(File) when is_list(File) ->
    {ok, Dev} = file:open(File, [write]),
    print_mailbox_in_file(Dev);
print_mailbox_in_file(Dev) ->
    receive
        {msg, M} ->
            io:format(Dev, "~w~n", [M]),
            print_mailbox_in_file(Dev);
        stop ->
            file:close(Dev)
    end.
