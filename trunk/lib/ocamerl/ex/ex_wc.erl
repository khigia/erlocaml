-module(ex_wc).

-ifdef(NDEBUG).
-export([doit/0]).
-else.
-compile([export_all, debug_info]).
-endif.


take_cycle(_L, N) when N < 0 ->
    error;
take_cycle([], N) when N > 0 ->
    error;
take_cycle(L, N) ->
    take_cycle(L, L, N, []).

take_cycle(_O, _L, 0, Acc) ->
    lists:reverse(Acc);
take_cycle(O, [], N, Acc) ->
    take_cycle(O, O, N, Acc);
take_cycle(O, [H|Q], N, Acc) ->
    take_cycle(O, Q, N-1, [H|Acc]).


new_mapper(ONode) ->
    R = make_ref(),
    Manager = {wc, ONode},
    Manager ! {new_mapper, R, self()},
    receive {ok, R, M} -> M after 1000 -> error end.

new_reducer(ENode) ->
    spawn(ENode, ?MODULE, reduce, []).

reduce() ->
    reduce(dict:new()).

reduce(Dict) ->
    receive
    {stop, P} ->
        P ! {reduce_result, Dict},
        ok;
    _I = {W, C} ->
        reduce(dict:update_counter(W, C, Dict))
    end.

collect_reducers(Reducers) ->
    lists:foreach(fun(Red) -> Red ! {stop, self()} end, Reducers),
    lists:foldl(
        fun(_Red, D) ->
            receive
            {reduce_result, Res} ->
                dict:merge(fun(_K,V1,V2) -> V1 + V2 end, D, Res)
            after 5000 ->
                error
            end
        end,
        dict:new(),
        Reducers
    ).

collect_mappers(Mappers) ->
    lists:foreach(fun(M) -> M ! stop end, Mappers).

process(Mappers, Reducers, Files) ->
    TaskManager = self(),
    Tasks = lists:foldl(
        fun({File, Mapper, Reducer}, Acc) ->
            R = make_ref(),
            Mapper ! {R, from_file, File, Reducer, TaskManager},
            [{R, Mapper, Reducer}|Acc]
        end,
        [],
        lists:zip3(
            Files,
            take_cycle(Mappers, length(Files)),
            take_cycle(Reducers, length(Files))
        )
    ),
    wait_tasks(Tasks).

wait_tasks([]) ->
    ok;
wait_tasks(Tasks) ->
    receive
    {ok, R, _Map} ->
        wait_tasks(lists:keydelete(R, 1, Tasks))
    % here could timeout and re-run some tasks ... partial tasks may need buffering in reduction
    end.

doit(Files, ONodes, MapperN, ENodes, ReducerN) ->
    io:format("files=~w~n", [Files]),
    Mappers = [new_mapper(ONode) || ONode <- take_cycle(ONodes, MapperN)],
    Reducers = [new_reducer(ENode) || ENode <- take_cycle(ENodes, ReducerN)],
    process(Mappers, Reducers, Files),
    collect_mappers(Mappers),
    Res = collect_reducers(Reducers),
    io:format("map-reduce wc on files ~p produced result: ~p~n", [Files, dict:to_list(Res)]),
    Res.
