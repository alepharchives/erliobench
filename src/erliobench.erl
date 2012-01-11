-module(erliobench).
-export([main/1]).

main([NumWorkers, Path]) ->
    io:format("Starting the test.~n", []),
    run(list_to_integer(NumWorkers), Path);
main(_) ->
    io:format(standard_error, "usage: erliobench num_workers path~n", []),
    erlang:halt().


run(N, Path) ->
    spawn_workers(N, Path),
    collect_data(0.0, 0.0).


spawn_workers(0, _) ->
    ok;
spawn_workers(N, Path) ->
    Self = self(),
    spawn_link(fun() -> worker(Self, N, Path) end),
    spawn_workers(N-1, Path).


collect_data(Count, Total) ->
    case round(Count) rem 10000 of
        0 when Count > 0, Total > 0 ->
            log("Rate: ~f/~f = ~f~n", [Count, Total, Count / Total]);
        _ ->
            ok
    end,
    receive {rec, Time} ->
        collect_data(Count + 1000.0, Total + Time)
    end.


worker(Parent, N, Path) ->
    log("Booting worker: ~p~n", [N]),
    {ok, Fd} = file:open(Path, [read, write, raw, binary]),
    put(start_time, erlang:now()),
    put(bin_data, mk_bin(1024)),
    worker_loop(Parent, Fd, 0).


worker_loop(Parent, Fd, Count) when Count >= 1000 ->
    Diff = timer:now_diff(now(), get(start_time)),
    Parent ! {rec, float(Diff) / 100000.0},
    put(start_time, now()),
    worker_loop(Parent, Fd, 0);
worker_loop(Parent, Fd, Count) ->
    case random:uniform() >= 0.5 of
        true ->
            {ok, <<_:1024/binary>>} = file:pread(Fd, 0, 1024);
        false ->
            ok = file:pwrite(Fd, 0, get(bin_data))
    end,
    worker_loop(Parent, Fd, Count+1).


mk_bin(Size) when is_number(Size), Size > 0 ->
    << <<0>> || _ <- lists:seq(0, Size-1) >>.


log(Fmt, Args) ->
    io:format(standard_error, Fmt, Args).
