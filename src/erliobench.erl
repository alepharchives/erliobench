-module(erliobench).
-export([main/1]).

-define(FILE_MOD, file).

main([NumWorkers, Path]) ->
    io:format("Starting the test.~n", []),
    run(list_to_integer(NumWorkers), Path);
main(_) ->
    io:format(standard_error, "usage: erliobench num_workers path~n", []),
    erlang:halt().


run(N, Path) ->
    spawn_workers(N, Path),
    put(start_time, erlang:now()),
    collect_data(0).


spawn_workers(0, _) ->
    ok;
spawn_workers(N, Path) ->
    Self = self(),
    spawn_link(fun() -> worker(Self, N, Path) end),
    spawn_workers(N-1, Path).


collect_data(Total) ->
    case Total rem 10000 of
        0 when Total > 0 ->
            TimeDiff = timer:now_diff(now(), get(start_time)),
            Rate = float(Total) / (float(TimeDiff) / 100000.0),
            log("Rate: ~f~n", [Rate]);
        _ ->
            ok
    end,
    receive {ops, Count} ->
        case get(start_time) of
            undefined -> put(start_time, now());
            _ -> ok
        end,
        collect_data(Total + Count)
    end.


worker(Parent, N, Path) ->
    log("Booting worker: ~p~n", [N]),
    {ok, Fd} = ?FILE_MOD:open(Path, [read, write, raw, binary]),
    put(bin_data, mk_bin(1024)),
    worker_loop(Parent, Fd, 0).


worker_loop(Parent, Fd, Count) when Count >= 1000 ->
    Parent ! {ops, Count*3},
    worker_loop(Parent, Fd, 0);
worker_loop(Parent, Fd, Count) ->
    case random:uniform() >= 0.5 of
        true ->
            {ok, _} = ?FILE_MOD:pread(Fd, 0, 1024);
        false ->
            ok = ?FILE_MOD:pwrite(Fd, 0, get(bin_data))
    end,
    worker_loop(Parent, Fd, Count+1).


mk_bin(Size) when is_number(Size), Size > 0 ->
    << <<0>> || _ <- lists:seq(0, Size-1) >>.


log(Fmt, Args) ->
    io:format(standard_error, Fmt, Args).
