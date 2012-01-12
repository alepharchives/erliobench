-module(erlfd).
-behaviour(gen_server).

-export([open/2, pread/3, pwrite/3]).

-export([init/1, terminate/2, code_change/3]).
-export([handle_call/3, handle_cast/2, handle_info/2]).


-define(FILE_MOD, file).


open(Path, Opts) ->
    proc_lib:start_link(?MODULE, init, [{Path, Opts}]).


pread(Fd, Pos, Size) ->
    gen_server:call(Fd, {pread, Pos, Size}).


pwrite(Fd, Pos, Data) ->
    gen_server:call(Fd, {pwrite, Pos, Data}).


init({Path, _Opts}) ->
    case ?FILE_MOD:open(Path, [read, write, raw, binary]) of
        {ok, Fd} ->
            proc_lib:init_ack({ok, self()}),
            gen_server:enter_loop(?MODULE, [], Fd);
        Other ->
            proc_lib:init_ack(Other)
    end.


terminate(_Reason, _Fd) ->
    ok.


handle_call({pread, Pos, Size}, _From, Fd) ->
    {reply, ?FILE_MOD:pread(Fd, Pos, Size), Fd};
handle_call({pwrite, Pos, Data}, _From, Fd) ->
    {reply, ?FILE_MOD:pwrite(Fd, Pos, Data), Fd}.


handle_cast(Msg, Fd) ->
    {stop, {unknown_cast, Msg}, Fd}.


handle_info(Msg, Fd) ->
    {stop, {unknown_info, Msg}, Fd}.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

