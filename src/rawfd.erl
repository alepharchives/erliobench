-module(rawfd).
-export([open/2, pread/3, pwrite/3]).

-on_load(init/0).

open(_Path, _Options) ->
    not_loaded(?LINE).

pread(_Fd, _Pos, _Size) ->
    not_loaded(?LINE).

pwrite(_Fd, _Pos, _Data) ->
    not_loaded(?LINE).

init() ->
    PrivDir = case code:priv_dir(?MODULE) of
        {error, _} ->
            EbinDir = filename:dirname(code:which(?MODULE)),
            AppPath = filename:dirname(EbinDir),
            filename:join(AppPath, "priv");
        Path ->
            Path
    end,
    erlang:load_nif(filename:join(PrivDir, "rawfd"), 0).

not_loaded(Line) ->
    erlang:nif_error({not_loaded, [{module, ?MODULE}, {line, Line}]}).
