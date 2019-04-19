-module(erl_yapb_nif).

-compile(no_native).
-on_load(init/0).

-export([
    encode/0,
    decode/2,
    add_schema/1
]).

init() ->
    FileName = libyapb,
    SoName = case code:priv_dir(FileName) of
        {error, bad_name} ->
            case filelib:is_dir(filename:join(["..", priv])) of
                true ->
                    filename:join(["..", priv, FileName]);
                _ ->
                    filename:join([priv, FileName])
            end;
        Dir ->
            filename:join(Dir, FileName)
    end,
    ok = erlang:load_nif(SoName, 0).

-define(nif_stub,nif_stub_error(?LINE)).
nif_stub_error(Line) ->
    erlang:nif_error({nif_not_loaded,module,?MODULE,line,Line}).

encode() ->
    ?nif_stub.

decode(_Bin, _Name) ->
    ?nif_stub.

add_schema(_Schema) ->
    ?nif_stub.
