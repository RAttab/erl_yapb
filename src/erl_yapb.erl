-module(erl_yapb).

-export([
    encode/0,
    decode/2,
    add_schema/1
]).

encode() ->
    erl_yapb_nif:encode().

decode(Bin, Name) ->
    erl_yapb_nif:decode(Bin, Name).

add_schema(Schema) ->
    %io:format(user, "DSERP~p~n", [Schema]),
    erl_yapb_nif:add_schema(Schema).
