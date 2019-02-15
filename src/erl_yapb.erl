-module(erl_yapb).

-export([
    encode/0,
    decode/3
]).

encode() ->
    erl_yapb_nif:encode().

decode(Bin, Name, Defs) ->
    erl_yapb_nif:decode(Bin, Name, Defs).
