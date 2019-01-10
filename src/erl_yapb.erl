-module(erl_yapb).

-export([
    encode/0
]).

encode() ->
    erl_yapb_nif:encode().
