-module(erl_yapb_tests).

-include_lib("eunit/include/eunit.hrl").

basic_test() ->
    ok = erl_yapb:encode().
