#!/usr/bin/env escript
%%! -pz _build/default/lib/timing/ebin _build/default/lib/timothy/ebin _build/default/lib/bear/ebin _build/default/lib/erl_yapb/ebin _build/default/lib/gpb/ebin
%%
-module(simple).
-include_lib("gpb/include/gpb.hrl").
-export([main/1]).
-record(da_record, {a, b}).

decode(Records) ->
    decode(Records, da_record).
decode([H|T], da_record) ->
    erl_yapb:decode(H, da_record),
    decode(T, da_record);
decode([], da_record) ->
    ok.

generate(N) ->
    generate(N, []).
generate(0, List) ->
    List;
generate(N, List) ->
    %io:format("Hello world!~n~p", [generate_random_record() | List]),
    generate(N-1, [generate_random_record() | List]).

generate_random_record() ->
    gpb:encode_msg(record(), defs()).

record() ->
    #da_record{
       a = 69,
       b = 666
      }.

defs() ->
    [
     {{msg, da_record}, [
                         #?gpb_field{name = a, fnum = 1, rnum = #da_record.a, type = int32, occurrence = required, opts = []},
                         #?gpb_field{name = b, fnum = 2, rnum = #da_record.b, type = int32, occurrence = optional, opts = []}
                        ]}
    ].

main([]) ->

    Defs = defs(),
    erl_yapb:add_schema(Defs),

    N = 10000, P = 1,
    %Alternatives = [{decode, fun () -> decode(Bin, da_record) end }],
    Records = generate(100),
    Timing = timing:function(fun () -> decode(Records) end, N, P),
    io:format("~p~n", [Timing]).
