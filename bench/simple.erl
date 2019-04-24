#!/usr/bin/env escript
%%! -pz _build/default/lib/timing/ebin _build/default/lib/timothy/ebin _build/default/lib/bear/ebin _build/default/lib/erl_yapb/ebin _build/default/lib/gpb/ebin
%%
-module(simple).
-include_lib("../include/da_message.hrl").
-include_lib("gpb/include/gpb.hrl").
-export([main/1]).
%-record(da_record, {a, b}).

decode_yapb(Records) ->
    decode_yapb(Records, da_record).
decode_yapb([H|T], da_record) ->
    erl_yapb:decode(H, da_record),
    decode_yapb(T, da_record);
decode_yapb([], da_record) ->
    ok.

decode_gpb(Records, Defs) ->
    decode_gpb(Records, da_record, Defs).
decode_gpb([H|T], da_record, Defs) ->
    da_message:decode_msg(H, da_record),
    decode_gpb(T, da_record, Defs);
decode_gpb([], da_record, _Defs) ->
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
       a = (rand:uniform(65536)-1),
       b = (rand:uniform(65536)-1)
      }.

defs() ->
    [
     {{msg, da_record}, [
                         #?gpb_field{name = a, fnum = 1, rnum = #da_record.a, type = uint32, occurrence = required, opts = []},
                         #?gpb_field{name = b, fnum = 2, rnum = #da_record.b, type = uint32, occurrence = optional, opts = []}
                        ]}
    ].

main([]) ->

    Defs = defs(),
    erl_yapb:add_schema(Defs),

    da_message:get_msg_defs(),
    N = 10000, P = 1,
    Records = generate(100),
    %da_message:load_nif(),
    Alternatives = [{yapb_decode, fun () -> decode_yapb(Records) end },
                    {gpb_decode, fun () -> decode_gpb(Records, Defs) end }],
    Timings = [{Name, timing:function(Fn, N, P)} || {Name, Fn} <- Alternatives],
    [io:format("~p~n", [{Name, Timing}]) || {Name, Timing} <- Timings].
