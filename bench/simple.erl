#!/usr/bin/env escript
%%! -pz _build/default/lib/timing/ebin _build/default/lib/timothy/ebin _build/default/lib/bear/ebin _build/default/lib/erl_yapb/ebin _build/default/lib/gpb/ebin
%%
-module(simple).
-include_lib("../include/da_message.hrl").
-include_lib("../include/stvlist.hrl").
-include_lib("gpb/include/gpb.hrl").
-export([main/1]).

decode_yapb(Records) ->
    decode_yapb(Records, da_record).
decode_yapb([H|T], MsgName) ->
    erl_yapb:decode(H, MsgName),
    decode_yapb(T, MsgName);
decode_yapb([], _MsgName) ->
    ok.

decode_gpb(Records, Defs) ->
    decode_gpb(Records, da_record, Defs).
decode_gpb([H|T], MsgName, Defs) ->
    gpb:decode_msg(H, MsgName, Defs),
    decode_gpb(T, MsgName, Defs);
decode_gpb([], _MsgName, _Defs) ->
    ok.

decode_gpb_nif(Records) ->
    decode_gpb_nif(Records, da_record).
decode_gpb_nif([H|T], da_record) ->
    da_message:decode_msg(H, da_record),
    decode_gpb_nif(T, da_record);
decode_gpb_nif([H|T], stvlist) ->
    stvlist:decode_msg(H, stvlist),
    decode_gpb_nif(T, stvlist);
decode_gpb_nif([], _MsgName) ->
    ok.

generate(N, Record, Defs) ->
    generate(N, Record, Defs, []).
generate(0, _Record, _Defs, List) ->
    List;
generate(N, Record, Defs, List) ->
    generate(N-1, Record, Defs, [generate_random_record(Record, Defs) | List]).

generate_random_record(Record, Defs) ->
    gpb:encode_msg(Record(), Defs).

record() ->
    #da_record{
       a = (rand:uniform(65536)-1),
       b = (rand:uniform(65536)-1)
      }.

record2() ->
    #stvlist{
       tvid = "4b21d034f1925638cab3f9a8714cb81f",
       ids = [rand:uniform(65536) - 1 || _ <- lists:seq(1, rand:uniform(3))]
      }.

main([]) ->

    Defs = da_message:get_msg_defs(),
    %Defs2 = stvlist:get_msg_defs(),
    erl_yapb:add_schema(Defs),

    N = 1, P = 1,
    Records = generate(1000000, fun() -> record() end, Defs),
    %Records2 = generate(100, fun() -> record2() end, Defs2),
    da_message:load_nif(),
    Alternatives = [
                    {yapb_decode, fun () -> decode_yapb(Records) end }
                    ],

    %Alternatives2 = [{noop, fun () -> true end },
    %                {stvlist_gpb_decode_nif, fun () -> decode_gpb_nif(Records2, stvlist) end },
    %                {stvlist_gpb_decode, fun () -> decode_gpb(Records2, stvlist, Defs2) end }],
    Timings = [{Name, timing:function(Fn, N, P)} || {Name, Fn} <- Alternatives],
    %Timings2 = [{Name, timing:function(Fn, N, P)} || {Name, Fn} <- Alternatives2],
    io:format("~p~n", ["DA_MESSAGE"]),
    [io:format("~p~n", [{Name, Timing}]) || {Name, Timing} <- Timings],
    io:format("~p~n", ["STVLIST"]),
    %[io:format("~p~n", [{Name, Timing}]) || {Name, Timing} <- Timings2],
    erl_yapb:print_stats().
