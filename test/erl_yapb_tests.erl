-module(erl_yapb_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("gpb/include/gpb.hrl").

-record(da_record, {a, b, c, d, e}).

%basic_test() ->
%    ok = erl_yapb:decode(<<"Derp">>, 1, 1).

new_test() ->
    Defs = [
            {{msg, da_record}, [
                         #?gpb_field{name = a, fnum = 1, rnum = #da_record.a, type = uint32, occurrence = required, opts = []},
                         #?gpb_field{name = b, fnum = 2, rnum = #da_record.b, type = int32, occurrence = optional, opts = []},
                         #?gpb_field{name = c, fnum = 3, rnum = #da_record.c, type = bool, occurrence = required, opts = []},
                         #?gpb_field{name = d, fnum = 4, rnum = #da_record.d, type = float, occurrence = required, opts = []},
                         #?gpb_field{name = e, fnum = 5, rnum = #da_record.e, type = double, occurrence = required, opts = []}
                        ]}
           ],


    Da_record = #da_record{
       a = 666,
       b = 69,
       c = true,
       d = 2.3,
       e = 696969.125
      },

    erl_yapb:add_schema(Defs),

    Bin = gpb:encode_msg(Da_record, Defs),
    Yapb_decode = erl_yapb:decode(Bin, da_record),
    Gpb_decode = gpb:decode_msg(Bin, da_record, Defs),
    erlang:display({yapb, Yapb_decode}),
    erlang:display({gpb, Gpb_decode}),
    Yapb_decode = Gpb_decode.
