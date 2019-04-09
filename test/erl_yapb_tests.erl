-module(erl_yapb_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("gpb/include/gpb.hrl").

-record(da_record, {a, b, c, d}).

%basic_test() ->
%    ok = erl_yapb:decode(<<"Derp">>, 1, 1).

new_test() ->
    Defs = [
            {{msg, da_record}, [
                         #?gpb_field{name = a, fnum = 1, rnum = #da_record.a, type = int32, occurrence = required, opts = []},
                         #?gpb_field{name = b, fnum = 2, rnum = #da_record.b, type = int32, occurrence = optional, opts = []}
                         %#?gpb_field{name = c, fnum = 3, rnum = #m4.c, type = int32, occurrence = repeated, opts = []},
                         %#?gpb_field{name = d, fnum = 4, rnum = #m4.d, type = int32, occurrence = repeated, opts = [packed]}
                        ]}
           ],


    M4 = #da_record{
       a = 666,
       b = 69
       %c = [67, 68, 69],
       %d = [67, 68, 69]
      },

    Schema = erl_yapb:add_schema(Defs),

    Bin = gpb:encode_msg(M4, Defs),
    %?debugVal(Bin),
    Gpb = gpb:decode_msg(Bin, da_record, Defs),
    %?debugVal(Gpb),
    Yapb_decode = erl_yapb:decode(Bin, da_record, Defs),
    Gpb_decode = gpb:decode_msg(Bin, da_record, Defs),
    erlang:display({yapb, Yapb_decode}),
    erlang:display({gpb, Gpb_decode}),
    Yapb_decode = Gpb_decode.
