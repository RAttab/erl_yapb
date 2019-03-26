-module(erl_yapb_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("gpb/include/gpb.hrl").

-record(m4, {a, b, c, d}).

%basic_test() ->
%    ok = erl_yapb:decode(<<"Derp">>, 1, 1).

new_test() ->
    Defs = [
            {{msg, m4}, [
                         #?gpb_field{name = a, fnum = 1, rnum = #m4.a, type = int32, occurrence = required, opts = []},
                         #?gpb_field{name = b, fnum = 2, rnum = #m4.b, type = int32, occurrence = optional, opts = []},
                         #?gpb_field{name = c, fnum = 3, rnum = #m4.c, type = int32, occurrence = repeated, opts = []},
                         #?gpb_field{name = d, fnum = 4, rnum = #m4.d, type = int32, occurrence = repeated, opts = [packed]}
                        ]}
           ],


    M4 = #m4{
       a = 69,
       b = 69,
       c = [67, 68, 69],
       d = [67, 68, 69]
      },

    Schema = erl_yapb:add_schema(Defs),

    Bin = gpb:encode_msg(M4, Defs),
    %?debugVal(Bin),
    Gpb = gpb:decode_msg(Bin, m4, Defs),
    %?debugVal(Gpb),
    erl_yapb:decode(Bin, m4, Defs) =:= gpb:decode_msg(Bin, m4, Defs).
