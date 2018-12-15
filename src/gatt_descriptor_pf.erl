-module(gatt_descriptor_pf).

-behavior(gatt_descriptor).

-export([uuid/1, init/2, flags/1, read_value/1, format_to_int/1]).

-record(state, {
                format :: format(),
                exponent=0 :: non_neg_integer(),
                unit=0 :: non_neg_integer(),
                name_space=1 :: 0 | 1,
                description=0 :: non_neg_integer()
               }).

-type format() :: boolean |
                  integer_format() |
                  iee_754_float32 | %% <<Val::32/float>>
                  iee_754_float64 | %% <<Val::64/float>>
                  utf8_string |
                  utf16_string |
                  opaque.

-type integer_format() :: uint2 |
                          uint4 |
                          uint12 |
                          uint16 |
                          uint24 |
                          uint32 |
                          uint48 |
                          uint64 |
                          uint128 |
                          sint8 |
                          sint12 |
                          sint16 |
                          sint48 |
                          sint64 |
                          sint128.

uuid(_) ->
    "2904".

flags(_) ->
    [read].

-spec init(Path::string(), [format() | {integer_format(), Exponent::non_neg_integer()}]) -> {ok, #state{}}.
init(_Path, [Format]) when is_atom(Format)->
    {ok, #state{format=Format}};
init(_Path, [{IntFormat, Exponent}]) ->
    {ok, #state{format=IntFormat, exponent=Exponent}}.

read_value(State=#state{format=Format, exponent=Exponent, unit=Unit,
                        name_space=NameSpace, description=Description}) ->
    {ok,
     <<(format_to_int(Format)):8/unsigned-integer,
       Exponent:8/signed-integer,
       Unit:16/unsigned-integer,
       NameSpace:8/unsigned-integer,
       Description:16/unsigned-integer>>,
     State}.


format_to_int(boolean) ->
    1;
format_to_int(uint2) ->
    2;
format_to_int(uint4) ->
    3;
format_to_int(uint8) ->
    4;
format_to_int(uint12) ->
    5;
format_to_int(uint16) ->
    6;
format_to_int(uint24) ->
    7;
format_to_int(uint32) ->
    8;
format_to_int(uint48) ->
    9;
format_to_int(uint64) ->
    10;
format_to_int(uint128) ->
    11;
format_to_int(sint8) ->
    12;
format_to_int(sint12) ->
    13;
format_to_int(sint16) ->
    14;
format_to_int(sint24) ->
    15;
format_to_int(sint32) ->
    16;
format_to_int(sint48) ->
    17;
format_to_int(sint64) ->
    18;
format_to_int(sint128) ->
    19;
format_to_int(iee_754_float32) -> %% <<Val::32/float>>
    20;
format_to_int(iee_754_float64) -> %% <<Val::64/float>>
    21;
format_to_int(utf8_string) ->
    25;
format_to_int(utf16_string) ->
    26;
format_to_int(opaque) ->
    27;
format_to_int(Type) ->
    erlang:error({unsupported_type, Type}).
