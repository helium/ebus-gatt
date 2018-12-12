-module(gatt_characteristic_string).

-behavior(gatt_characteristic).

-export([init/2, uuid/1, flags/1, read_value/1]).

-record(state, {
                value :: binary(),
                uuid :: unicode:unicode_binary()
               }).

init(_Path, Opts) ->
    Required = fun(K) ->
                       case lists:keyfind(K, 1, Opts) of
                           false -> error({required_value, K});
                           {_, V} -> V
                       end
               end,
    %% TODO: Add CUD descriptor for name in opts?
    {ok, [], #state{uuid=Required(uuid),
                    value=unicode:characters_to_binary(Required(value))}}.

uuid(#state{uuid=UUID}) ->
    UUID.

flags(#state{}) ->
    [read].

read_value(State=#state{value=Value}) ->
    {ok, Value, State}.
