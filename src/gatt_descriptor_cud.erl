-module(gatt_descriptor_cud).

-behavior(gatt_descriptor).

-export([uuid/1, init/2, flags/1, read_value/1]).

-record(state, {
                value :: string()
               }).

uuid(_) ->
    "2901".

flags(_) ->
    [read].

init(_Path, [Description]) ->
    {ok, #state{value=Description}}.

read_value(State=#state{value=Description}) ->
    {ok, list_to_binary(Description), State}.
