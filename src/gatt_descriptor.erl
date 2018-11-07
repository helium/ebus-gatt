-module(gatt_descriptor).

-callback init(Args::any()) -> {ok, State::any()} | {error, term()}.
-callback uuid(State::any()) -> gatt:uuid().
-callback flags(State::any()) -> [flag()].

-behavior(gatt_object).
-include("gatt.hrl").

-record(state, {
                characteristic_path :: ebus:object_path(),
                path :: ebus:object_path(),
                module :: atom(),
                flags=[] :: [flag()],
                state :: any()
               }).

-type descriptor() :: #state{}.
-type spec() :: {Module::atom(), Index::non_neg_integer()}.
-type flag() :: read | write.
-export_type([descriptor/0, spec/0]).

-export([init/1, uuid/1, path/1, properties/1, handle_message/3]).

-spec init(list()) -> {ok, gatt:descriptor()} | {error, term()}.
init([CharPath, Path, Module, Args]) ->
    case Module:init(Args) of
        {ok, ModuleState} ->
            {ok, #state{path=Path, characteristic_path=CharPath,
                        module=Module, state=ModuleState}};
        {error, Error} ->
            {error, Error}
    end.

uuid(#state{module=Module, state=ModuleState}) ->
    Module:uuid(ModuleState).

path(#state{path=Path}) ->
    Path.

characteristic_path(#state{characteristic_path=CharPath}) ->
    CharPath.

flags(#state{module=Module, state=ModuleState}) ->
    Module:flags(ModuleState).

properties(State=#state{}) ->
    #{ ?GATT_DESCRIPTOR_IFACE =>
           #{
             "Characteristic" => characteristic_path(State),
             "UUID" => uuid(State),
             "Flags" => flags_to_strings(flags(State))
            }
     }.

handle_message(Member, Msg, State=#state{module=Module, state=ModuleState}) ->
    case Module:handle_message(Member, Msg, ModuleState) of
        {noreply, NewModuleState} ->
            {noreply, State#state{state=NewModuleState}};
        {reply, Types, Args, NewModuleState} ->
            {reply, Types, Args, State#state{state=NewModuleState}};
        {reply_error, ErrorName, ErrorMsg, NewModuleState} ->
            {reply_error, ErrorName, ErrorMsg, State#state{state=NewModuleState}};
        {stop, Reason, NewModuleState} ->
            {stop, Reason, State#state{state=NewModuleState}}
    end.

-spec flags_to_strings([flag()]) -> [string()].
flags_to_strings(Flags) ->
    lists:map(fun flag_to_string/1, Flags).

-spec flag_to_string(flag()) -> string().
flag_to_string(read) -> "read";
flag_to_string(write) -> "write".
