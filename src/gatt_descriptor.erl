-module(gatt_descriptor).

-callback init(Path::ebus:object(), Args::any()) -> {ok, State::any()} | {error, term()}.
-callback uuid(State::any()) -> gatt:uuid().
-callback flags(State::any()) -> [flag()].
-callback read_value(State::any()) -> {ok, binary(), State::any()} |
                                      {error, GatError::string(), State::any()}.
-callback write_value(State::any(), binary()) -> {ok, State::any()} |
                                                 {error, GatError::string(), State::any()}.

-optional_callbacks([read_value/1, write_value/2]).

-behavior(gatt_object).
-include("gatt.hrl").
-include_lib("ebus/include/ebus.hrl").

-record(state, {
                bus :: ebus:bus(),
                characteristic_path :: ebus:object_path(),
                path :: ebus:object_path(),
                module :: atom(),
                flags=[] :: [flag()],
                state :: any()
               }).

-type descriptor() :: #state{}.
-type spec() :: {Module::atom(), Index::non_neg_integer(), Args::[any()]}.
-type flag() :: read | write.
-export_type([descriptor/0, spec/0]).

-export([init/1, uuid/1, path/1, properties/1, handle_message/3]).

-spec init(list()) -> {ok, gatt:descriptor()} | {error, term()}.
init([Bus, CharPath, Path, Module, Args]) ->
    case Module:init(Path, Args) of
        {ok, ModuleState} ->
            case ebus:register_object_path(Bus, Path, self()) of
                ok ->
                    {ok, #state{bus=Bus, path=Path, characteristic_path=CharPath,
                                module=Module, state=ModuleState}};
                {error, Error} ->
                    {error, Error}
            end;
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
    #{ ?GATT_DESCRIPTOR_IFACE => mk_properties(State) }.

handle_message(Member=?GATT_DESCRIPTOR("ReadValue"), _Msg,
               State=#state{module=Module, state=ModuleState}) ->
    case erlang:function_exported(Module, read_value, 1) of
        false -> {reply_error, ?GATT_ERROR_NOT_SUPPORTED, Member, State};
        true ->
            case Module:read_value(ModuleState) of
                {ok, Bin, NewModuleState} ->
                    {reply, [{array, byte}], [Bin], State#state{state=NewModuleState}};
                {error, GattError, NewModuleState} ->
                    {reply_error, GattError, Member, State#state{state=NewModuleState}}
            end
    end;
handle_message(Member=?GATT_DESCRIPTOR("WriteValue"), Msg,
               State=#state{module=Module, state=ModuleState}) ->
    case erlang:function_exported(Module, write_value, 2) of
        false -> {reply_error, ?GATT_ERROR_NOT_SUPPORTED, Member, State};
        true ->
            case ebus_message:args(Msg) of
                {ok, [Bin]} when is_binary(Bin) ->
                    case Module:write_value(ModuleState, Bin) of
                        {ok, NewModuleState} ->
                            {reply, [], [], State#state{state=NewModuleState}};
                        {error, GatError, NewModuleState} ->
                            {reply_error, GatError, Member, State#state{state=NewModuleState}}
                    end;
                _ ->
                    {reply_error, ?GATT_ERROR_FAILED, "Bad argument", State}
            end
    end;

handle_message(Member, _Msg, State=#state{}) ->
    lager:warning("Unhandled message ~p", [Member]),
    {reply_error, ?DBUS_ERROR_NOT_SUPPORTED, Member, State}.


%%
%% Internal
%%

-spec mk_properties(#state{}) -> map().
mk_properties(State=#state{}) ->
    #{
      "Characteristic" => characteristic_path(State),
      "UUID" => uuid(State),
      "Flags" => flags_to_strings(flags(State))
     }.

-spec flags_to_strings([flag()]) -> [string()].
flags_to_strings(Flags) ->
    lists:map(fun flag_to_string/1, Flags).

-spec flag_to_string(flag()) -> string().
flag_to_string(read) -> "read";
flag_to_string(write) -> "write".
