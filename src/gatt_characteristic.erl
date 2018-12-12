-module(gatt_characteristic).

-callback init(ebus:object_path(), Args::any()) -> {ok, [gatt:descriptor_spec()], State::any()} |
                                                   {error, term()}.
-callback uuid(State::any()) -> gatt:uuid().
-callback flags(State::any()) -> [flag()].
-callback read_value(State::any()) -> {ok, binary(), State::any()} |
                                      {error, GatError::string(), State::any()}.
-callback write_value(State::any(), binary()) -> {ok, State::any()} |
                                                 {error, GatError::string(), State::any()}.
-callback start_notify(State::any()) -> {ok, State::any()} |
                                        {error, GatError::string(),
                                         State::any()}.
-callback stop_notify(State::any()) -> {ok, State::any()} |
                                       {error, GatError::string(),
                                        State::any()}.
-callback handle_signal(SignalID::ebus:filter_id(), Msg::ebus:messgage(),
                        State::any()) -> ebus_object:handle_info_result().

-callback handle_info(Msg::term(), State::any()) -> ebus_object:handle_info_result().

-optional_callbacks([read_value/1, write_value/2,
                     start_notify/1, stop_notify/1,
                     handle_signal/3, handle_info/2]).

-behavior(gatt_object).
-include("gatt.hrl").
-include_lib("ebus/include/ebus.hrl").

-record(state, {
                bus :: ebus:bus(),
                service_path :: ebus:object_path(),
                path :: ebus:object_path(),
                descriptors=#{} ::  #{ebus:object_path() => gatt:descriptor()},
                module :: atom(),
                state :: any()
               }).

-type characteristic() :: #state{}.
-type spec() :: {Module::atom(), Index::non_neg_integer()} |
                {Module::atom(), Index::non_neg_integer(), Args::any()}.
-type flag() :: read | write | notify.
-export_type([characteristic/0, spec/0, flag/0]).

-export([init/1, uuid/1, path/1, properties/1, flags/1, handle_message/3,
         add_descriptor/2, fold_descriptors/3,
         value_changed/2, value_invalidated/1,
         properties_changed/3,
         handle_signal/3, handle_info/2]).

-spec init(list()) -> {ok, gatt:characteristic()} | {error, term()}.
init([Bus, ServicePath, Path, Module, Args]) ->
    case Module:init(Path, Args) of
        {ok, DescSpecs, ModuleState} ->
            State = #state{bus=Bus, service_path=ServicePath, path=Path, module=Module, state=ModuleState},
            case lists:foldl(fun(_, {error, Reason}) -> {error, Reason};
                                (DescSpec, AccState) ->
                                     case add_descriptor(AccState, DescSpec) of
                                         {ok, _, AccState1} -> AccState1;
                                         {error, Reason} -> {error, Reason}
                                     end
                             end, State, DescSpecs) of
                {error, Error} -> {error, Error};
                NewState ->
                    case ebus:register_object_path(Bus, Path, self()) of
                        ok ->
                            {ok, NewState};
                        {error, Error} -> {error, Error}
                    end
            end;
        {error, Error} ->
            {error, Error}
    end.

uuid(State=#state{module=Module}) ->
    Module:uuid(State#state.state).

path(#state{path=Path}) ->
    Path.

service_path(#state{service_path=ServicePath}) ->
    ServicePath.

flags(State=#state{module=Module}) ->
    Module:flags(State#state.state).

properties(State=#state{}) ->
    #{?GATT_CHARACTERISTIC_IFACE => mk_properties(State)}.

-spec value_changed(ebus:object_path(), NewValue::binary()) -> ok.
value_changed(Path, NewValue) ->
    properties_changed(Path, #{"Value" => NewValue}, []).

-spec value_invalidated(ebus:object_path()) -> ok.
value_invalidated(Path) ->
    properties_changed(Path, #{}, ["Value"]).

-spec properties_changed(ebus:object_path(), #{string() => any()}, [string()]) -> ok.
properties_changed(Path, Changed, Invalidated) ->
    self() ! {properties_changed, Path, ?GATT_CHARACTERISTIC_IFACE, Changed, Invalidated},
    ok.


-spec add_descriptor(characteristic(), gatt_descriptor:spec())
                    -> {ok, ebus:object_path(), characteristic()} | {error, term()}.
add_descriptor(State=#state{}, {Module, Index, Args}) ->
    DescKey = "/desc" ++ erlang:integer_to_list(Index),
    DescPath = State#state.path ++ DescKey,
    case gatt_descriptor:init([State#state.bus, State#state.path, DescPath, Module, Args]) of
        {ok, Descriptor} ->
            {ok, DescPath, update_descriptor(DescKey, Descriptor, State)};
        {error, Error} ->
            {error, Error}
    end.

-spec fold_descriptors(characteristic(),
                       fun((ebus:object_path(), gatt:descriptor(), AccIn::any()) -> AccOut::any()),
                       Acc0::any()) -> Acc1::any().
fold_descriptors(State=#state{}, Fun, Acc) ->
    maps:fold(Fun, Acc, State#state.descriptors).


-spec handle_message(Member::string(), Msg::ebus:message(), #state{})
                    -> ebus_object:handle_message_result().
handle_message(Member, Msg, State=#state{}) ->
    case find_descriptor(ebus_message:path(Msg), State) of
        {error, no_path} ->
            {reply_error, ?GATT_ERROR_FAILED, Member, State};
        {error, char_path} ->
            %% Handle characteristic message
            handle_message_characteristic(Member, Msg, State);
        {ok, DescKey, Descriptor} ->
            handle_message_descriptor(DescKey, Descriptor, Member, Msg, State)
    end.


handle_signal(SignalID, Msg, State=#state{module=Module, state=ModuleState}) ->
    case erlang:function_exported(Module, handle_signal, 3) of
        false -> ok;
        true ->
            Result = Module:handle_signal(SignalID, Msg, ModuleState),
            handle_info_result_characteristic(Result, State)
    end.

handle_info(Msg, State=#state{module=Module, state=ModuleState}) ->
    case erlang:function_exported(Module, handle_info, 2) of
        false -> ok;
        true ->
            Result = Module:handle_info(Msg, ModuleState),
            handle_info_result_characteristic(Result, State)
    end.


%%
%% Internal
%%

handle_info_result_characteristic(Result, State=#state{}) ->
    case Result of
        {noreply, NewModuleState} ->
            {noreply, State#state{state=NewModuleState}};
        {noreply, NewModuleState, Action} ->
            {noreply, State#state{state=NewModuleState}, Action};
        {stop, Reason, NewModuleState} ->
            {stop, Reason, State#state{state=NewModuleState}}
    end.

handle_message_characteristic(Member=?GATT_CHARACTERISTIC("ReadValue"), _Msg,
               State=#state{module=Module, state=ModuleState}) ->
    case erlang:function_exported(Module, read_value, 1) of
        false ->
            {reply_error, ?GATT_ERROR_NOT_SUPPORTED, Member, State};
        true ->
            case Module:read_value(ModuleState) of
                {ok, Bin, NewModuleState} ->
                    {reply, [{array, byte}], [Bin], State#state{state=NewModuleState}};
                {error, GattError, NewModuleState} ->
                    lager:error("ERROR ~p", [GattError]),
                    {reply_error, GattError, Member, State#state{state=NewModuleState}}
            end
    end;
handle_message_characteristic(Member=?GATT_CHARACTERISTIC("WriteValue"), Msg,
               State=#state{module=Module, state=ModuleState}) ->
    case erlang:function_exported(Module, write_value, 2) of
        false -> {reply_error, ?GATT_ERROR_NOT_SUPPORTED, Member, State};
        true ->
            case ebus_message:args(Msg) of
                {ok, [Bin, _]} when is_binary(Bin) ->
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
handle_message_characteristic(Member=?GATT_CHARACTERISTIC("StartNotify"), _Msg,
               State=#state{module=Module, state=ModuleState}) ->
    case erlang:function_exported(Module, start_notify, 1) of
        false -> {reply_error, ?GATT_ERROR_NOT_SUPPORTED, Member, State};
        true ->
            case Module:start_notify(ModuleState) of
                {ok, NewModuleState} ->
                    {noreply, State#state{state=NewModuleState}};
                {error, GatError, NewModuleState} ->
                    {reply_error, GatError, Member, State#state{state=NewModuleState}}
            end
    end;
handle_message_characteristic(Member=?GATT_CHARACTERISTIC("StopNotify"), _Msg,
               State=#state{module=Module, state=ModuleState}) ->
    case erlang:function_exported(Module, stop_notify, 1) of
        false -> {reply_error, ?GATT_ERROR_NOT_SUPPORTED, Member, State};
        true ->
            case Module:stop_notify(ModuleState) of
                {ok, NewModuleState} ->
                    {noreply, State#state{state=NewModuleState}};
                {error, GatError, NewModuleState} ->
                    {reply_error, GatError, Member, State#state{state=NewModuleState}}
            end
    end;
handle_message_characteristic(Member, _Msg, State=#state{}) ->
    lager:warning("Unhandled characteristic message ~p", [Member]),
    {reply_error, ?GATT_ERROR_NOT_SUPPORTED, Member, State}.


handle_message_descriptor(DescKey, Descriptor, Member, Msg, State=#state{}) ->
    case gatt_descriptor:handle_message(Member, Msg, Descriptor) of
        {reply, Types, Args, NewDescriptor} ->
            {reply, Types, Args,
             update_descriptor(DescKey,NewDescriptor, State)};
        {reply_error, ErrorName, ErrorMsg, NewDescriptor} ->
            {reply_error, ErrorName, ErrorMsg,
             update_descriptor(DescKey, NewDescriptor, State)}
    end.

mk_properties(State=#state{}) ->
    #{
      "Service" =>  service_path(State),
      "UUID" => uuid(State),
      "Flags" => flags_to_strings(flags(State)),
      "Descriptors" => [gatt_descriptor:path(D) || D <- maps:values(State#state.descriptors)]
     }.

-spec find_descriptor(ebus:object_path(), #state{}) -> {error, no_path} |
                                                       {error, char_path} |
                                                       {error, {no_descriptor, DescKey::string}} |
                                                       {ok, DesckKey::string(), gatt:descriptor()}.
find_descriptor(undefined, _State=#state{}) ->
    {error, no_path};
find_descriptor(DescPath, State=#state{path=CharPath}) ->
    case string:prefix(DescPath, CharPath) of
        [] -> {error, char_path};
        DescKey ->
            case maps:get(DescKey, State#state.descriptors, false) of
                false -> {error, {no_descriptor, DescKey}};
                Descriptor -> {ok, DescKey, Descriptor}
            end
    end.

-spec update_descriptor(string(), gatt:descriptor(), #state{}) -> #state{}.
update_descriptor(DescKey, Descriptor, State=#state{}) ->
    NewDescs = maps:put(DescKey, Descriptor, State#state.descriptors),
    State#state{descriptors=NewDescs}.

flags_to_strings(Flags) ->
    lists:map(fun flag_to_string/1, Flags).

-spec flag_to_string(flag()) -> string().
flag_to_string(read) -> "read";
flag_to_string(write) -> "write";
flag_to_string(notify) -> "notify".
