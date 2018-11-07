-module(gatt_characteristic).

-callback init(ebus:object_path(), Args::any()) -> {ok, [gatt:descriptor_spec()], State::any()} |
                                                   {error, term()}.
-callback uuid(State::any()) -> gatt:uuid().
-callback flags(State::any()) -> [flag()].
-callback read_value(State::any()) -> {ok, binary(), State::any()} |
                                      {error, GatError::string(), State::any()}.
-callback write_value(State::any(), binary()) -> {ok, State::any()} |
                                                 {error, GatError::string(), State::any()}.
-callback start_notify(State::any()) -> {ok, State::any()} | {error, GatError::string(), State::any()}.
-callback stop_notify(State::any()) -> {ok, State::any()} | {error, GatError::string(), State::any()}.

-optional_callbacks([read_value/1, write_value/2,
                     start_notify/1, stop_notify/1]).

-behavior(gatt_object).
-include("gatt.hrl").
-include_lib("ebus/include/ebus.hrl").

-record(state, {
                service_path :: ebus:object_path(),
                path :: ebus:object_path(),
                descriptors=#{} ::  #{ebus:object_path() => gatt:descriptor()},
                module :: atom(),
                state :: any()
               }).

-type characteristic() :: #state{}.
-type spec() :: {Module::atom(), Index::non_neg_integer()}.
-type flag() :: read | write | notify.
-export_type([characteristic/0, spec/0, flag/0]).

-export([init/1, uuid/1, path/1, properties/1, flags/1, handle_message/3,
         add_descriptor/3, fold_descriptors/3,
         value_changed/2, value_invalidated/1,
         properties_changed/3]).

-spec init(list()) -> {ok, gatt:characteristic()} | {error, term()}.
init([ServicePath, Path, Module, Args]) ->
    case Module:init(Path, Args) of
        {ok, Descriptors, ModuleState} ->
            State = #state{service_path=ServicePath, path=Path, module=Module, state=ModuleState},
            NewState = lists:foldl(fun({DescMod, DescIndex}, AccState) ->
                                           case add_descriptor(AccState, DescMod, DescIndex) of
                                               {ok, _, AccState1} -> AccState1;
                                               {error, Error} -> {error, Error}
                                           end;
                                      (_, {error, Error}) ->
                                           {error, Error}
                                   end, State, Descriptors),
            {ok, NewState};
        {error, Error} ->
            {error, Error}
    end.

uuid(#state{module=Module, state=ModuleState}) ->
    Module:uuid(ModuleState).

path(#state{path=Path}) ->
    Path.

service_path(#state{service_path=ServicePath}) ->
    ServicePath.

flags(#state{module=Module, state=ModuleState}) ->
    Module:flags(ModuleState).

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


-spec add_descriptor(characteristic(), atom(), non_neg_integer())
                    -> {ok, ebus:object_path(), characteristic()} | {error, term()}.
add_descriptor(State=#state{}, Module, Index) ->
    DescKey = "/desc" ++ erlang:integer_to_list(Index),
    DescPath = State#state.path ++ DescKey,
    case gatt_descriptor:init([State#state.path, DescPath, Module, []]) of
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

handle_message(Member=?GATT_CHARACTERISTIC("ReadValue"), _Msg,
               State=#state{module=Module, state=ModuleState}) ->
    case erlang:function_exported(Module, read_value, 1) of
        false -> {reply_error, ?GATT_ERROR_NOT_SUPPORTED, Member, State};
        true ->
            case Module:read_value(ModuleState) of
                {ok, Bin, NewModuleState} ->
                    {reply, [binary], [Bin], State#state{state=NewModuleState}};
                {error, GattError, NewModuleState} ->
                    {reply_error, GattError, Member, State#state{state=NewModuleState}}
            end
    end;
handle_message(Member=?GATT_CHARACTERISTIC("WriteValue"), Msg,
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
handle_message(Member=?GATT_CHARACTERISTIC("StartNotify"), _Msg,
               State=#state{module=Module, state=ModuleState}) ->
    case erlang:function_exported(Module, start_notify, 1) of
        false -> {reply_error, ?GATT_ERROR_NOT_SUPPORTED, Member, State};
        true ->
            case Module:start_notify(ModuleState) of
                {ok, NewModuleState} ->
                    {reply, [], [], State#state{state=NewModuleState}};
                {error, GatError, NewModuleState} ->
                    {reply_error, GatError, Member, State#state{state=NewModuleState}}
            end
    end;
handle_message(Member=?GATT_CHARACTERISTIC("StopNotify"), _Msg,
               State=#state{module=Module, state=ModuleState}) ->
    case erlang:function_exported(Module, stop_notify, 1) of
        false -> {reply_error, ?GATT_ERROR_NOT_SUPPORTED, Member, State};
        true ->
            case Module:stop_notify(ModuleState) of
                {ok, NewModuleState} ->
                    {reply, [], [], State#state{state=NewModuleState}};
                {error, GatError, NewModuleState} ->
                    {reply_error, GatError, Member, State#state{state=NewModuleState}}
            end
    end;

handle_message(Member, _Msg, State=#state{}) ->
    lager:warning("Unhandled message ~p", [Member]),
    {reply_error, ?DBUS_ERROR_NOT_SUPPORTED, Member, State}.

%%
%% Internal
%%

mk_properties(State=#state{}) ->
    #{
      "Service" =>  service_path(State),
      "UUID" => uuid(State),
      "Flags" => flags_to_strings(flags(State)),
      "Descriptors" => [gatt_descriptor:path(D) || D <- maps:values(State#state.descriptors)]
     }.

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
