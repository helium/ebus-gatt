-module(ble_agent).

-behavior(ebus_object).

-callback init(Args::[any()]) -> ebus_object:init_result().
-callback capability(State::any()) -> capability().
-callback release(State::any()) -> any().
-callback request_authorization(State::any(), Device::ebus:object_path()) -> authorization_result().
-callback authorize_service(State::any(), Device::ebus:object_path(), Service::ebus:uuid()) -> authorization_result().

-optional_callbacks([release/1, request_authorization/2, authorize_service/3]).

-type authorization_result() :: accept |
                                reject |
                                cancel.
-type capability() :: display_only |
                      display_yes_no |
                      keyboard_only |
                      no_input_no_output |
                      keyboard_display.

-include("gatt.hrl").
-include_lib("ebus/include/ebus.hrl").

-define(BLE_AGENT_MANAGER_PATH, "/org/bluez").
-define(BLE_AGENT_MANAGER_INTERFACE, "org.bluez.AgentManager1").
-define(BLE_AGENT_MANAGER(M), ?BLE_AGENT_MANAGER_INTERFACE ++ "." ++ M).

-define(BLE_AGENT_INTERFACE, "org.bluez.Agent1").
-define(BLE_AGENT(M), ?BLE_AGENT_INTERFACE ++ "." ++ M).

-define(BLE_AGENT_ERROR_REJECTED, "org.bluez.Error.Rejected").
-define(BLE_AGENT_ERROR_CANCELED, "org.bluez.Error.Canceled").

%% ebus_object
-export([start_link/4, init/1, terminate/2, handle_message/3]).
%% API
-export([stop/2]).

-record(state, {
                proxy :: ebus:proxy(),
                agent_name :: string(),
                module :: atom(),
                state :: any()
               }).

stop(Pid, Reason) ->
    gen_server:stop(Pid, Reason, 5000).

start_link(Bus, BasePath, Module, Args) ->
    AgentName = BasePath ++ "ble_agent" ++ integer_to_list(erlang:system_time(millisecond)),
    ebus_object:start_link(Bus, AgentName, ?MODULE, [Bus, AgentName, Module, Args], []).

init([Bus, AgentName, Module, Args]) ->
    case Module:init(Args) of
        {ok, MState} ->
            Capability = Module:capability(MState),
            {ok, Proxy} = ebus_proxy:start_link(Bus, ?BLUEZ_SERVICE, []),
            {ok, _} = ebus_proxy:call(Proxy, ?BLE_AGENT_MANAGER_PATH, ?BLE_AGENT_MANAGER("RegisterAgent"),
                                      [object_path, string], [AgentName, capability_to_string(Capability)]),
            {ok, #state{proxy=Proxy, agent_name=AgentName, module=Module, state=MState}};
        Other -> Other
    end.


handle_message(?BLE_AGENT("Release"), _Msg, State=#state{module=Module, state=ModuleState}) ->
    case erlang:function_exported(Module, release, 1) of
        true ->
            Module:release(ModuleState),
            {noreply, State};
        false ->
            {noreply, State}
    end;
handle_message(?BLE_AGENT("Cancel"), _Msg,
               State=#state{module=Module, state=ModuleState}) ->
    lager:info("Authorization request canceled"),
    case erlang:function_exported(Module, cancel, 1) of
        true ->
            Module:cancel(ModuleState),
            {noreply, State};
        false ->
            {noreply, State}
    end;
handle_message(?BLE_AGENT("RequestAuthorization")=Member, Msg,
               State=#state{module=Module, state=ModuleState}) ->
    lager:debug("Received Device Authorization Request"),
    case erlang:function_exported(Module, request_authorization, 2) of
        false -> {reply_error, ?DBUS_ERROR_NOT_SUPPORTED, Member, State};
        true ->
            case ebus_message:args(Msg) of
                {ok, [DevicePath]} ->
                    lager:info("Requesting authorization for device: ~p", [DevicePath]),
                    handle_authorization_result(Module:request_authorization(ModuleState, DevicePath),
                                                State);
                {error, Error} ->
                    lager:error("Failed to decode authorization args: ~p", [Error])
            end
    end;
handle_message(?BLE_AGENT("AuthorizeService")=Member, Msg,
               State=#state{module=Module, state=ModuleState}) ->
    lager:debug("Received Device/Service Authorization Request"),
    case erlang:function_exported(Module, authorize_service, 3) of
        false -> {reply_error, ?DBUS_ERROR_NOT_SUPPORTED, Member, State};
        true ->
            case ebus_message:args(Msg) of
                {ok, [DevicePath, Service]} ->
                    lager:info("Requesting authorization for device: ~p service: ~p", [DevicePath, Service]),
                    handle_authorization_result(Module:authorize_service(ModuleState, DevicePath, Service),
                                                State);
                {error, Error} ->
                    lager:error("Failed to decode authorization args: ~p", [Error])
            end
    end;

handle_message(Member, _Msg, State=#state{}) ->
    lager:warning("Unhandled message ~p", [Member]),
    {noreply, State}.

terminate(_, State=#state{}) ->
    ebus_proxy:call(State#state.proxy, ?BLE_AGENT_MANAGER_PATH, ?BLE_AGENT_MANAGER("UnregisterAgent"),
                    [object_path], [State#state.agent_name]).


%%
%% Internal
%%

-spec handle_authorization_result(authorization_result(), #state{}) -> ebus_object:handle_message_result().
handle_authorization_result(accept, State) ->
    {reply, [], [], State};
handle_authorization_result(reject, State) ->
    {reply_error, ?BLE_AGENT_ERROR_REJECTED, "", State};
handle_authorization_result(canceled, State) ->
    {reply_error, ?BLE_AGENT_ERROR_CANCELED, "", State}.

-spec capability_to_string(capability()) -> string().
capability_to_string(display_only) ->
    "DisplayOnly";
capability_to_string(display_yes_no) ->
    "DisplayYesNo";
capability_to_string(keyboard_only) ->
    "KeyboardOnly";
capability_to_string(no_input_no_output) ->
    "NoInputNoOutput";
capability_to_string(keyboard_display) ->
    "KeyboardDisplay".
