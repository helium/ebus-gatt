%% @doc Represents a GATT application as described by a "Profile" in
%% [https://www.bluetooth.com/specifications/gatt/generic-attributes-overview]

-module(gatt_application).

-include_lib("ebus/include/ebus.hrl").
-include("gatt.hrl").

-behavior(ebus_object).

-callback bus() -> ebus:bus().
-callback path() -> ebus:object_path().
-callback adapter_path() -> ebus:object_path().
-callback init(Args::any()) -> {ok, [gatt:service_spec()], State::any()} | {error, term()}.

-export([start_link/3, init/1, terminate/2,
         handle_message/3, handle_info/2]).

-record(state, {
                application_sup :: pid(),
                path :: ebus:object_path(),
                adapter_path :: string(),
                module :: atom(),
                state :: any(),
                proxy :: ebus:proxy(),
                bus :: ebus:bus()
               }).

start_link(Sup, Module, Args) ->
    case Module:bus() of
        {ok, Bus} ->
            Path = Module:path(),
            ebus_object:start_link(Bus, Path, ?MODULE, [Sup, Bus, Module, Args], []);
        {error, Error} ->
            {error, Error}
    end.

init([Sup, Bus, Module, Args]) ->
    AdapterPath = Module:adapter_path(),
    Path = Module:path(),
    {ok, Proxy} = ebus_proxy:start_link(Bus, ?BLUEZ_SERVICE, []),
    case has_gatt_manager(Proxy, AdapterPath) of
        {error, Error} ->
            ebus_proxy:stop(Proxy),
            {stop, {error, Error}};
        ok ->
            case Module:init(Args) of
                {ok, ServiceSpecs, ModuleState} ->
                    self() ! {register_application, ServiceSpecs},
                    {ok, #state{application_sup=Sup,
                                proxy=Proxy, bus=Bus,
                                adapter_path=AdapterPath, path=Path,
                                module=Module, state=ModuleState}};
                {error, Error} ->
                    {stop, {error, Error}}
            end
    end.

handle_message(?DBUS_OBJECT_MANAGER("GetManagedObjects"), _Msg, State=#state{}) ->
    lager:info("Managed Objects"),
    DescFn = fun(_, Desc, Acc) ->
                     maps:put(gatt_descriptor:path(Desc),
                              gatt_descriptor:properties(Desc),
                              Acc)
             end,
    CharFn = fun(_, Char, Acc) ->
                     Map1 = maps:put(gatt_characteristic:path(Char),
                                     gatt_characteristic:properties(Char),
                                     Acc),
                     gatt_characteristic:fold_descriptors(Char, DescFn, Map1)
             end,
    ServiceFn = fun(Service, Acc) ->
                        Map1 = maps:put(gatt_service:path(Service),
                                        gatt_service:properties(Service),
                                        Acc),
                        gatt_service:fold_characteristics(Service, CharFn, Map1)
                end,

    Result = fold_services(State, ServiceFn, #{}),
    {reply, [{dict, object_path, {dict, string, {dict, string, variant}}}],
     [Result], State};

handle_message(Member, _Msg, State=#state{}) ->
    lager:warning("Unhandled message ~p", [Member]),
    {noreply, State}.


handle_info({register_application, ServiceSpecs}, State=#state{}) ->
    %% Spawn of the actual RegisterApplication call since we'll get
    %% called back for our managed objects.
    NewState = start_services(ServiceSpecs, State),
    Parent = self(),
    spawn_link(fun() ->
                       Result = call_manager(State, "RegisterApplication",
                                             [object_path, {dict, string, variant}],
                                             [NewState#state.path, #{}]),
                       Parent ! {register_application_result, Result}
               end),
    {noreply, NewState};
handle_info({register_application_result, Result}, State=#state{}) ->
    case Result of
        {ok, _} -> {noreply, State};
        {error, Error} -> {stop, {error, Error}, State}
    end;

handle_info(Msg, State) ->
    lager:warning("Unhandled info message ~p", [Msg]),
    {noreply, State}.

terminate(_Reason, State=#state{}) ->
    send_manager(State, "UnregisterApplication", [object_path], [State#state.path]).

%%
%% Private
%%

-spec start_services([gatt:service_spec()], #state{}) -> #state{}.
start_services(ServiceSpecs, State=#state{}) ->
    %% TODO: Move this to gatt_appliation_sup?
    ServiceSup = gatt_application_sup:service_sup(State#state.application_sup),
    lists:foreach(fun(ServiceSpec) ->
                          gatt_service_sup:start_service(ServiceSup, State#state.bus,
                                                         State#state.path, ServiceSpec)
                  end, ServiceSpecs),
    State.

-spec services(#state{}) -> [gatt:service()].
services(State=#state{}) ->
    %% TODO: Move this to gat_appliation_sup?
    ServiceSup = gatt_application_sup:service_sup(State#state.application_sup),
    gatt_service_sup:services(ServiceSup).

-spec fold_services(#state{}, fun((pid(), AccIn::any()) -> AccOut::any()), Acc0::any())
                   -> Acc1::any().
fold_services(State=#state{}, Fun, Acc) ->
    lists:foldl(Fun, Acc, services(State)).

call_manager(State, Member, Types, Args) ->
    ebus_proxy:call(State#state.proxy, State#state.adapter_path,
                    ?GATT_MANAGER(Member), Types, Args).

send_manager(State, Member, Types, Args) ->
    ebus_proxy:send(State#state.proxy, State#state.adapter_path,
                    ?GATT_MANAGER(Member), Types, Args).

-spec has_gatt_manager(ebux:proxy(), string()) -> ok | {error, term()}.
has_gatt_manager(Proxy, AdapterName) ->
    case ebus_proxy:managed_objects(Proxy) of
        {ok, [Objects]} ->
            case maps:get(AdapterName, Objects, false) of
                false ->
                    {error, no_adapter};
                Props ->
                    case maps:get(?GATT_MANAGER_IFACE, Props, false) of
                        false ->
                            {error, no_gatt_manager};
                        _ ->
                            ok
                    end
            end;
        {error, Error} ->
            {error, Error}
    end.
