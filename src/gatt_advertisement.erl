-module(gatt_advertisement).

-callback init(Args::any()) -> {ok, State::any()} | {error, term()}.
-callback type() -> peripheral | broadcast.
-callback services(State::any()) -> [atom()].
-callback sollicit_uuids(State::any()) -> [gatt:uuid()].
-callback manufacturer_data(State::any()) -> #{Code::integer() => Data::binary()}.
-callback service_data(State::any()) -> #{Key::string() => Data::binary()}.
-callback include_tx_power(State::any()) -> boolean().

-optional_callbacks([services/1, sollicit_uuids/1, manufacturer_data/1,
                     service_data/1, include_tx_power/1]).

-behavior(ebus_object).

-include("gatt.hrl").
-include_lib("ebus/include/ebus.hrl").

%% ebus_object
-export([start_link/5, init/1, handle_message/3, handle_call/3, handle_info/2]).
%% API
-export([properties/1, path/1, stop/2]).

-type advertisement() :: pid().
-export_type([advertisement/0]).

-record(state, {
                proxy :: ebus:proxy(),
                path :: string(),
                adapter_path :: string(),
                module :: atom(),
                state :: any()
               }).

path(Pid) ->
    gen_server:call(Pid, path).

properties(Pid) ->
    gen_server:call(Pid, properties).

stop(Pid, Reason) ->
    gen_server:stop(Pid, Reason, 5000).

start_link(Bus, BasePath, Index, Module, Args) ->
    Path = BasePath ++ "/adv" ++ integer_to_list(Index),
    ebus_object:start_link(Bus, Path, ?MODULE, [Bus, Path, Module, Args], []).

init([Bus, Path, Module, Args]) ->
    AdapterPath = Module:adapter_path(),
    case Module:init(Args) of
        {ok, ModuleState} ->
            {ok, Proxy} = ebus_proxy:start_link(Bus, ?BLUEZ_SERVICE, []),
            {ok, _} = ebus_proxy:call(Proxy, AdapterPath, ?DBUS_PROPERTIES("Set"),
                                      [string, string, variant],
                                      [?GATT_ADAPTER_IFACE, "Powered", true]),
            self() ! register_advertisement,
            {ok, #state{proxy=Proxy,
                        adapter_path=AdapterPath,
                        path=Path,
                        module=Module,
                        state=ModuleState}};
        {error, Error} -> {error, Error}
    end.


handle_message(?DBUS_PROPERTIES("GetAll"), _Msg, State=#state{}) ->
    {reply, [{dict, string, variant}],  [mk_properties(State)], State};
handle_message(?DBUS_OBJECT_MANAGER("GetManagedObjects"), _Msg, State=#state{}) ->
    {reply, [{dict, object_path, {dict, string, {dict, string, variant}}}],
     [#{}], State};

handle_message(Member, _Msg, State=#state{}) ->
    lager:warning("Unhandled advertisement message ~p", [Member]),
    {reply_error, ?GATT_ERROR_NOT_SUPPORTED, Member, State}.


handle_call(path, _From, State=#state{}) ->
    {reply, State#state.path, State};
handle_call(properties, _From, State=#state{}) ->
    {reply, #{?GATT_ADVERTISEMENT_IFACE => mk_properties(State)}, State};

handle_call(Msg, _From, State) ->
    lager:warning("Unhandled call ~p", [Msg]),
    {noreply, State}.


handle_info(register_advertisement, State=#state{}) ->
    Parent = self(),
    spawn_link(fun() ->
                       RegResult = call_manager(State, "RegisterAdvertisement",
                                               [object_path, {dict, string, variant}],
                                               [State#state.path, #{}]),
                       Parent ! {register_advertisement_result, RegResult}
               end),
    {noreply, State};
handle_info({register_advertisement_result, Result}, State=#state{}) ->
    case Result of
        {ok, _} ->
            lager:info("Started advertisement ~p", [State#state.module]),
            {noreply, State};
        {error, Error} ->
            lager:error("Failed to start advertisement: ~p", [Error]),
            {stop, {error, Error}, State}
    end;

handle_info(Msg, State) ->
    lager:warning("Unhandled info message ~p", [Msg]),
    {noreply, State}.





%%
%% Internal
%%

call_manager(State, Member, Types, Args) ->
    ebus_proxy:call(State#state.proxy, State#state.adapter_path,
                    ?GATT_ADVERTISING_MANAGER(Member), Types, Args).


-spec mk_properties(#state{}) -> #{string() => any()}.
mk_properties(#state{module=Module, state=ModuleState}) ->
    BaseProps = #{"Type" => adv_type_to_string(Module:type())},
    Ident = fun(V) -> V end,
    OptKeys = [{"ServiceUUIDs", services, fun(Modules) ->
                                                  [M:uuid() || M <- Modules]
                                          end},
               {"SollicitUUIDs", sollicit_uuids, Ident},
               {"ManufacturerData", manufacturer_data, Ident},
               {"ServiceData", service_data, Ident},
               {"IncludeTxPower", include_tx_power, Ident},
               {"LocalName", local_name, Ident}
              ],
    lists:foldl(fun({Key, Fun, Transform}, Acc) ->
                        case erlang:function_exported(Module, Fun, 1) of
                            false -> Acc;
                            true ->
                                maps:put(Key, Transform(Module:Fun(ModuleState)), Acc)
                        end
                end, BaseProps, OptKeys).


adv_type_to_string(peripheral) ->
    "peripheral";
adv_type_to_string(broadcast) ->
    "broadcast";
adv_type_to_string(Type) ->
    erlang:error({invalid_advertising_type, Type}).
