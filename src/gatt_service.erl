-module(gatt_service).

-callback init(Args::any()) -> {ok, [gatt:characteristic_spec()], State::any()} | {error, term()}.
-callback uuid() -> gatt:uuid().
-callback handle_signal(SignalID::ebus:filter_id(), Msg::ebus:messgage(),
                        State::any()) -> ebus_object:handle_info_result().
-callback handle_info(Msg::any(), State::any()) -> ebus_object:handle_info_result().

-optional_callbacks([handle_signal/3, handle_info/2]).

-behavior(ebus_object).
-behavior(gatt_object).

-include("gatt.hrl").
-include_lib("ebus/include/ebus.hrl").

%% ebus_object
-export([start_link/6, init/1, handle_message/3, handle_call/3, handle_info/2]).
%% gatt_object
-export([properties/1, uuid/1]).
%% API
-export([add_characteristic/2, fold_characteristics/3,
         add_descriptor/3]).

-export([path/1]).

-type service() :: pid().
-type spec() :: {Module::atom(), Index::non_neg_integer(), Primary::boolean() } |
                {Module::atom(), Index::non_neg_integer(), Primary::boolean(), Args::any()}.
-export_type([service/0, spec/0]).

-record(state, {
                bus :: ebus:bus(),
                path :: string(),
                primary :: boolean(),
                module :: atom(),
                state :: any(),
                path_mp :: re:mp(),
                characteristics=#{} :: #{ebus:oject_path() => gatt:characteristic()}
               }).

-spec add_characteristic(pid(), gatt:characteristic_spec()) -> {ok, ebus:object_path()} | {error, term()}.
add_characteristic(Pid, CharSpec) ->
    gen_server:call(Pid, {add_characteristic, CharSpec}).

-spec fold_characteristics(pid(),
                           fun((ebus:object_path(),
                                gatt:characteristic(),
                                AccIn::any()) -> AccOut::any()),
                           Acc0::any()) -> Acc1::any().
fold_characteristics(Pid, Fun, Acc) ->
    gen_server:call(Pid, {fold_characteristics, Fun, Acc}).

-spec add_descriptor(pid(), ebus:object_path(), gatt_descriptor:spec())
                    -> {ok, ebus:object_path()} | {error, term()}.
add_descriptor(Pid, CharPath, DescSpec) ->
    gen_server:call(Pid, {add_descriptor, CharPath, DescSpec}).

%% gatt_object
properties(Pid) ->
    gen_server:call(Pid, properties).

uuid(Pid) ->
    gen_server:call(Pid, uuid).

path(Pid) ->
    gen_server:call(Pid, path).

start_link(Bus, BasePath, Index, Primary, Module, Args) ->
    Path = BasePath ++ "/service" ++ integer_to_list(Index),
    ebus_object:start_link(Bus, Path, ?MODULE, [Bus, Path, Primary, Module, Args], []).

init([Bus, Path, Primary, Module, Args]) ->
    case Module:init(Args) of
        {ok, CharSpecs, ModuleState} ->
            {ok, PathPattern} = re:compile("^(" ++ Path ++ ")(/char[0-9]+)?.*"),
            State0 = #state{bus=Bus,
                            path=Path,
                            module=Module,
                            state=ModuleState,
                            primary=Primary,
                            path_mp=PathPattern},
            State1 = lists:foldl(fun(_, {error, Error}) -> {error, Error};
                                    (CharSpec, AccState0) ->
                                         case start_characteristic(CharSpec, AccState0) of
                                             {ok, _, AccState1} -> AccState1;
                                             {error, Error} -> {error, Error}
                                         end
                                 end, State0, CharSpecs),
            {ok, State1};
        {error, Error} -> {error, Error}
    end.

handle_message(Member, Msg, State=#state{}) ->
    case find_characteristic(ebus_message:path(Msg), State) of
        {error, no_path} ->
            {reply_error, ?GATT_ERROR_FAILED, Member, State};
        {error, service_path} ->
            %% Only service path detected
            handle_message_service(Member, Msg, State);
        {ok, CharKey, Characteristic} ->
            %% Characteristic detected. Pass on to characteristic
            handle_message_characteristic(CharKey, Characteristic, Member, Msg, State)
    end;


handle_message(Member, _Msg, State) ->
    lager:warning("Unhandled service message ~p", [Member]),
    {reply_error, ?GATT_ERROR_NOT_SUPPORTED, Member, State}.

handle_call(path, _From, State=#state{}) ->
    {reply, State#state.path, State};
handle_call(uuid, _From, State=#state{module=Module}) ->
    {reply, Module:uuid(), State};
handle_call(properties, _From, State=#state{}) ->
    {reply, #{?GATT_SERVICE_IFACE => mk_properties(State)}, State};
handle_call({add_characteristic, Module, Index, Args}, _From, State=#state{}) ->
    case start_characteristic({Module, Index, Args}, State) of
        {ok, CharPath, NewState} ->
            {reply, {ok, CharPath}, NewState};
        {error, Error} ->
            {reply, {error, Error}, State}
    end;
handle_call({fold_characteristics, Fun, Acc}, _From, State=#state{}) ->
    {reply, maps:fold(Fun, Acc, State#state.characteristics), State};
handle_call({add_descriptor, CharPath, DescSpec}, _From, State=#state{}) ->
    case find_characteristic(CharPath, State) of
        {error, Error} ->
            {reply, {error, Error}, State};
        {ok, CharKey, Characteristic} ->
            case gatt_characteristic:add_descriptor(Characteristic, DescSpec) of
                {ok, DescPath, NewCharaceristic} ->
                    {reply, {ok, DescPath},
                     update_characteristic(CharKey, NewCharaceristic, State)};
                {error, Error} ->
                    {reply, {error, Error}, State}
            end
    end;

handle_call(Msg, _From, State) ->
    lager:warning("Unhandled call ~p", [Msg]),
    {noreply, State}.


handle_info({properties_changed, Path, IFace, Changed, Invalidated}, State=#state{}) ->
    {noreply, State,
     {signal, Path, ?DBUS_PROPETIES_INTERFACE, "PropertiesChanged",
      [string, {dict, string, variant}, {array, string}],
      [IFace, Changed, Invalidated]}};
handle_info({ebus_signal, Path, SignalID, Msg}, State=#state{module=Module, state=ModuleState}) ->
    case find_characteristic(Path, State) of
        {error, no_path} ->
            lager:warning("Received signal ~p without path: ~p", [SignalID, Path]),
            {noreply, State};
        {error, service_path} ->
            %% Only service path detected
            case erlang:function_exported(Module, handle_signal, 3) of
                false -> {noreply, State};
                true ->
                    Result = Module:handle_signal(SignalID, Msg, ModuleState),
                    handle_info_result_service(Result, State)
            end;
        {ok, CharKey, Characteristic} ->
            %% Characteristic detected. Pass on to characteristic
            Result = gatt_characteristic:handle_signal(SignalID, Msg, Characteristic),
            handle_info_result_characteristic(Result, CharKey, State)
    end;
handle_info({ebus_info, Path, Msg}, State=#state{}) ->
    case find_characteristic(Path, State) of
        {error, no_path} ->
            lager:warning("Received ebus_info without path: ~p", [Path]),
            {noreply, State};
        {error, service_path} ->
            %% Only service path detected
            handle_info(Msg, State);
        {ok, CharKey, Characteristic} ->
            %% Characteristic detected. Pass on to characteristic
            Result = gatt_characteristic:handle_info(Msg, Characteristic),
            handle_info_result_characteristic(Result, CharKey, State)
    end;
handle_info(Msg, State=#state{module=Module, state=ModuleState}) ->
    case erlang:function_exported(Module, handle_info, 2) of
        false ->
            lager:warning("Unhandled service info ~p", [Msg]),
            {noreply, State};
        true ->
            Result = Module:handle_info(Msg, ModuleState),
            handle_info_result_service(Result, State)
    end.

%%
%% Internal
%%

-spec find_characteristic(ebus:object_path(), #state{}) -> {error, no_path} |
                                                           {error, service_path} |
                                                           {ok, CharKey::string(), gatt:characteristic()}.
find_characteristic(undefined, _State=#state{})->
    {error, no_path};
find_characteristic(CharPath, State=#state{path=ServicePath}) ->
    case re:run(CharPath, State#state.path_mp,
                [{capture, all_but_first, list}]) of
        nomatch -> {error, no_path};
        {match, [ServicePath]} -> {error, service_path};
        {match, [ServicePath, CharKey]} ->
            case maps:get(CharKey, State#state.characteristics, false) of
                false -> {error, {no_characteristic, CharKey}};
                Characteristic -> {ok, CharKey, Characteristic}
            end
    end.

-spec start_characteristic(gatt_characteristic:spec(), #state{})
                          -> {ok, ebus:object_path(), #state{}} | {error, term()}.
start_characteristic({Module, Index, Args}, State=#state{}) ->
    CharKey = "/char" ++ erlang:integer_to_list(Index),
    CharPath = State#state.path ++ CharKey,
    case gatt_characteristic:init([State#state.bus, State#state.path, CharPath, Module, Args]) of
        {ok, Characteristic} ->
            {ok, CharPath, update_characteristic(CharKey, Characteristic, State)};
        {error, Error} ->
            {error, Error}
    end.

-spec update_characteristic(string(), gatt:characteristic(), #state{}) -> #state{}.
update_characteristic(CharKey, Characteristic, State=#state{}) ->
    NewChars = maps:put(CharKey, Characteristic, State#state.characteristics),
    State#state{characteristics=NewChars}.

-spec mk_properties(#state{}) -> #{string() => any()}.
mk_properties(State=#state{module=Module}) ->
    #{"UUID" => Module:uuid(),
      "Primary" => State#state.primary,
      "Characteristics" =>
          [gatt_characteristic:path(C) || C <- maps:values(State#state.characteristics)]
     }.

handle_message_service(Member=?DBUS_PROPERTIES("GetAll"), Msg, State=#state{}) ->
    case ebus_message:interface(Msg) of
        ?GATT_SERVICE_IFACE ->
            {reply, [{dict, string, variant}],  [mk_properties(State)], State};
        Other ->
            lager:warning("Unhandled service request ~p(\"~p\")", [Member, Other]),
            {reply_error, ?GATT_ERROR_NOT_SUPPORTED, Member, State}
    end;
handle_message_service(Member, _Msg, State=#state{}) ->
    lager:warning("Unhandled service message ~p", [Member]),
    {reply_error, ?GATT_ERROR_NOT_SUPPORTED, Member, State}.

handle_message_characteristic(CharKey, Characteristic, Member, Msg, State) ->
    case gatt_characteristic:handle_message(Member, Msg, Characteristic) of
        {reply, Types, Args, NewCharacteristic} ->
            {reply, Types, Args,
             update_characteristic(CharKey,NewCharacteristic, State)};
        {reply_error, ErrorName, ErrorMsg, NewCharacteristic} ->
            {reply_error, ErrorName, ErrorMsg,
             update_characteristic(CharKey, NewCharacteristic, State)};
        {noreply, NewCharaceristic} ->
            {noreply,
             update_characteristic(CharKey, NewCharaceristic, State)}
    end.

handle_info_result_service(Result, State=#state{}) ->
    case Result of
        {noreply, NewModuleState} ->
            {noreply, State#state{state=NewModuleState}};
        {noreply, NewModuleState, Action} ->
            {noreply, State#state{state=NewModuleState}, Action};
        {stop, Reason, NewModuleState} ->
            {stop, Reason, State#state{state=NewModuleState}}
    end.

handle_info_result_characteristic(Result, CharKey, State=#state{}) ->
    case Result of
        {noreply, NewCharacteristic} ->
            {noreply, update_characteristic(CharKey, NewCharacteristic, State)};
        {noreply, NewCharacteristic, Action} ->
            {noreply, update_characteristic(CharKey, NewCharacteristic, State), Action};
        {stop, Reason, NewCharacteristic} ->
            {stop, Reason, update_characteristic(CharKey, NewCharacteristic, State)}
    end.
