%% @doc Represents a GATT Profile as described in
%% [https://www.bluetooth.com/specifications/gatt/generic-attributes-overview]

-module(gatt_profile).

-include("gatt.hrl").

-behavior(ebus_object).

-export([start_link/3, init/1, handle_message/3]).

-record(state, {
                proxy :: ebus:proxy(),
                adapter_path :: string()
               }).

start_link(Bus, Path, AdapterPath) ->
    ebus_object:start_link(Bus, Path, ?MODULE, [Bus, Path, AdapterPath], []).

init([Bus, Path, AdapterPath]) ->
    {ok, Proxy} = ebus_proxy:start_link(Bus, ?BLUEZ_SERVICE, []),
    case has_gatt_manager(Proxy, AdapterPath) of
        {error, Error} ->
            ebus_proxy:stop(Proxy),
            {stop, {error, Error}};
        ok ->
            case ebus_proxy:call(Proxy, AdapterPath,
                                 ?GATT_MANAGER("RegisterApplication"),
                                 [object_path, {dict, string, variant}],
                                 [Path, #{}]) of
                {ok, _} ->
                    {ok, #state{proxy=Proxy, adapter_path=AdapterPath}};
                {error, Error} ->
                    {stop, {error, Error}}
            end
    end.

handle_message(Member, _Msg, State=#state{}) ->
    lager:warning("Unhandled message ~p", [Member]),
    {noreply, State}.

%%
%% Private
%%

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
            end
    end.
