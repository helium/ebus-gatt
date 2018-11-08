-module(gatt_service_sup).

-behavior(supervisor).

-export([start_link/0, init/1]).
-export([start_service/4, services/1]).

start_link() ->
    supervisor:start_link(?MODULE, []).

init([]) ->
    SupFlags = #{ strategy => one_for_one},
    {ok, {SupFlags, []}}.

start_service(Sup, Bus, BasePath, {Module, Index, Primary}) ->
    ChildSpec = #{ id => make_ref(),
                   start => {gatt_service, start_link,
                             [Bus, BasePath, Index, Primary, Module, []]},
                   restart => transient,
                   type => worker
                 },
    supervisor:start_child(Sup, ChildSpec).

services(Sup) ->
    [Pid || {_, Pid, _, _} <- supervisor:which_children(Sup)].
