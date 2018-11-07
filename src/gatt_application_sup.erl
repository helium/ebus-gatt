-module(gatt_application_sup).

-behavior(supervisor).

-export([start_link/2, init/1]).
-export([service_sup/1]).

-define(SERVICES, services).

start_link(Module, Args) ->
    supervisor:start_link(?MODULE, [Module, Args]).

init([Module, Args]) ->
    SupFlags = {rest_for_one, 3, 10},
    ChildSpecs = [
                  #{ id => application,
                     type => worker,
                     start => {gatt_application, start_link, [self(), Module, Args]}
                   },
                  #{ id => ?SERVICES,
                     type => supervisor,
                     start => {gatt_service_sup, start_link, []}
                   }
                 ],
    {ok, {SupFlags, ChildSpecs}}.

service_sup(Sup) ->
    Children = supervisor:which_children(Sup),
    {?SERVICES, Pid, _, _} = lists:keyfind(?SERVICES, 1, Children),
    Pid.
