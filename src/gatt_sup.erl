-module(gatt_sup).

-behaviour(supervisor).

-export([start_link/0, init/1]).
-export([start_application/2]).

-spec start_link() -> {ok, pid()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok, {{one_for_one, 1, 5}, []}}.

start_application(Module, Args) ->
    ChildSpec = #{
                  id => Module,
                  restart => transient,
                  type => supervisor,
                  start => {gatt_application_sup, start_link, [Module, Args]}
                 },
    supervisor:start_child(?MODULE, ChildSpec).
