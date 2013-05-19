-module(mc_erl_servers_sup).
-behavior(supervisor).

-export([init/1]).

-export([start_link/0]).

start_link() ->
        supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_) ->
        lager:info("starting servers_sup"),
        {ok, {{simple_one_for_one, 5, 5000},
              [{server, {mc_erl_server, start_link, []},
                permanent, 5000, worker, [mc_erl_server]}]}}.
