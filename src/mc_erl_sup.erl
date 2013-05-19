-module(mc_erl_sup).
-behavior(supervisor).

-export([init/1]).

-export([start_link/0]).

start_link() ->
        supervisor:start_link(?MODULE, []).

init(_) ->
        {ok, {{one_for_one, 5, 5000},
              [{worlds_sup, {mc_erl_worlds_sup, start_link, []},
                       permanent, infinity, supervisor, [mc_erl_worlds_sup]},
               {servers_sup, {mc_erl_servers_sup, start_link, []},
                       permanent, infinity, supervisor, [mc_erl_servers_sup]}
              ]}}.
