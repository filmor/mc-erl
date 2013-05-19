-module(mc_erl_worlds_sup).
-behavior(supervisor).

-export([init/1]).

-export([start_link/0]).

start_link() ->
        supervisor:start_link(?MODULE, []).

init(_) ->
        ignore.
        %{ok, {{simple_one_for_one, 5, 5000}, [
                                %]}}.
