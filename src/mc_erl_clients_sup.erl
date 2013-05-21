-module(mc_erl_clients_sup).
-behavior(supervisor).

-export([init/1]).

-export([start_link/0, start_client/3]).

start_link() ->
        supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_client(Socket, PublicKey, PrivateKey) ->
        supervisor:start_child(?MODULE, [Socket, PublicKey, PrivateKey]).

init(_) ->
        lager:info("starting clients_sup"),
        {ok, {{simple_one_for_one, 5, 5000},
              [{client, {mc_erl_client, start_link, []},
                permanent, 5000, worker, [mc_erl_client]}]}}.
