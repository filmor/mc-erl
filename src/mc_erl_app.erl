%% @copyright 2012-2013 Gregory Fefelov, Feiko Nanninga

-module(mc_erl_app).
-export([start/2, stop/1, start/0]).

ensure_started(App) ->
	case application:start(App) of
		ok -> ok;
		{error, {already_started, App}} -> ok
	end.

start(_StartType, _StartArgs) ->
	mc_erl_sup:start_link().

stop(_State) -> mc_erl_server_sup:shutdown().

%% to be called from OS' command line
start() ->
        lager:start(),
	ensure_started(mnesia),
	ensure_started(cutkey),
        ensure_started(crypto),
        ensure_started(public_key),
        ensure_started(ssl),
        ensure_started(epgsql),
        mc_erl_entities:init(),
	ok = application:start(mc_erl).
