%% @copyright 2013 Feiko Nanninga

-module(mc_erl_config).
-behaviour(gen_server).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

% api functions
-export([start_link/0, set/2, get_server_option/2]).

-record(state, {event_manager}).

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

set(Key, Value) ->
        gen_server:call(?MODULE, {set, Key, Value}).

get_server_option(Name, Key) ->
        {ok, Env} = application:get_env(servers),
        Server = proplists:get_value(Name, Env),
        proplists:get_value(Key, Server).

% gen server stuff
init([]) ->
        {ok, #state{event_manager=gen_event:start_link({local, config_events})}}.

handle_call({set, Key, Value}, _From, State) ->
        application:set(Key, Value),
        gen_event:notify(State#state.event_manager, {Key, Value}),
        {reply, ok, State};

handle_call(Message, _From, State) ->
        io:format("[~s] received call: ~p~n", [?MODULE, Message]),
        {noreply, State}.

handle_cast(Message, State) ->
	io:format("[~s] received cast: ~p~n", [?MODULE, Message]),
	{noreply, State}.

handle_info(Message, State) ->
        io:format("[~s] received info: ~p~n", [?MODULE, Message]),
        {noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.
