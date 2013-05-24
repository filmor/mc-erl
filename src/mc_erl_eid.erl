%% @copyright 2013 Feiko Nanninga
%%
%% @doc Supplies regions with blocks of 1024 entity ids. Eids are reused.

-module(mc_erl_eid).
-behavior(gen_server).

-export([start_link/0, alloc/0, free/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
          terminate/2, code_change/3]).

-record(state, {freed=[], next=0}).

start_link() ->
        gen_server:start_link({local, ?MODULE}, ?MODULE, []).

alloc() ->
        gen_server:call(?MODULE, alloc).

free(Block) ->
        gen_server:cast(?MODULE, {free, Block}).

init([]) ->
        {ok, #state{}}.

handle_call(alloc, _From, #state{freed=Freed, next=Next}=State) ->
        case Freed of
                [] ->
                        {reply, Next, State#state{next=Next+1}};
                [Block|Tail] ->
                        {reply, Block, State#state{freed=Tail}}
        end;

handle_call(Message, _From, State) ->
	lager:info("[~s] received call: ~p~n", [?MODULE, Message]),
	{noreply, State}.

handle_cast({free, Block}, #state{freed=Freed}=State) ->
        {noreply, State#state{freed=lists:sort([Block|Freed])}};

handle_cast(stop, State) ->
	lager:info("[~s] stopping~n", [?MODULE]),
	{stop, normal, State};

handle_cast(Message, State) ->
	lager:warning("[~s] received cast: ~p~n", [?MODULE, Message]),
	{noreply, State}.

handle_info(Message, State) ->
	lager:warning("[~s] received info: ~p~n", [?MODULE, Message]),
	{noreply, State}.

terminate(Reason, _State) ->
	lager:warning("[~s] terminated with Reason=~p~n", [?MODULE, Reason]),
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.
