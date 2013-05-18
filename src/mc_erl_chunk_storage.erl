-module(mc_erl_chunk_storage).

-behaviour(gen_server).

% for debugging:
-export([connect/0]).

-export([start_link/0, stop/0,
         init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([get_worlds/0, add_world/1, clear_world/1, remove_world/1,
         get_chunk/1, get_chunk_versions/1, set_chunk/2, remove_chunk/1]).

-include("records.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start_link() ->
        gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
        gen_server:cast(?MODULE, stop).

get_worlds() ->
        gen_server:call(?MODULE, get_worlds).

add_world(Name) ->
        gen_server:call(?MODULE, {add_world, Name}).

clear_world(Name) ->
        gen_server:call(?MODULE, {clear_world, Name}).

remove_world(Name) ->
        gen_server:call(?MODULE, {remove_world, Name}).

get_chunk(Chunk) ->
        gen_server:call(?MODULE, {get_chunk, Chunk}).

get_chunk_versions(Chunk) ->
        gen_server:call(?MODULE, {get_chunk_versions, Chunk}).

set_chunk(Chunk, Data) when is_record(Data, chunk_data) ->
        gen_server:cast(?MODULE, {set_chunk, Chunk, Data}).

remove_chunk(Chunk) ->
        gen_server:cast(?MODULE, {remove_chunk, Chunk}). 


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% behaviour implementation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-record(state, {db_con}).

init([]) ->
        {ok, #state{db_con=connect()}}.

handle_call(get_worlds, _From, State) ->
        {ok, _, WorldStrings} = pgsql:equery(State#state.db_con,
                                             "SELECT name FROM worlds;"),
        {reply, [list_to_atom(binary_to_list(S)) ||
                        {S} <- WorldStrings], State};

handle_call({add_world, Name}, _From, State) ->
        R = case pgsql:equery(State#state.db_con,
                              "INSERT INTO worlds (name) VALUES ($1);",
                              [Name]) of
                {ok, 1} ->
                        ok;
                {error, {error, error, <<"23505">>, _, _}} ->
                        {error, already_exists}
        end,
        {reply, R, State};

handle_call({clear_world, Name}, _From, State) ->
        R = case pgsql:equery(State#state.db_con,
                              "SELECT id FROM worlds WHERE name = $1", [Name]) of
                {ok, _, []} ->
                        {error, world_not_found};
                {ok, _, [{Id}]} ->
                        {ok, _} = pgsql:equery(State#state.db_con,
                                               "DELETE FROM chunks WHERE world = $1",
                                               [Id]),
                        ok
        end,
        {reply, R, State};

handle_call({remove_world, Name}, _From, State) ->
        R = case pgsql:equery(State#state.db_con,
                              "SELECT id FROM worlds WHERE name = $1", [Name]) of
                {ok, _, []} ->
                        {error, world_not_found};
                {ok, _, [{Id}]} ->
                        {ok, _} = pgsql:equery(State#state.db_con,
                                               "DELETE FROM chunks WHERE world = $1",
                                               [Id]),
                        {ok, _} = pgsql:equery(State#state.db_con,
                                               "DELETE FROM worlds WHERE id = $1",
                                               [Id]),
                        ok
        end,
        {reply, R, State};

handle_call({get_chunk, {chunk, World, X, Y, Z}}, _From, State) ->
        Coord = chunk_to_int8(X, Y, Z),
        case pgsql:equery(State#state.db_con,
                          "SELECT id FROM worlds WHERE name = $1",
                          [World]) of
                {ok, _, []} ->
                        {reply, world_not_found, State};
                {ok, _, [{WorldId}]} ->
                        case pgsql:equery(State#state.db_con,
                                          "SELECT data FROM chunks WHERE world = $1 AND coord = $2 AND selected = true",
                                          [WorldId, Coord]) of
                                {ok, _, []} ->
                                        {reply, chunk_not_found, State};
                                {ok, _, [{Bin}]} ->
                                        {reply, bin_to_chunk(Bin), State}
                        end
        end;

handle_call({get_chunk_versions, {chunk, World, X, Y, Z}}, _From, State) ->
        Coord = chunk_to_int8(X, Y, Z),
        case pgsql:equery(State#state.db_con,
                          "SELECT id FROM worlds WHERE name = $1",
                          [World]) of
                {ok, _, []} ->
                        {reply, world_not_found, State};
                {ok, _, [{WorldId}]} ->
                        {ok, _, Versions} = pgsql:equery(State#state.db_con,
                                                         "SELECT timestamp FROM chunks WHERE world = $1 AND coord = $2",
                                                         [WorldId, Coord]),
                        {reply, [V || {V} <- Versions], State}
        end;

handle_call(Message, _From, State) ->
        io:format("[~s] received unknown call: ~p~n", [?MODULE, Message]),
        {noreply, State}.

handle_cast({set_chunk, {chunk, World, X, Y, Z}, Chunk}, State) when is_record(Chunk, chunk_data) ->
        {ok, _, [{WorldId}]} = pgsql:equery(State#state.db_con,
                                            "SELECT id FROM worlds WHERE name = $1",
                                            [World]),
        Coord = chunk_to_int8(X, Y, Z),
        Data = chunk_to_bin(Chunk),
        {ok, _} = pgsql:equery(State#state.db_con,
                               "UPDATE chunks SET selected = false WHERE world = $1 AND coord = $2 AND selected = true",
                               [WorldId, Coord]),
        {ok, 1} = pgsql:equery(State#state.db_con,
                               "INSERT INTO chunks (world, coord, selected, data) VALUES ($1, $2, true, $3)",
                               [WorldId, Coord, Data]),
        {noreply, State};

handle_cast({remove_chunk, {chunk, World, X, Y, Z}}, State) ->
        Coord = chunk_to_int8(X, Y, Z),
        case pgsql:equery(State#state.db_con,
                          "SELECT id FROM worlds WHERE name = $1",
                          [World]) of
                {ok, _, []} ->
                        {reply, world_not_found, State};
                {ok, _, [{WorldId}]} ->
                        {ok, _} = pgsql:equery(State#state.db_con,
                                               "DELETE FROM chunks WHERE world = $1 AND coord = $2",
                                               [WorldId, Coord]),
                        {noreply, State}
        end;

handle_cast(Message, State) ->
        io:format("[~s] received unknown cast: ~p~n", [?MODULE, Message]),
        {noreply, State}.

handle_info(Message, State) ->
        io:format("[~s] received unknown info: ~p~n", [?MODULE, Message]),
        {noreply, State}.

terminate(_Reason, _State) ->
        ok.

code_change(_OldVsn, State, _Extra) ->
        {ok, State}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% database helpers
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
connect() ->
        {Host, Role, Pw, Db} = mc_erl_config:get(database, undefined),
        {ok, C} = pgsql:connect(Host, Role, Pw, [{database, Db}]),
        C.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% conversions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
chunk_to_int8(X, Y, Z) ->
        <<Coord:64/signed>> = <<X:32/signed, Y:4/unsigned, Z:28/signed>>,
        Coord.

chunk_to_bin(Chunk) when is_record(Chunk, chunk_data) ->
        term_to_binary(Chunk).

bin_to_chunk(Bin) when is_binary(Bin) ->
        binary_to_term(Bin).
