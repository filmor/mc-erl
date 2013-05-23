-module(mc_erl_entities).

-export([init/0, add/1, update/1, delete/1, get_entity/1, get_pid/1,
         get_cached_location/1, get_current_location/1,
         get_player_count/0]).

-include("records.hrl").

init() ->
        {atomic, ok} = mnesia:create_table(entity, [{attributes,
                        record_info(fields, entity)}, {index, [name]}]),
        ok.

add(E) when is_record(E, entity) ->
        F = fun() -> [] = mnesia:read(entity, E#entity.eid),
                     mnesia:write(E) end,
        {atomic, ok} = mnesia:transaction(F),
        ok.

update(E) when is_record(E, entity) ->
        F = fun() -> mnesia:write(E) end,
        {atomic, ok} = mnesia:transaction(F),
        ok.

delete(Eid) when is_integer(Eid) ->
        F = fun() -> mnesia:delete(entity, Eid) end,
        {atomic, ok} = mnesia:transaction(F),
        ok;
delete(Entity) when is_record(Entity, entity) ->
        F = fun() -> mnesia:delete_object(Entity) end,
        {atomic, ok} = mnesia:transaction(F),
        ok.

get_entity(Entity) when is_record(Entity, entity) ->
        get_entity(Entity#entity.eid);
get_entity(Eid) when is_integer(Eid) ->
        F = fun() -> mnesia:read(entity, Eid) end,
        {atomic, [Entity]} = mnesia:transaction(F),
        Entity.

get_pid(E) ->
        (get_entity(E))#entity.pid.

get_cached_location(E) ->
        (get_entity(E))#entity.location.

get_current_location(E) ->
        gen_server:call(get_pid(E), get_location).

get_player_count() ->
        MatchSpec = #entity{type=player, _='_'},
        F = fun() -> mnesia:select(entity, MatchSpec) end,
        {atomic, L} = mnesia:transaction(F),
        length(L).
