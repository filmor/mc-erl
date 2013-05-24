%% @copyright 2012-2013 Gregory Fefelov, Feiko Nanninga

-module(mc_erl_chat).

-export([broadcast/1, broadcast/2, to_player/2, filter_message/1, is_valid_nickname/1]).

-include("records.hrl").

-define(ALLOWED_CHARS, " !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_abcdefghijklmnopqrstuvwxyz{|}~⌂ÇüéâäàåçêëèïîìÄÅÉæÆôöòûùÿÖÜø£Ø×ƒáíóúñÑªº¿®¬½¼¡«»").

broadcast(Player, Message) when is_record(Player, entity) andalso Player#entity.type =:= player ->
	broadcast(Player#entity.name ++ ": " ++ Message);

broadcast(Player, Message) when is_record(Player, entity) ->
	broadcast(Player#entity.name ++ ": " ++ Message).

broadcast(Message) ->
	Players = mc_erl_entity_manager:get_all_players(),
	lists:map(fun(Player) -> to_player(Player, filter_message(Message)) end, Players).



%% Receiver = player record or name or pid
to_player(Name, Message) when is_list(Name) andalso length(Name) > 0 ->
	Receiver = mc_erl_entity_manager:get_player(Name),
	to_player(Receiver, Message);
	
to_player(Receiver, Message) when is_record(Receiver, entity) andalso Receiver#entity.type =:= player ->
	to_player(Receiver#entity.pid, Message);
	
to_player(Logic, Message) when is_pid(Logic) ->
	Parts = split_message(Message),
	lists:map(fun(Part) -> mc_erl_player_logic:packet(Logic, {chat, Part}) end, Parts).

is_valid_nickname(Nickname) ->
	lists:all(fun(X) -> 
		((X >= $a) and (X =< $z)) or ((X >= $A) and (X =< $Z)) or
		((X >= $0) and (X =< $9)) or (X == $-) or (X == $_) end, Nickname).

filter_message(Message) ->
	lists:filter(fun(X) -> lists:member(X, ?ALLOWED_CHARS) end, Message).

split_message(Message) ->
	split_message(Message, []).

split_message(Message, Output) ->
	if length(Message) =< 119 ->
		lists:reverse([Message|Output]);
	true ->
		{Part, Rest} = lists:split(119, Message),
		split_message(Rest, [Part|Output])
	end.


