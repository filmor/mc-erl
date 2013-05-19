-module(mc_erl_chunk_generator).

-export([gen_chunk/1, gen_biome/1]).

-include("records.hrl").

gen_chunk({chunk, _X, Y, _Z}=C) ->
	io:format("[~w] generating chunk ~p~n", [?MODULE, C]),
	EmptyChunk = #chunk_data{types=binary:copy(<<0>>,16*16*16),
	                         metadata=binary:copy(<<0>>,16*16*8),
	                         block_light=binary:copy(<<255>>,16*16*8),
	                         sky_light=binary:copy(<<255>>,16*16*8)},
	if
		Y =:= 0 ->
			EmptyChunk#chunk_data{types=list_to_binary([binary:copy(<<7>>,256),
			                                            binary:copy(<<1>>,256*15)])};
		Y =:= 1; Y =:= 2 ->
			EmptyChunk#chunk_data{types=list_to_binary([binary:copy(<<7>>,256),
			                                            binary:copy(<<1>>,256*15)])};
		Y =:= 3 ->
			EmptyChunk#chunk_data{types=list_to_binary([binary:copy(<<3>>,256*15),
	                                                       binary:copy(<<2>>,256)])};
	    Y =:= 4 -> EmptyChunk
	end.

gen_biome({column, X, Y}) ->
	io:format("[~w] generating biome data ~p~n", [?MODULE, {X, Y}]),
	binary:copy(<<0>>,256).
