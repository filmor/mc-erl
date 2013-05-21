-module(mc_erl_client_lib).
-export([init_player/3]).

-include_lib("public_key/include/public_key.hrl").

init_player(Socket, PublicKey, PrivateKey) when is_record(PrivateKey, 'RSAPrivateKey') ->
	proc_lib:init_ack(self()),

	{ok, Packet} = mc_erl_protocol:decode_packet(Socket),
	case Packet of
		{server_list_ping, [1] } ->
			send(Socket, {disconnect,[lists:flatten([
				167, "1", 0,
				"51", 0,
				"1.4.6", 0,
				mc_erl_config:get(description, []), 0,
				integer_to_list(mc_erl_entity_manager:player_count()), 0,
				"101"])]}),
			gen_tcp:close(Socket);

		{handshake, [51, Name, _Host, _Port]} ->
			io:format("[~s] Player joining: ~s~n", [?MODULE, Name]),
			
			% generate token
			Token = crypto:strong_rand_bytes(4),

			% send encryption_key_request
			send(Socket, {encryption_key_request, ["-", PublicKey, Token]}),

			% wait for encryption_key_response
			{ok, {encryption_key_response, [EncrSymKey, EncrToken]}} = mc_erl_protocol:decode_packet(Socket),

			% check EncrToken
			DecrToken = public_key:decrypt_private(EncrToken, PrivateKey),
			case Token =:= DecrToken of
				true ->
					DecrSymKey = public_key:decrypt_private(EncrSymKey, PrivateKey),
					send(Socket, {encryption_key_response, [<<>>,<<>>]}),
					
					% encryption is in effect!
					
					Writer = proc_lib:spawn_link(fun() -> async_writer(Socket, DecrSymKey, DecrSymKey) end),
					Logic = mc_erl_player_logic:start_logic(Writer, Name),
					Self = self(),
					Decoder = spawn_link(fun() -> reader(Self, Logic) end),
					mc_erl_player_logic:packet(Logic, login_sequence),
					
					decrypter(Socket, DecrSymKey, DecrSymKey, Decoder);
				false ->
					send(Socket, {disconnect, ["You suck."]}),
					gen_tcp:close(Socket)
			end;
		{handshake, [_, _, _, _]} ->
			send(Socket, {disconnect, ["You're too oldschool! (Wrong client version.)"]}),
			gen_tcp:close(Socket)
	end.

reader(Decrypter, Logic) when is_pid(Logic) ->
	case mc_erl_protocol:decode_packet(Decrypter) of
		{ok, Packet} ->
			mc_erl_player_logic:packet(Logic, {packet, Packet}),
			reader(Decrypter, Logic);
		{error, closed} ->
			io:format("[~s] socket is closed~n", [?MODULE]),
			mc_erl_player_logic:packet(Logic, net_disconnect)
	end.

decrypter(Socket, Key, IVec, Decoder) ->
	receive
		{get_bytes, N} ->
			try mc_erl_protocol:decrypt(Socket, Key, IVec, N) of
				{Bytes, NewIVec} ->
					Decoder ! {bytes, Bytes},
					decrypter(Socket, Key, NewIVec, Decoder)
			catch
				connection_closed ->
					Decoder ! {error, closed}
			end
	end.



% asynchronous writer to pass on to logic
async_writer(Socket, Key, IVec) ->
	receive
		stop -> 
			gen_tcp:close(Socket),
			ok;
		{packet, Data} ->
			Encoded = mc_erl_protocol:encode_packet(Data),
			{Encr, NewIVec} = mc_erl_protocol:encrypt(Socket, Key, IVec, Encoded),
			gen_tcp:send(Socket, Encr),
			async_writer(Socket, Key, NewIVec)
	end.

send(Writer, Packet) ->
        Writer ! {packet, mc_erl_protocol:encode_packet(Packet)}.
