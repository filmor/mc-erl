%% @copyright 2012-2013 Gregory Fefelov, Feiko Nanninga

-module(mc_erl_client_lib).
-export([start_reader/1, start_decrypting_reader/2, read_async/1,
         start_writer/2, write_async/2, write_sync/2,
         initialize_encryption/3, decrypter_set_controlling_process/1]).

-include_lib("public_key/include/public_key.hrl").

start_reader(Source) ->
        Self = self(),
        spawn_link(fun() -> reader(Source, Self) end).

start_decrypting_reader(Socket, Key) ->
        Client = self(),
        Decrypter = spawn_link(fun() -> Reader = receive {reader, R} -> R end,
                                        decrypter(Socket, Key, Key, Reader) end),
        spawn_link(fun() -> reader(Decrypter, Client) end).

%% @doc Performs an async read from the source (either Socket or Decrypter).
read_async(Source) ->
	case mc_erl_protocol:decode_packet(Source) of
		{ok, Packet} ->
			Packet;
                {error, closed} ->
			lager:warning("[~s] socket is closed~n", [?MODULE]),
			net_disconnect
	end.

reader(Source, Client) when is_pid(Client) ->
        case read_async(Source) of
                net_disconnect ->
			mc_erl_player_logic:packet(Client, net_disconnect);
                Packet ->
			mc_erl_player_logic:packet(Client, {packet, Packet}),
			reader(Source, Client)
	end.

start_decrypter(Socket, Key) ->
        Self = self(),
        spawn_link(fun() -> decrypter(Socket, Key, Key, Self) end).

decrypter_set_controlling_process(Decrypter) ->
        Decrypter ! {set_controlling_process, self()}.

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
			end;
                {set_controlling_process, Pid} ->
                        decrypter(Socket, Key, IVec, Pid)
	end.

%% @doc Starts an encrypting writer process. For unencrypted sends use write_async/2.
start_writer(Socket, Key) ->
        spawn_link(fun() -> writer(Socket, Key, Key) end).

%% @doc Send packet through Writer process
write_sync(Writer, Packet) ->
        Writer ! {packet, Packet},
        ok.

%% @doc Send packet without Writer process
write_async(Socket, Packet) ->
        Encoded = mc_erl_protocol:encode_packet(Packet),
        ok = gen_tcp:send(Socket, Encoded).

% asynchronous writer to pass on to logic
writer(Socket, Key, IVec) ->
	receive
		stop -> 
			gen_tcp:close(Socket),
			ok;
		{packet, Data} ->
			Encoded = mc_erl_protocol:encode_packet(Data),
			{Encr, NewIVec} = mc_erl_protocol:encrypt(Socket, Key,
                                                                  IVec, Encoded),
			ok = gen_tcp:send(Socket, Encr),
			writer(Socket, Key, NewIVec)
	end.

initialize_encryption(Socket, PublicKey, PrivateKey) ->
        % generate token
        Token = crypto:strong_rand_bytes(4),

        % send encryption_key_request
        P = {encryption_key_request, ["-", PublicKey, Token]},
        mc_erl_client_lib:write_sync(Socket, P),

        % wait for encryption_key_response
        {ok, {encryption_key_response, [EncrSymKey, EncrToken]}}
                = mc_erl_client_lib:read_sync(Socket),

        % check EncrToken
        DecrToken = public_key:decrypt_private(EncrToken, PrivateKey),
        case Token =:= DecrToken of
                true ->
                        DecrSymKey = public_key:decrypt_private(EncrSymKey, PrivateKey),
                        P2 = {encryption_key_response, [<<>>,<<>>]},
                        mc_erl_client_lib:write_sync(Socket, P2),

                        % encryption is in effect!

                        {ok,
                         start_writer(Socket, DecrSymKey),
                         start_decrypter(Socket, DecrSymKey)};
                false ->
                        mc_erl_client_lib:write_sync(Socket, {disconnect,
                                                              ["You suck."]}),
                        gen_tcp:close(Socket),
                        error
        end.
