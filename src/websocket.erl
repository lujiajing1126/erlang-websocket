-module(websocket).
-author('lujiajing1126@gmail.com').
-export([start/0]).
 
-define(PORT, 12345).
-define(END,1).
-define(CONTINUE,0).
 
start() ->
    {ok, Listen} = gen_tcp:listen(?PORT, [binary, {packet, 0}, {reuseaddr, true}, {active, once}]),
    io:format("listen on ~p~n", [?PORT]),
    par_connect(Listen).
 
par_connect(Listen) ->
    {ok,Socket} = gen_tcp:accept(Listen),
    spawn(fun() -> par_connect(Listen) end),
    wait(Socket).
 
wait(Socket) ->
    receive
        {tcp, Socket, HeaderData} ->
            HeaderList = binary:split(HeaderData, <<"\r\n">>, [global]),
            HeaderList1 = [list_to_tuple(binary:split(Header, <<": ">>)) || Header <- HeaderList, Header /= nomatch],
            {_, SecWebSocketKey} = lists:keyfind(<<"Sec-WebSocket-Key">>, 1, HeaderList1),
            Sha1 = crypto:hash(sha,[SecWebSocketKey, <<"258EAFA5-E914-47DA-95CA-C5AB0DC85B11">>]),
            Base64 = base64:encode(Sha1),
            Handshake = [
                <<"HTTP/1.1 101 Switching Protocols\r\n">>,
                <<"Upgrade: websocket\r\n">>,
                <<"Connection: Upgrade\r\n">>,
                <<"Sec-WebSocket-Accept: ">>, Base64, <<"\r\n">>,
                <<"\r\n">>
            ],
            gen_tcp:send(Socket, Handshake),
            loop(Socket);
        Any ->
            io:format("Received: ~p~n", [Any]),
            wait(Socket)
    end.
 
loop(Socket) ->
    ok = inet:setopts(Socket, [binary,{packet, 0}, {active, once}]),
    receive
        {tcp, Socket, Data} ->
            handle_data(Data, Socket);
        {tcp_closed, Socket} ->
            gen_tcp:close(Socket);
        {message,Data} ->
            io:format("RPC Received:~p~n", [Data]),
            io:format("Socket :~p~n", [Socket]),
            send_message(Data,Socket),
            loop(Socket);
        Any ->
            io:format("Received:~p~n", [Any]),
            loop(Socket)
    end.

%%
%% Websockets internal functions for RFC6455 and hybi draft
%%
parse_frames(hybi, Frames, Socket) ->
    io:format("~p~n",[Frames]),
    try parse_hybi_frames(Socket, Frames, []) of
        Parsed ->
            io:format("L84 ~n",[]), 
            process_frames(Parsed, [])
    catch
        _:_ -> error
    end.

process_frames([], Acc) ->
    lists:reverse(Acc);
process_frames([{Opcode, Payload} | Rest], Acc) ->
    case Opcode of
        8 -> close;
        _ ->
            process_frames(Rest, [Payload | Acc])
    end.

parse_hybi_frames(_, <<>>, Acc) ->
    io:format("hybi1~n",[]),
    lists:reverse(Acc);
parse_hybi_frames(S, <<_Fin:1,
                      _Rsv:3,
                      Opcode:4,
                      _Mask:1,
                      PayloadLen:7,
                      MaskKey:4/binary,
                      Payload:PayloadLen/binary-unit:8,
                      Rest/binary>>,
                  Acc) when PayloadLen < 126 ->
    io:format("hybi2~n",[]),
    Payload2 = hybi_unmask(Payload, MaskKey, <<>>),
    parse_hybi_frames(S, Rest, [{Opcode, Payload2} | Acc]);
parse_hybi_frames(S, <<_Fin:1,
                      _Rsv:3,
                      Opcode:4,
                      _Mask:1,
                      126:7,
                      PayloadLen:16,
                      MaskKey:4/binary,
                      Payload:PayloadLen/binary-unit:8,
                      Rest/binary>>,
                  Acc) ->
    io:format("hybi3~n",[]),
    Payload2 = hybi_unmask(Payload, MaskKey, <<>>),
    parse_hybi_frames(S, Rest, [{Opcode, Payload2} | Acc]);
parse_hybi_frames(Socket, <<_Fin:1,
                           _Rsv:3,
                           _Opcode:4,
                           _Mask:1,
                           126:7,
                           _PayloadLen:16,
                           _MaskKey:4/binary,
                           _/binary-unit:8>> = PartFrame,
                  Acc) ->
    io:format("L113",[]),
    ok = inet:setopts(Socket, [binary,{packet, 0}, {active, once}]),
    receive
        {tcp_closed, _} ->
            gen_tcp:close(Socket),
            exit(normal);
        {ssl_closed, _} ->
            gen_tcp:close(Socket),
            exit(normal);
        {tcp_error, _, _} ->
            gen_tcp:close(Socket),
            exit(normal);
        {tcp, _, Continuation} ->
            parse_hybi_frames(Socket, <<PartFrame/binary, Continuation/binary>>,
                              Acc);
        _ ->
            gen_tcp:close(Socket),
            exit(normal)
    after
        5000 ->
            gen_tcp:close(Socket),
            exit(normal)
    end;
parse_hybi_frames(S, <<_Fin:1,
                      _Rsv:3,
                      Opcode:4,
                      _Mask:1,
                      127:7,
                      PayloadLen:64,
                      MaskKey:4/binary,
                      Payload:PayloadLen/binary-unit:8,
                      Rest/binary>>,
                  Acc) ->
    io:format("hybi5~n",[]),
    Payload2 = hybi_unmask(Payload, MaskKey, <<>>),
    parse_hybi_frames(S, Rest, [{Opcode, Payload2} | Acc]).

%% Unmasks RFC 6455 message
hybi_unmask(<<O:32, Rest/bits>>, MaskKey, Acc) ->
    <<MaskKey2:32>> = MaskKey,
    hybi_unmask(Rest, MaskKey, <<Acc/binary, (O bxor MaskKey2):32>>);
hybi_unmask(<<O:24>>, MaskKey, Acc) ->
    <<MaskKey2:24, _:8>> = MaskKey,
    <<Acc/binary, (O bxor MaskKey2):24>>;
hybi_unmask(<<O:16>>, MaskKey, Acc) ->
    <<MaskKey2:16, _:16>> = MaskKey,
    <<Acc/binary, (O bxor MaskKey2):16>>;
hybi_unmask(<<O:8>>, MaskKey, Acc) ->
    <<MaskKey2:8, _:24>> = MaskKey,
    <<Acc/binary, (O bxor MaskKey2):8>>;
hybi_unmask(<<>>, _MaskKey, Acc) ->
    Acc.

handle_data(Data, Socket) ->
    case parse_frames(hybi, Data, Socket) of
        close ->
            gen_tcp:close(Socket),
            exit(normal);
        error ->
            io:format("L120 close~n"),
            gen_tcp:close(Socket),
            exit(normal);
        Payload ->
            [Line] = Payload,
            case unicode:characters_to_binary(Line) of
                {incomplete,_,_} ->
                    io:format("L127 close~n"),
                    gen_tcp:close(Socket),
                    exit(normal);
                Str ->
                    io:format("ws line 83 String is : ~p~n",[Str]),
                    userservice ! {self(),Socket,{message,Str}},
                    loop(Socket)
            end
    end.

payload_length(N) ->
    case N of
        N when N =< 125 -> << N >>;
        N when N =< 16#ffff -> << 126, N:16 >>;
        N when N =< 16#7fffffffffffffff -> << 127, N:64 >>
    end.

send_message(Str,Socket) ->
    io:format("Send Message to Client: ~p,sizeOf: ~p~n",[Str,size(Str)]),
    Frame = <<1:1, 0:3, 1:4, (payload_length(size(Str)))/binary, Str/binary>>,
    gen_tcp:send(Socket, Frame).