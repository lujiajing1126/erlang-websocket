-module(userservice).
-export([start/0]).

-include("user.hrl").

start() ->
	ets:new(user,[public,ordered_set,named_table,{keypos,#user.userid}]),
	register(userservice,spawn(fun() -> loop() end)).

loop() ->
	receive
		{From,Socket,{message,Str}} ->
			io:format("step1 receive message: ~p~n",[Str]),
			io:format("json message: ~p~n",[rfc4627:decode(Str)]),
			case rfc4627:decode(Str) of
				{ok,{_,[{"data",Data},{"type",<<"system">>}]},[]} ->
					io:format("json message: ~p~n",[Data]),
					{_,[{"username",UsernameBinary},{"password",PasswordBinary}]} = Data,
					{Username,Password} = {unicode:characters_to_list(UsernameBinary),unicode:characters_to_list(PasswordBinary)},
					save(Username,Username,Password,Socket,From),
					loop();
				{ok,{_,[{"data",Data},{"type",<<"message">>}]},[]} ->
					io:format("json message: ~p~n",[Data]),
					sendMessage(From,Data),
					loop();
				{ok,{_,[{"data",{_,[{"message",MessageBinary}]}},{"to",ToUserBinary},{"type",<<"message">>}]},[]} ->
					{User,Message} = {unicode:characters_to_list(ToUserBinary),unicode:characters_to_list(MessageBinary)},
					find_user_and_send(User,Message),
					loop();
				_Any ->
					loop()
			end
	end.

save(Username,Nickname,Password,Socket,Pid) ->
	Uuid = os:cmd("uuidgen"),
	User = #user{nickname=Nickname,userid=Nickname,password=Password,socket=Socket,pid=Pid,uuid=Uuid},
	ets:insert(user,User),
	io:format("uuid: ~p~n",[Uuid]),
	sendMessage(Pid,"Login Succeed").

find_user_and_send(User,Message) ->
	io:format("send_to: ~p with message: ~p ~n",[User,Message]),
	case ets:lookup(user,User) of
		[ToUser] ->
			sendMessage(ToUser#user.pid,Message)
	end.

sendMessage(Pid,Str) ->
	Pid ! {message,Str}.