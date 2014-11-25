-module(userservice).
-export([start/0]).

-include("user.hrl").

start() ->
	register(userservice,spawn(fun() -> loop() end)).

loop() ->
	receive
		{From,{message,Str}} ->
			io:format("receive message: ~p~n",[Str]),
			loop()
	end.