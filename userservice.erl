-module(userservice).
-export([start/0]).

-include("user.hrl").

start() ->
	register(userservice,spawn(fun() -> loop() end)).

loop() ->
	receive
		{From,{store,Key,Value}} ->
			loop()
	end.