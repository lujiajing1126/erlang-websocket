-module(whosvchat).

-export([start/0]).

start() ->
	userservice:start(),
	websocket:start().