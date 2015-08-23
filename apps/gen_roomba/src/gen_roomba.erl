-module(gen_roomba).
-export([start/0]).

start() -> application:ensure_all_started(gen_roomba).
