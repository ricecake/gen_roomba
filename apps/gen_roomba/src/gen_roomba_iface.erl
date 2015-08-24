-module(gen_roomba_iface).

-export([
	open/1, close/1,
	passive/1, active/1,
	drive/3,
	send_bytes/2,
	read_bytes/2
]).

open(Device) when is_list(Device) ->
	{ok, FD} = serctl:open(Device),
	Termios = lists:foldl(
	    fun(Fun, Acc) -> Fun(Acc) end,
	    serctl:mode(raw),
	    [
		fun(N) -> serctl:flow(N, false) end,
		fun(N) -> serctl:ispeed(N, b115200) end,
		fun(N) -> serctl:ospeed(N, b115200) end
	    ]
	),
	ok = serctl:tcsetattr(FD, tcsanow, Termios),
	ok = serctl:write(FD, <<128:8, 131:8>>),
	{ok, FD}.

close(FD) ->
	ok = serctl:write(FD, <<128:8, 133:8>>),
	serctl:close(FD).

passive(FD) -> gen_roomba_iface:send_bytes(FD, <<128:8>>).
active(FD)  -> gen_roomba_iface:send_bytes(FD, <<131:8>>).

drive(FD, 0,     right) -> drive(FD, 150, -1);
drive(FD, 0,     left)  -> drive(FD, 150,  1);
drive(FD, Speed, right) -> drive(FD, Speed, -2000);
drive(FD, Speed, left)  -> drive(FD, Speed,  2000);
drive(FD, Speed, none)  -> drive(FD, Speed, 32768);
drive(FD, Speed, Radius) when (Speed >= -500) and (Speed =< 500) and
			      (Radius >= -32768) and (Radius =< 32768) ->
	gen_roomba_iface:send_bytes(FD, <<137, Speed:16, Radius:16>>).

send_bytes(FD, Bytes) -> serctl:write(FD, Bytes).
read_bytes(_FD, _Bytes) -> ok.
