-module(gen_roomba_iface).

-export([
	open/1, close/1,
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
	ok = serctl:write(FD, <<128:8>>),
	serctl:close(FD).

send_bytes(FD, Bytes) -> serctl:write(FD, Bytes).
read_bytes(_FD, _Bytes) -> ok.
