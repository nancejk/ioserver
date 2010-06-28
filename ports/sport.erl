-module(sport).
-export([start/0, stop/0, send_ints/2]).

start() ->
    spawn( fun() ->
		   register( ?MODULE, self() ),
		   process_flag(trap_exit, true),
		   Port = open_port({spawn, "./sport"}, [{packet, 2}]),
		   loop(Port)
	    end).

stop() ->    
    ?MODULE ! stop.

send_ints(X,Y) ->
    call_port({call, self(), <<X:32/little-integer,Y:32/little-integer>>}).

call_port(Msg) ->
    ?MODULE ! Msg,
    receive
	{?MODULE, Result} ->
	    Result
    end.

loop(Port) ->
    receive
	{call, Caller, Payload} ->
	    Port ! {self(), {command,Payload}},
	    receive
		{Port, {data,Data}} ->
		    Caller ! {?MODULE, decode(Data)}
	    end,
	    loop(Port);
	stop ->
	    Port ! {self(), close},
	    receive
		{Port, closed} ->
		    exit(normal)
	    end;
	{'EXIT', Port, Reason} ->
	    exit({port_terminated, Reason})
    end.
	
decode([Int]) ->
    Int.
