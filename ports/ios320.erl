% Erlang module for communicating with Acromag IOS320
% 12 bit ADC.

-module(ios320).
-include("ios320.hrl").
-export([start/0,start/2,stop/0,bytepads/0, read_status/0, configure_default/0, read_channels/0]).

start() ->
    start(run, "./ios320").

start(Mode, Executable) when Mode == run ->
    spawn( fun() ->
		   register( ?MODULE, self() ),
		   process_flag(trap_exit, true),
		   Port = open_port({spawn, Executable}, [{packet,2}]),
		   loop(Port)
	   end);
start(Mode,Executable) ->
    {Mode, Executable}.

stop() ->
    ?MODULE ! stop.

bytepads() ->
    call_port({cmd, self(), <<0:8/integer>>}).

read_status() ->
    call_port({cmd, self(), <<1:8/integer>>}).

read_channels() ->
    call_port({cmd, self(), <<4:8/integer>>}).

configure_default() ->
    [_|T] = tuple_to_list(#ios320config{}),
    BP = lists:map(fun iosutils:zero_blob/1, bytepads()),
    {ok, Final} = iosutils:interleave(T, BP),
    call_port({cmd, self(), list_to_binary([<<2:8/integer>> | Final])}).

call_port(Msg) -> 
    ?MODULE ! Msg,
    receive
	{?MODULE, Result} ->
	    Result
    end.

loop(Port) ->
    receive
	{cmd, Caller, Payload} ->
	    Port ! {self(), {command, Payload}},
	    receive
		{Port, {data,Data}} ->
		    Caller ! {?MODULE, Data}
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

