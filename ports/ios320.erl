% Erlang module for communicating with Acromag IOS320
% 12 bit ADC.

-module(ios320).
-behavior(gen_server).
-export([start_link/0, start_link/1, handle_call/3, init/1, terminate/2]).

% This is a temporary record.  Should be replaced with something reasonable.
-record(ios320config, {yes=no}).

start_link() ->
    gen_server:start_link({local, cardA}, ?MODULE, [cardA], []).

start_link(CardSlot) when is_atom(CardSlot) ->
    case CardSlot of
	slotA ->
	    gen_server:start_link({local, slotA}, ?MODULE, [slotA], []);
	slotB ->
	    gen_server:start_link({local, slotB}, ?MODULE, [slotB], []);
	slotC ->
	    gen_server:start_link({local, slotC}, ?MODULE, [slotC], []);
	slotD ->
	    gen_server:start_link({local, slotD}, ?MODULE, [slotD], []);
	true ->
	    error
    end.

init([CardSlot]) when is_atom(CardSlot) ->
    PortName = list_to_atom( atom_to_list(CardSlot) ++ atom_to_list(port) ),
    spawn( fun() ->
		   Port = open_port({spawn, "./ios320"}, [{packet, 2}, binary, exit_status]),
		   register( PortName, self() ),
		   loop(Port)
	   end),
    {ok, {PortName, #ios320config{}}}.

terminate(shutdown, {PortName, Configuration}=State) ->
    PortName ! stop.

handle_call({bytepads, _}, Caller, {PortName, Configuration}=State) ->
    case call_port({cmd, PortName, <<0:8/integer>>}) of
    {ok, Data} ->
	    {reply, Data, State};
	true ->
	    {error, unknown}
    end.

call_port({cmd, PortName, Msg}) -> 
    PortName ! {cmd, self(), Msg},
    receive
	Result ->
	    Result
    end.

loop(Port) ->
    receive
	{cmd, Caller, Payload} ->
	    Port ! {self(), {command, Payload}},
	    receive
		{Port, {data,Data}} ->
		    Caller ! binary_to_term(Data)
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
