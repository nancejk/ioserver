% Erlang module for communicating with Acromag IOS320
% 12 bit ADC.

-module(ios320).
-include("ios320.hrl").
-behavior(gen_server).
-export([start_link/0, start_link/1, handle_call/3, init/1, terminate/2]).

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
    {ok, {PortName, CardSlot, noconfig}}.

terminate(shutdown, {PortName, _CardSlot, _Configuration}=_State) ->
    PortName ! stop.

handle_call({bytepads, _Arbitrary}, _Caller, {PortName, _CardSlot, _Configuration}=State) ->
    io:format("bytepads called~n"),
    case call_port({cmd, PortName, <<0:8/integer>>}) of
	{ok, Data} ->
	    {reply, Data, State};
	true ->
	    {error, unknown}
    end;

handle_call({configure, initial}, _Caller, {PortName, CardSlot, noconfig}=_State) ->
    io:format("entered initial configuration~n"),
    [ _ | Body ] = tuple_to_list(#ios320config{}),
    % Can we somehow get the gen_server to take care of this?
    case call_port({cmd, PortName, <<0:8/integer>>}) of
	{ok, Bytepads} ->
	    io:format("building bytepads (len ~p)~n",[length(Bytepads)]),
	    {ok, FinalBlob} = iosutils:interleave(Body, Bytepads),
	    io:format("calling port~n"),
	    case call_port({cmd, PortName, list_to_binary([<<1:8/integer>> | FinalBlob])}) of
		{ok, _} ->
		    io:format("got reply~n"),
		    {reply, ok, FinalBlob};
		true ->
		    {reply, error, noconfig}
	    end;
	 true ->
	    {reply, error, noconfig}
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

