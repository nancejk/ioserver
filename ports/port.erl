-module(port).

% API
-export([start/0, start/1, stop/0, add/2, multiply/2, divide/2]).
% Internal
-export([init/1]).

start() ->
  start("./cport").
start(ExtPrg) ->
  spawn_link(?MODULE, init, [ExtPrg]).
stop() ->
  ?MODULE ! stop.

add(X,Y) ->
  call_port({add, X, Y}).
multiply(X, Y) ->
  call_port({multiply, X, Y}).
divide(X, Y) ->
  call_port({divide, X, Y}).

call_port(Msg) ->
  ?MODULE ! {call, self(), Msg},
  receive
    Result ->
      Result
  end.

init(ExtPrg) ->
  register(?MODULE, self()),
  process_flag(trap_exit, true),
  Port = open_port({spawn, ExtPrg}, [{packet, 2}]), 
  loop(Port).

loop(Port) ->
  receive
    {call, Caller, Msg} ->
      io:format("Calling port with ~p~n", [Msg]),
      erlang:port_command(Port, encode(Msg)),
      io:format("Waiting for response...~n"),
      receive
	{Port, {data,Data}} ->
	  Caller ! decode(Data);
	{Port, {exit_status, Status}} when Status > 128 ->
	  io:format("Port terminated with signal: ~p~n", [Status-128]),
	  exit({port_terminated, Status});
	{Port, {exit_status, Status}} ->
	  io:format("Port terminated with status: ~p~n", [Status]),
	  exit({port_terminated, Status});
	{'EXIT', Port, Reason} ->
	  exit(Reason)
      end,
      loop(Port);
    stop ->
      erlang:port_close(Port),
      exit(normal)
  end.

encode({add, X, Y}) ->
  [1,X,Y];
encode({multiply, X, Y}) ->
  [2,X,Y];
encode({divide, X, Y}) ->
  [3,X,Y].

decode([Int]) ->
  Int.
