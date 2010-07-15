-module(iosutils).
-export([zero_blob/1, interleave/2]).

% Returns a binary blob containing zero
% Bytes bytes wide.
zero_blob(Bytes) ->
    Width = 8*Bytes,
    <<0:Width>>.

% Interleaves the members of two lists
% A and B.
% interleave([1,2],[3,4]) -> [1,3,2,4].
interleave(A,B) when length(A) == length(B) ->
    {ok, interleave(A,B,[])};
interleave(_, _) ->
    {error, different_lengths}.
interleave([],[],List) ->
    List;
interleave([H1|T1],[H2|T2],List) ->
    interleave(T1,T2,List ++ [H1,H2]).
