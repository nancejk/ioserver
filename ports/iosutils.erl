-module(iosutils).
-export([zero_blob/1]).

% Returns a binary blob containing zero
% Bytes bytes wide.
zero_blob(Bytes) ->
    Width = 8*Bytes,
    <<0:Width>>.
