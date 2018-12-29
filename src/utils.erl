-module(utils).
-export([hexstr/1]).

hexstr(BS) when is_binary(BS) ->
  hexstr(binary:bin_to_list(BS));
hexstr([H]) ->
  io_lib:format("~2.16.0B", [H]);
hexstr([H | T]) ->
  io_lib:format("~2.16.0B ", [H]) ++ hexstr(T).
