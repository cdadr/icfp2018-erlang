-module(model).
-export([load_model/1, get_bit/4, printLine/3, printLayer/2]).

load_model(Filename) ->
  {ok, File} = file:open(Filename, [read, binary]),
  io:format("Opened ~s OK~n", [Filename]),
  {ok, <<Dim>>} = file:read(File, 1),
  io:format("Dim is ~B~n", [Dim]),
  NBytes = ((Dim * Dim * Dim) + 7) div 8,
  io:format("NBytes is ~B~n", [NBytes]),
  {ok, ModelData} = file:read(File, NBytes),
  io:format("read model: ~w~n", [ModelData]),
  io:format("done.~n"),
  {Dim, ModelData}.

get_bit({Dim, ModelData}, X, Y, Z) ->
  Pos = (((Z * Dim) + Y) * Dim) + X,
  BytePos = Pos div 8,
  BitPos = Pos rem 8,
  ByteVal = binary:at(ModelData, BytePos),
  ((1 bsl BitPos) band ByteVal) =/= 0.

printLine(Model, Y, Z) ->
  {Dim, _} = Model,
  Line = [test1:get_bit(Model, X, Y, Z) || X <- lists:seq(0, Dim - 1)],
  lists:map(fun(true) -> $*; (_) -> $. end, Line).

printLayer(Model, Y) ->
  {Dim, _} = Model,
  lists:map(fun(Line) -> io:format("~s~n", [Line]) end, [test1:printLine(Model, Y, Z) || Z <- lists:seq(0, Dim - 1)]).
