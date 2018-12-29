-module(trace).
-export([
  load_trace/1,
  print_instruction/1,
  print_instructions/1
  ]).

-record(instruction,
  { opcode,
    args = [],
    pos,
    bytes }).

parse_nd(ND) ->
  Dx = ND div 9,
  Dy = (ND rem 9) div 3,
  Dz = ND rem 3,
  {Dx - 1, Dy - 1, Dz - 1}.
  
parse_lld(A, I) ->
  case A of
    1 -> {I - 15, 0, 0};
    2 -> {0, I - 15, 0};
    3 -> {0, 0, I - 15}
  end.

parse_sld(A, I) ->
  case A of
    1 -> {I - 5, 0, 0};
    2 -> {0, I - 5, 0};
    3 -> {0, 0, I - 5}
  end.

read_instruction(BS, File) ->
  Pos = case file:position(File, {cur, 0}) of
    {ok, P} -> P - 1
  end,
  <<B0>> = BS,
  case BS of 
    <<2#11111111:8>> ->
      #instruction{opcode=halt, pos=Pos, bytes=BS};
    <<2#11111110:8>> ->
      #instruction{opcode=wait, pos=Pos, bytes=BS};
    <<2#11111101:8>> ->
      #instruction{opcode=flip, pos=Pos, bytes=BS};
    
    << 2#00:2,  LLDA:2, 2#0100:4>> ->
      read_instruction_smove(Pos, B0, File, LLDA);
    <<SLD2A:2, SLD1A:2, 2#1100:4>> ->
      read_instruction_lmove(Pos, B0, File, SLD1A, SLD2A);

    <<ND:5, 2#111:3>> ->
      #instruction{opcode=fusionp, args=[parse_nd(ND)], pos=Pos, bytes=BS};
    <<ND:5, 2#110:3>> ->
      #instruction{opcode=fusions, args=[parse_nd(ND)], pos=Pos, bytes=BS};
    <<ND:5, 2#101:3>> ->
      read_instruction_fission(Pos, B0, File, ND);
    <<ND:5, 2#011:3>> ->
      #instruction{opcode=fill, args=[parse_nd(ND)], pos=Pos, bytes=BS};
    <<ND:5, 2#010:3>> ->
      #instruction{opcode=void, args=[parse_nd(ND)], pos=Pos, bytes=BS};
    <<ND:5, 2#001:3>> ->
      read_instruction_gfill(Pos, B0, File, ND);
    <<ND:5, 2#000:3>> ->
      read_instruction_gvoid(Pos, B0, File, ND)
  end.

read_instruction_smove(Pos, B0, File, LLDA) ->
  {ok, B1} = file:read(File, 1),
  <<2#000:3, LLDI:5>> = B1,
  #instruction{opcode=smove, args=[parse_lld(LLDA, LLDI)], pos=Pos, bytes=list_to_binary([B0, binary:at(B1, 0)])}.

read_instruction_lmove(Pos, B0, File, SLD1A, SLD2A) ->
  {ok, B1} = file:read(File, 1),
  <<SLD2I:4, SLD1I:4>> = B1,
  #instruction{opcode=lmove, args=[parse_sld(SLD1A, SLD1I), parse_sld(SLD2A, SLD2I)], pos=Pos, bytes=list_to_binary([B0, binary:at(B1, 0)])}.

read_instruction_fission(Pos, B0, File, ND) ->
  {ok, <<M>>} = file:read(File, 1),
  #instruction{opcode=fission, args=[parse_nd(ND), M], pos=Pos, bytes=list_to_binary([B0, M])}.

read_instruction_gfill(Pos, B0, File, ND) ->
  {ok, <<DX:8, DY:8, DZ:8>>} = file:read(File, 3),
  #instruction{opcode=gfill, args=[parse_nd(ND), DZ, DY, DZ], pos=Pos, bytes=list_to_binary([B0, DX, DY, DZ])}.
  
read_instruction_gvoid(Pos, B0, File, ND) ->
  {ok, <<DX:8, DY:8, DZ:8>>} = file:read(File, 3),
  #instruction{opcode=gvoid, args=[parse_nd(ND), DZ, DY, DZ], pos=Pos, bytes=list_to_binary([B0, DX, DY, DZ])}.
  
read_tracefile(File) ->
  case file:read(File, 1) of
    {ok, BS}  ->
      Instr = read_instruction(BS, File),
      [Instr | read_tracefile(File)];
    eof -> 
      io:format("--done--~n"),
      []
  end.

load_trace(Filename) ->
  {ok, File} = file:open(Filename, [read, binary]),
  read_tracefile(File).

print_args([H]) ->
  io_lib:format("~w", [H]);
print_args([H | T]) ->
  io_lib:format("~w, ", [H]) ++ print_args(T).


print_instruction(#instruction{ opcode = Opcode,  args = Args, pos = Pos, bytes = Bytes }) ->
  case Args of
    [] -> io:format("  ~4B: ~-11s ~w~n",    [Pos, utils:hexstr(Bytes), Opcode]);
    _  -> io:format("  ~4B: ~-11s ~w ~s~n", [Pos, utils:hexstr(Bytes), Opcode, print_args(Args)])
  end.

print_instructions([]) ->
  ok;
print_instructions([H | T]) ->
  print_instruction(H),
  print_instructions(T).
