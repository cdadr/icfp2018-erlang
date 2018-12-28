-module(trace).
-export([load_trace/1]).

-record(instruction,
  { opcode,
    args,
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
      io:format("  ~4B: ~2.16.0B     Halt~n", [Pos, B0]),
      #instruction{opcode=halt, pos=Pos, bytes=BS};
    <<2#11111110:8>> ->
      io:format("  ~4B: ~2.16.0B     Wait~n", [Pos, B0]),
      #instruction{opcode=wait, pos=Pos, bytes=BS};
    <<2#11111101:8>> ->
      io:format("  ~4B: ~2.16.0B     Flip~n", [Pos, B0]),
      #instruction{opcode=flip, pos=Pos, bytes=BS};
    
    << 2#00:2,  LLDA:2, 2#0100:4>> ->
      read_instruction_smove(Pos, B0, File, LLDA);
    <<SLD2A:2, SLD1A:2, 2#1100:4>> ->
      read_instruction_lmove(Pos, B0, File, SLD1A, SLD2A);

    <<ND:5, 2#111:3>> ->
      io:format("  ~4B: ~2.16.0B     FusionP ~w~n", [Pos, B0, parse_nd(ND)]),
      #instruction{opcode=fusionp, args=[parse_nd(ND)], pos=Pos, bytes=BS};
    <<ND:5, 2#110:3>> ->
      io:format("  ~4B: ~2.16.0B     FusionS ~w~n", [Pos, B0, parse_nd(ND)]),
      #instruction{opcode=fusions, args=[parse_nd(ND)], pos=Pos, bytes=BS};
    <<ND:5, 2#101:3>> ->
      read_instruction_fission(Pos, B0, File, ND);
    <<ND:5, 2#011:3>> ->
      io:format("  ~4B: ~2.16.0B     Fill ~w~n", [Pos, B0, parse_nd(ND)]),
      #instruction{opcode=fill, args=[parse_nd(ND)], pos=Pos, bytes=BS};
    <<ND:5, 2#010:3>> ->
      io:format("  ~4B: ~2.16.0B     Void ~w~n", [Pos, B0, parse_nd(ND)]),
      #instruction{opcode=void, args=[parse_nd(ND)], pos=Pos, bytes=BS};
    <<ND:5, 2#001:3>> ->
      read_instruction_gfill(Pos, B0, File, ND);
    <<ND:5, 2#000:3>> ->
      read_instruction_gvoid(Pos, B0, File, ND)
  end.

read_instruction_smove(Pos, B0, File, LLDA) ->
  {ok, B1} = file:read(File, 1),
  case B1 of
    <<2#000:3, LLDI:5>> -> io:format("  ~4B: ~2.16.0B ~2.16.0B  SMove ~w~n", 
        [Pos, B0, binary:at(B1, 0), parse_lld(LLDA, LLDI)]),
    #instruction{opcode=smove, args=[parse_lld(LLDA, LLDI)], pos=Pos, bytes=list_to_binary([B0, binary:at(B1, 0)])}
  end.

read_instruction_lmove(Pos, B0, File, SLD1A, SLD2A) ->
  {ok, B1} = file:read(File, 1),
  <<SLD2I:4, SLD1I:4>> = B1,
  io:format("  ~4B: ~2.16.0B ~2.16.0B  LMove ~w ~w~n", 
      [Pos, B0, binary:at(B1, 0), parse_sld(SLD1A, SLD1I), parse_sld(SLD2A, SLD2I)]),
  #instruction{opcode=lmove, args=[parse_sld(SLD1A, SLD1I), parse_sld(SLD2A, SLD2I)], pos=Pos, bytes=list_to_binary([B0, binary:at(B1, 0)])}.

read_instruction_fission(Pos, B0, File, ND) ->
  {ok, <<M>>} = file:read(File, 1),
  io:format("  ~4B: ~2.16.0B ~2.16.0B  Fission ~w, m=~B~n", 
      [Pos, B0, M, parse_nd(ND), M]),
  #instruction{opcode=fission, args=[parse_nd(ND), M], pos=Pos, bytes=list_to_binary([B0, M])}.

read_instruction_gfill(Pos, B0, File, ND) ->
  {ok, <<DX:8, DY:8, DZ:8>>} = file:read(File, 3),
  io:format("  ~4B: ~2.16.0B ~2.16.0B ~2.16.0B ~2.16.0B  GFill ~w, (~B, ~B, ~B)~n", 
      [Pos, B0, DX, DY, DZ, parse_nd(ND), DZ, DY, DZ]),
  #instruction{opcode=gfill, args=[parse_nd(ND), DZ, DY, DZ], pos=Pos, bytes=list_to_binary([B0, DX, DY, DZ])}.
  
read_instruction_gvoid(Pos, B0, File, ND) ->
  {ok, <<DX:8, DY:8, DZ:8>>} = file:read(File, 3),
  io:format("  ~4B: ~2.16.0B ~2.16.0B ~2.16.0B ~2.16.0B  GVoid ~w, (~B, ~B, ~B)~n", 
      [Pos, B0, DX, DY, DZ, parse_nd(ND), DZ, DY, DZ]),
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
