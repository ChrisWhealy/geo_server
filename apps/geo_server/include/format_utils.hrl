
%% -----------------------------------------------------------------------------
%% Format size in binary units Kb, Mb or Gb
kb() -> 1024.
mb() -> kb() * 1024.
gb() -> mb() * 1024.

format_as_binary_units(undefined)             -> io_lib:format("~w bytes",[0]);
format_as_binary_units(N) when N < 1024       -> io_lib:format("~w bytes",[N]);
format_as_binary_units(N) when N < 1048576    -> format_as_binary_units_int(N, kb(), "Kb");
format_as_binary_units(N) when N < 1073741824 -> format_as_binary_units_int(N, mb(), "Mb");
format_as_binary_units(N) when is_integer(N)  -> format_as_binary_units_int(N, gb(), "Gb").

format_as_binary_units_int(N, Unit, UnitStr) ->
  WholeUnits = N div Unit,
  Rem = (N - (WholeUnits * Unit)) / Unit,
  io_lib:format("~.2f ~s",[WholeUnits + Rem, UnitStr]).


