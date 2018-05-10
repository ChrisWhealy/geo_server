-module(format).

-author("Chris Whealy <chris.whealy@sap.com>").
-revision("Revision: 1.0.0").
-created("Date: 2018/05/09 18:08:22").
-created_by("chris.whealy@sap.com").

-export([
    as_binary_units/1
]).

-define(KB, 1024).
-define(MB, ?KB * 1024).
-define(GB, ?MB * 1024).

%% =====================================================================================================================
%%
%%                                                 P U B L I C   A P I
%%
%% =====================================================================================================================

%% ---------------------------------------------------------------------------------------------------------------------
%% Format integer as binary units of Kb, Mb or Gb truncated to three decimal places
as_binary_units(undefined)            -> "0 bytes";
as_binary_units(0)                    -> "0 bytes";
as_binary_units(N) when is_integer(N) -> lists:flatten(as_binary_units_int(N)).



%% =====================================================================================================================
%%
%%                                                P R I V A T E   A P I
%%
%% =====================================================================================================================

%% ---------------------------------------------------------------------------------------------------------------------
as_binary_units_int(N) when N < ?KB -> io_lib:format("~w bytes",[N]);
as_binary_units_int(N) when N < ?MB -> as_binary_units_int(N, ?KB, "Kb");
as_binary_units_int(N) when N < ?GB -> as_binary_units_int(N, ?MB, "Mb");
as_binary_units_int(N)              -> as_binary_units_int(N, ?GB, "Gb").

as_binary_units_int(N, Unit, UnitStr) ->
  WholeUnits = N div Unit,
  Rem = (N - (WholeUnits * Unit)) / Unit,
  io_lib:format("~.2f ~s",[WholeUnits + Rem, UnitStr]).


