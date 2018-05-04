%% =====================================================================================================================
%%
%%                                F O R M A T   T I M E   V A L U E S   A S   S T R I N G S
%%
%% =====================================================================================================================

%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
%% Transform Erlang local time or timestamp to string
format_datetime(undefined) -> undefined;

% Standard Erlang timestamp format
format_datetime({Mega,Sec,Micro}) ->
  format_datetime(calendar:now_to_local_time({Mega,Sec,Micro}));

% Standard Erlang DateTime format
format_datetime({{YYYY,MM,DD},{H,M,S}}) ->
  lists:flatten(io_lib:format("~w ~s ~s, ~2..0B:~2..0B:~2..0B", [YYYY, month_name(MM), format_day(DD), H, M, S]));

% Custom DateTime format with additional microseconds value
format_datetime({{YYYY,MM,DD},{H,M,S,Micro}}) ->
  S1 = S + make_millis(Micro),
  lists:flatten(io_lib:format("~w ~s ~s, ~2..0B:~2..0B:~6.3.0fs", [YYYY, month_name(MM), format_day(DD), H, M, S1])).


%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% Convert a number of seconds to string
format_seconds(undefined) -> undefined;

format_seconds(0)   -> "0.0s";
format_seconds(0.0) -> "0.0s";

format_seconds(T) -> lists:flatten(format_seconds_int(T)).

format_seconds_int(T) when T >= 3600 ->
  H  = trunc(T) div 3600,
  T1 = trunc(T) rem 3600,
  S  = (T1 rem 60) + mantissa(T), 

  io_lib:format("~w:~2..0B:~6.3.0fs",[H, (T1 div 60), S]);

format_seconds_int(T) when T >= 60 ->
  M = trunc(T) div 60,
  S = (trunc(T) rem 60) + mantissa(T),
  io_lib:format("~w:~6.3.0fs",[M, S]);

format_seconds_int(T) when is_integer(T) -> io_lib:format("~ws",[T]);
format_seconds_int(T)                    -> io_lib:format("~.3fs",[T]).

%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% Month number to name conversion
month_name(MM) -> element(MM, {"Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"}).

%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% Add ordinal string to a day number
format_day(D) -> integer_to_list(D) ++ ordinal(D).

ordinal(D) when D == 1; D == 21; D == 31 -> "st";
ordinal(D) when D == 2; D == 22          -> "nd";
ordinal(D) when D == 3; D == 23          -> "rd";
ordinal(_)                               -> "th".

%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
mantissa(F) when is_float(F)   -> F - trunc(F);
mantissa(F) when is_integer(F) -> 0.0.
