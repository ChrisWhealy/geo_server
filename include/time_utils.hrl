%% -----------------------------------------------------------------------------
%%
%%                       T I M E   C A L C U L A T I O N S
%%
%% -----------------------------------------------------------------------------

%% Difference between two times where the times are in various formats:
%%  o  Two standard Erlang DateTimes
%%  o  Two custom Erlang DateTimes with additional microseconds part
%%  o  Two standard Erlang Timestamps
%%
%% The first parameter is always assumed to be later (bigger) than the second and
%% the result is truncated to the nearest millisecond

time_diff(_, undefined) -> 0;
time_diff(undefined, _) -> 0;

%% Two standard Erlang timestamps
time_diff({Mega1, Sec1, Micro1}, {Mega2, Sec2, Micro2}) ->
  (Mega1 - Mega2) + (Sec1 - Sec2) + make_millis(Micro1 - Micro2);

%% Two custom datetimes with additional microseconds part
time_diff({Date1,{H1,M1,S1,Micro1}}, {Date2,{H2,M2,S2,Micro2}}) ->
  SecDiff = calendar:datetime_to_gregorian_seconds({Date1,{H1,M1,S1}}) -
            calendar:datetime_to_gregorian_seconds({Date2,{H2,M2,S2}}),

  SecDiff + make_millis(Micro1 - Micro2);

%% Two standard Erlang datetimes
time_diff({Date1, Time1}, {Date2, Time2}) ->
  calendar:datetime_to_gregorian_seconds({Date1, Time1}) - calendar:datetime_to_gregorian_seconds({Date2, Time2}).

%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
%% Truncate microseconds to millesconds and return a decimal fraction
make_millis(Micro) -> (Micro div 1000) / 1000.

%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
%% Convert standard DateTime to Timestamp
to_timestamp({{YYYY,MM,DD},{H,M,S}}) ->
    TS = calendar:datetime_to_gregorian_seconds({{YYYY,MM,DD},{H,M,S}}) - 62167219200,
    {(TS div 1000000), (TS rem 1000000), 0};

%% Convert custom DateTime to Timestamp
to_timestamp({{YYYY,MM,DD},{H,M,S,Micro}}) ->
    TS = calendar:datetime_to_gregorian_seconds({{YYYY,MM,DD},{H,M,S}}) - 62167219200,
    {(TS div 1000000), (TS rem 1000000), Micro}.

%% -----------------------------------------------------------------------------
%%
%%          F O R M A T   T I M E   V A L U E S   A S   S T R I N G S
%%
%% -----------------------------------------------------------------------------

%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
%% Transform Erlang local time or timestamp to string
format_datetime(undefined) -> "Undefined";

% Standard Erlang timestamp format
format_datetime({Mega,Sec,Micro}) ->
  format_datetime(calendar:now_to_local_time({Mega,Sec,Micro}));

% Standard Erlang DateTime format
format_datetime({{YYYY,MM,DD},{H,M,S}}) ->
  io_lib:format("~w ~s ~s, ~2..0B:~2..0B:~2..0B", [YYYY, month_name(MM), format_day(DD), H, M, S]);

% Custom DateTime format with additional microseconds value
format_datetime({{YYYY,MM,DD},{H,M,S,Micro}}) ->
  S1 = S + make_millis(Micro),
  lists:flatten(io_lib:format("~w ~s ~s, ~2..0B:~2..0B:~6.3.0fs", [YYYY, month_name(MM), format_day(DD), H, M, S1])).

%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% Convert a number of seconds to string
format_seconds(0)   -> "0.0s";
format_seconds(0.0) -> "0.0s";

format_seconds(T) when T >= 3600 ->
  H  = trunc(T) div 3600,
  T1 = trunc(T) rem 3600,
  S  = (T1 rem 60) + mantissa(T), 

  io_lib:format("~w:~2..0B:~6.3.0fs",[H, (T1 div 60), S]);

format_seconds(T) when T >= 60 ->
  M = trunc(T) div 60,
  S = (trunc(T) rem 60) + mantissa(T),
  io_lib:format("~w:~6.3.0fs",[M, S]);

format_seconds(T) when is_integer(T) -> io_lib:format("~ws",[T]);
format_seconds(T)                    -> io_lib:format("~.3fs",[T]).

%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% Month number to name conversion
month_name(MM) -> element(MM, {"Jan", "Feb", "Mar", "Apr", "May", "Jun",
                               "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"}).

%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% Add ordinal string to a day number
format_day(D) -> integer_to_list(D) ++ ordinal(D).

%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
ordinal(D) when D == 1; D == 21; D == 31 -> "st";
ordinal(D) when D == 2; D == 22          -> "nd";
ordinal(D) when D == 3; D == 23          -> "rd";
ordinal(_)                               -> "th".

%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
mantissa(F) when is_float(F)   -> F - trunc(F);
mantissa(F) when is_integer(F) -> 0.0.
