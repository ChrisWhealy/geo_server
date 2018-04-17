
-include("../include/rec_country_server.hrl").

%% =====================================================================================================================
%% Format size in binary units Kb, Mb or Gb
%% =====================================================================================================================
kb() -> 1024.
mb() -> kb() * 1024.
gb() -> mb() * 1024.

format_as_binary_units(N) -> lists:flatten(format_as_binary_units_int(N)).

format_as_binary_units_int(undefined)             -> io_lib:format("~w bytes",[0]);
format_as_binary_units_int(N) when N < 1024       -> io_lib:format("~w bytes",[N]);
format_as_binary_units_int(N) when N < 1048576    -> format_as_binary_units_int(N, kb(), "Kb");
format_as_binary_units_int(N) when N < 1073741824 -> format_as_binary_units_int(N, mb(), "Mb");
format_as_binary_units_int(N) when is_integer(N)  -> format_as_binary_units_int(N, gb(), "Gb").

format_as_binary_units_int(N, Unit, UnitStr) ->
  WholeUnits = N div Unit,
  Rem = (N - (WholeUnits * Unit)) / Unit,
  io_lib:format("~.2f ~s",[WholeUnits + Rem, UnitStr]).



%% =====================================================================================================================
%% Convert various datatypes to JavaScript-ready binary strings
%% =====================================================================================================================

%% ---------------------------------------------------------------------------------------------------------------------
%% Convert all values to binary ensuring that string values are delimited by double quotes

%% Erlang 'undefined' become JavaScript 'null'
as_binary(undefined) -> <<"null">>;

%% Assume existing binary values are already formatted correctly (might be dangerous!)
as_binary(V) when is_binary(V) -> V;

%% Numbers do not need to be delimited
as_binary(V) when is_integer(V); is_float(V) -> list_to_binary(io_lib:format("~p",[V]));

as_binary(V) when is_atom(V) -> list_to_binary([<<"\"">>, atom_to_list(V), <<"\"">>]);
as_binary(V) when is_pid(V)  -> list_to_binary([<<"\"">>, pid_to_list(V), <<"\"">>]);

%% For strings, io_lib:format adds double quotes automatically
as_binary(V) -> list_to_binary(lists:flatten(io_lib:format("~p",[V]))).



%% =====================================================================================================================
%% Convert a list to a JSON array
%% =====================================================================================================================

%% ---------------------------------------------------------------------------------------------------------------------
%% Make a JSON array from the supplied list of values
%% Each element in Vs must be quote delimited string, object or another array
make_json_array([])                  -> <<"[]">>;
make_json_array(Vs) when is_list(Vs) -> list_to_binary([<<"[">>, lists:join(<<",">>,Vs), <<"]">>]);
make_json_array(V)                   -> list_to_binary([<<"[">>, V,                      <<"]">>]).

%% ---------------------------------------------------------------------------------------------------------------------
%% Make a JSON property from the supplied name and value
make_json_prop(PropName, PropValue) -> list_to_binary(lists:flatten([as_binary(PropName), <<":">>, as_binary(PropValue)])).
  

%% ---------------------------------------------------------------------------------------------------------------------
%% Make a JSON object from the supplied list of properties
%% Each property must already be in the form <<"\"prop_name\":"\value\"">>
make_json_obj([]) -> <<"{}">>;

make_json_obj(Props) when is_list(Props) -> list_to_binary([<<"{">>, lists:flatten(lists:join(<<",">>, Props)), <<"}">>]);
make_json_obj(Prop)                      -> list_to_binary([<<"{">>, Prop, <<"}">>]).



%% =====================================================================================================================
%%
%%        Convert record into a simple JSON object
%%
%% =====================================================================================================================

record_to_json(country_server, Rec) ->
  %% Format all internal values in the record
  R1 = #country_server{
      name           = Rec#country_server.name
    , country_name   = Rec#country_server.country_name
    , continent      = Rec#country_server.continent
    , country_code   = Rec#country_server.country_code
    , pid            = Rec#country_server.pid
    , status         = Rec#country_server.status
    , substatus      = Rec#country_server.substatus
    , progress       = Rec#country_server.progress
    , children       = len(Rec#country_server.children)
    , started_at     = format_datetime(Rec#country_server.started_at)
    , start_complete = format_seconds(time_diff(Rec#country_server.start_complete, Rec#country_server.started_at))
    , mem_usage      = format_as_binary_units(Rec#country_server.mem_usage)
    , zip_size       = format_as_binary_units(Rec#country_server.zip_size)
    },

  kv_to_json(kv_country_server_record(R1)).


%% Create a KV list from a country_server record and a record instance
kv_country_server_record(R) -> lists:zip(record_info(fields, country_server), tl(tuple_to_list(R))).

%% Assemble the JSON object from the KV list
kv_to_json(KVList) -> kv_to_json(KVList, []).

kv_to_json([], Acc)             -> make_json_obj(Acc);
kv_to_json([{K,V} | Rest], Acc) -> kv_to_json(Rest, lists:append(Acc, [make_json_prop(K, V)])).


%% ---------------------------------------------------------------------------------------------------------------------
%% Get the length of a list that might be undefined
len(undefined)         -> undefined;
len(L) when is_list(L) -> length(L).



%% =====================================================================================================================
%%
%%                                           T I M E   C A L C U L A T I O N S
%%
%% =====================================================================================================================

%% Difference between two times where the times are in various formats:
%%  o  Two standard Erlang DateTimes
%%  o  Two custom Erlang DateTimes with additional microseconds part
%%  o  Two standard Erlang Timestamps
%%
%% The first parameter is always assumed to be later (bigger) than the second and
%% the result is truncated to the nearest millisecond

time_diff(_, undefined) -> undefined;
time_diff(undefined, _) -> undefined;

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

%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
%% Truncate microseconds to millesconds and return a decimal fraction
make_millis(Micro) -> (Micro div 1000) / 1000.

%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
%% Convert standard DateTime to Timestamp
to_timestamp({{YYYY,MM,DD},{H,M,S}}) ->
  TS = calendar:datetime_to_gregorian_seconds({{YYYY,MM,DD},{H,M,S}}) - 62167219200,
  {(TS div 1000000), (TS rem 1000000), 0};

%% Convert custom DateTime to Timestamp
to_timestamp({{YYYY,MM,DD},{H,M,S,Micro}}) ->
  TS = calendar:datetime_to_gregorian_seconds({{YYYY,MM,DD},{H,M,S}}) - 62167219200,
  {(TS div 1000000), (TS rem 1000000), Micro}.



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
