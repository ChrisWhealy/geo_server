-module(country_manager).

-author("Chris Whealy <chris.whealy@sap.com>").
-revision("Revision: 1.0.0").
-created("Date: 2018/03/02 09:22:03").
-created_by("chris.whealy@sap.com").

-export([
    init/1
  , start/1
]).

%% Put -defines before -includes because COUNTRY_SERVER_NAME is needed in manage_server_status.hrl
-define(COUNTRY_SERVER_NAME(CountryCode), list_to_atom("country_server_" ++ string:lowercase(CountryCode))).
-define(RETRY_LIMIT, 3).
-define(RETRY_WAIT,  5000).

%% Include record definitions first
-include("../include/rec_cmd_response.hrl").
-include("../include/rec_country_server.hrl").

%% Include various utilities
-include("../include/trace.hrl").
-include("../include/now.hrl").
-include("../include/utils_time.hrl").
-include("../include/utils_format_time.hrl").
-include("../include/utils_memory_usage.hrl").
-include("../include/utils_json_transform.hrl").
-include("../include/utils_server_status.hrl").


%% =====================================================================================================================
%%
%%                                                 P U B L I C   A P I
%%
%% =====================================================================================================================

%% ---------------------------------------------------------------------------------------------------------------------
%% Initialise the country manager
init(CountryList) ->
  case whereis(?MODULE) of
    undefined -> register(?MODULE, spawn(?MODULE, start, [CountryList]));
    _         -> already_registered
end,

{ok, whereis(?MODULE), []}.


%% ---------------------------------------------------------------------------------------------------------------------
%% Start the country manager
%%
%% This process is responsible for starting and then managing each of the individual country servers
start(CountryList) ->
  process_flag(trap_exit, true),

  % Debug trace flag starts in the off position.  Can be switched on from the admin screen
  put(trace, false),

  ?TRACE("Starting country manager (~p) with ~w country servers at ~s",[self(), length(CountryList), format_datetime(?NOW)]),
  CountryServerList  = initialise_country_server_list(CountryList),
  CountryServerList1 = wait_for_head_responses(fetch_zip_file_sizes(CountryServerList)),

  %% The default sort order is by descending ZIP size
  wait_for_msgs(lists:sort(fun(A,B) -> sort_servers_by(zip_size, A, B) end, CountryServerList1)).



%% =====================================================================================================================
%%
%%                                                P R I V A T E   A P I
%%
%% =====================================================================================================================

%% ---------------------------------------------------------------------------------------------------------------------
%% Country manager receive loop
wait_for_msgs(CountryServerList) ->
  %% If the CountryServerList is emmpty, then either none of the country servers have started yet, or we are shutting down
  case CountryServerList of
    [] ->
      %% Are we shutting down?
      case get(shutdown) of
        true -> exit(normal);
        _    -> ok
      end;

    _ -> ok
  end,

  ServerStatusList1 = receive
    %% * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
    %% Messages from processes that have crashed
    %%
    {'EXIT', CountryServerPid, Reason} ->
      case Reason of
        {stopped, DeadServerName} ->
          io:format("Country server ~p was stopped~n", [DeadServerName]),
          set_server_status(CountryServerList, DeadServerName, stopped, undefined, 0, [], undefined);

        {no_cities, DeadServerName} ->
          io:format("Country server ~p terminated: no_cities~n", [DeadServerName]),
          set_server_status(CountryServerList, DeadServerName, stopped, no_cities, 0, [], undefined);

        {country_file_error, ReasonStr} ->
          io:format("Error reading country file ~s~n", [ReasonStr]),
          set_server_status(CountryServerList, CountryServerPid, crashed, country_file_error, 0, [], undefined);

        {fca_country_file_error, Reason} ->
          io:format("Error reading internal FCA file. ~p~n", [Reason]),
          set_server_status(CountryServerList, CountryServerPid, crashed, fca_country_file_error, 0, [], undefined);

        {fcp_country_file_error, Reason} ->
          io:format("Error reading internal FCP file. ~p~n", [Reason]),
          set_server_status(CountryServerList, CountryServerPid, crashed, fcp_country_file_error, 0, [], undefined);

        {country_zip_file_error, ZipFile, Reason} ->
          io:format("Error unzipping file ~s: ~p~n", [ZipFile, Reason]),
          set_server_status(CountryServerList, CountryServerPid, crashed, country_zip_file_error, 0, [], undefined);

        _ ->
          DeadServerName = get_server_name_from_pid(CountryServerPid, CountryServerList),
          io:format("Country server ~p (~p) terminated for reason '~p'~n", [DeadServerName, CountryServerPid, Reason]),

          Reason1 = case Reason of
            {retry_limit_exceeded, _} -> retry_limit_exceeded;
            _                         -> see_logs
          end,

          set_server_status(CountryServerList, CountryServerPid, crashed, Reason1, 0, [], undefined)
      end;


    %% * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
    %% Messages from country servers reporting the how their startup sequence is progressing
    %%
    %% Initialisation
    {starting, country_file_download, CountryCode} ->
      set_server_status(CountryServerList, ?COUNTRY_SERVER_NAME(CountryCode), starting, country_file_download, 0, [], undefined);
    
    {starting, Substatus, CountryServer} ->
      set_server_status(CountryServerList, CountryServer, starting, Substatus, complete, [], undefined);

    {starting, init, CountryServer, StartTime} ->
      set_server_status(CountryServerList, CountryServer, starting, init, init, [], StartTime);

    {starting, Substatus, CountryServer, Id} ->
      set_server_status(CountryServerList, CountryServer, starting, Substatus, complete, Id, undefined);
  
    {starting, Substatus, CountryServer, progress, Progress} ->
      set_server_status(CountryServerList, CountryServer, starting, Substatus, Progress, [], undefined);


    %% * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
    %% Sort messages
    %%
    {sort, Dir, ColName, RequestHandlerPid} when is_pid(RequestHandlerPid) ->
      ?TRACE("Sorting server status records by ~p ~p",[ColName, Dir]),
      CountryServerList1 = lists:sort(fun(A,B) ->
          case Dir of
            ascending -> sort_servers_by(ColName, B, A);
            _         -> sort_servers_by(ColName, A, B)
          end
        end,
        CountryServerList),
      RequestHandlerPid ! CountryServerList1,
      CountryServerList1;


    %% * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
    %% Start up complete, server now running
    %%
    {started, running, CountryServer, CityCount, StartupComplete} ->
      ?TRACE("Country server ~p is up and running",[CountryServer]),
      set_server_running(CountryServerList, CountryServer, CityCount, StartupComplete);


    %% * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
    %% STATUS commands
    %%
    %% Status commmands from some request handler
    {cmd, status, RequestHandlerPid} when is_pid(RequestHandlerPid) ->
      ?TRACE("Server status requested from request handler ~p",[RequestHandlerPid]),
      RequestHandlerPid ! {country_server_list, CountryServerList, trace_on, get(trace)},
      CountryServerList;

    {cmd, status, started, RequestHandlerPid} when is_pid(RequestHandlerPid) ->
      ?TRACE("List of started servers requested by ~p",[RequestHandlerPid]),
      RequestHandlerPid ! {started_servers, started_servers(CountryServerList)},
      CountryServerList;


    %% * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
    %% SHUTDOWN messages
    %%
    %% Shutdown all country servers, but keep the country manager up
    {cmd, shutdown_all, RequestHandlerPid} when is_pid(RequestHandlerPid) ->
      ?TRACE("Shutdown all country servers but keep country_manager running"),
      put(shutdown, false),
      CountryServerList1 = stop_all_country_servers(CountryServerList),
      RequestHandlerPid ! #cmd_response{from_server = country_manager, cmd = shutdown_all, status = ok, payload = CountryServerList1},
      CountryServerList1;

    %% Shutdown all country servers and then shutdown the country manager
    {cmd, terminate, RequestHandlerPid} when is_pid(RequestHandlerPid) ->
      %% Set process dictionary flag to indicate country_manager shutdown
      put(shutdown, true),
      ?TRACE("Shutdown all country servers then shutdown the country manager"),
      CountryServerList1 = stop_all_country_servers(CountryServerList),
      RequestHandlerPid ! #cmd_response{from_server = country_manager, cmd = terminate, status = goodbye},
      CountryServerList1;

    %% Shutdown a specific country server
    {cmd, shutdown, CountryCode, RequestHandlerPid} when is_pid(RequestHandlerPid) ->
      CountryServer = ?COUNTRY_SERVER_NAME(CountryCode),
      ?TRACE("Shutdown country server ~p",[CountryServer]),

      case whereis(CountryServer) of
        undefined ->
          io:format("~p not started~n",[CountryServer]),
          CountryServerList;

        _Pid ->
          CountryServer ! {cmd, shutdown},

          T  = lists:keyfind(CountryServer, #country_server.name, CountryServerList),
          T1 = #country_server{
                 name         = T#country_server.name
               , country_name = T#country_server.country_name
               , continent    = T#country_server.continent
               , country_code = T#country_server.country_code
               , status       = stopped
               , trace        = T#country_server.trace
               , zip_size     = T#country_server.zip_size
               },

          RequestHandlerPid ! #cmd_response{from_server = country_manager, cmd = shutdown, status = ok, payload = T1},

          lists:keyreplace(CountryServer, #country_server.name, CountryServerList, T1)
      end;


    %% * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
    %% START messages
    %%
    %% (Re)start a specific country server
    {cmd, start, CountryCode, RequestHandlerPid} when is_pid(RequestHandlerPid) ->
      CountryServer = ?COUNTRY_SERVER_NAME(CountryCode),

      {CountryServerList1, ResponseRec} = case whereis(CountryServer) of
        undefined ->
          T = lists:keyfind(CountryServer, #country_server.name, CountryServerList),

          %% Did the server lookup work?
          case T of
            false ->
              io:format("Error: Lookup of ~p in CountryServerList failed~n",[CountryServer]),
              {CountryServerList,
               #cmd_response{from_server = country_manager, cmd = start, status = error, reason = country_server_not_found}};
            _ ->
              T1 = start_country_server(T),
              {lists:keyreplace(CountryServer, #country_server.name, CountryServerList, T1),
               #cmd_response{from_server = country_manager, cmd = start, status = ok, payload = T1}}
          end;

        _Pid ->
          {CountryServerList,
           #cmd_response{from_server = country_manager, cmd = start, status = error, reason = already_started}}
      end,

      RequestHandlerPid ! ResponseRec,
      CountryServerList1;

    %% Start all the country servers at once - hopefully, this worn't explode when running in cloud foundry
    {cmd, start_all, RequestHandlerPid} when is_pid(RequestHandlerPid) ->
      ?TRACE("Starting all country servers"),
      CountryServerList1 = start_all_country_servers(CountryServerList),
      RequestHandlerPid ! #cmd_response{from_server = country_manager, cmd = start_all, status = ok, payload = CountryServerList1},
      CountryServerList1;


    %% * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
    %% Reset a single country server that has crashed
    %%
    {cmd, reset, CC, RequestHandlerPid} when is_pid(RequestHandlerPid) ->
      CountryServerName = ?COUNTRY_SERVER_NAME(CC),
      S = lists:keyfind(CountryServerName, #country_server.name, CountryServerList),

      case S#country_server.status of
        crashed ->
          ?TRACE("Reseting crashed server ~p",[CountryServerName]),
          S1 = reset_crashed_server(S),
          RequestHandlerPid ! #cmd_response{from_server = CountryServerName, cmd = reset, status = ok, payload = S1},
          lists:keyreplace(CountryServerName, #country_server.name, CountryServerList, S1);

        _ ->
          RequestHandlerPid ! #cmd_response{from_server = CountryServerName, cmd = reset, status = error, reason = server_not_crashed},
          CountryServerList
      end;

  

    %% * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
    %% Reset all crashed country servers
    %%
    {cmd, reset_all, RequestHandlerPid} when is_pid(RequestHandlerPid) ->
      CountryServerList1 = [
        (fun(S) ->
          case S#country_server.status of
            crashed -> reset_crashed_server(S);
            _       -> S
          end
        end)(Svr)
        || Svr <- CountryServerList
        ],
      
      RequestHandlerPid ! #cmd_response{from_server = country_manager, cmd = reset_all, status = ok, payload = CountryServerList1},
      CountryServerList1;

  

    %% * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
    %% Debug trace on/off commands
    %%
    %% Turn trace on/off for the country manager
    {cmd, trace, TraceState, RequestHandlerPid} when is_pid(RequestHandlerPid) ->
      case TraceState of
        on  -> put(trace, true);
        off -> put(trace, false)
      end,

      RequestHandlerPid ! #cmd_response{from_server = country_manager, cmd = trace, status = ok, payload = TraceState},
      CountryServerList;

    %% Turn trace on/off for an individual country server
    %% The value bound to Trace must be either of the atoms 'trace_on' or 'trace_off'
    {cmd, Trace, CC, RequestHandlerPid} when is_pid(RequestHandlerPid) ->
      CountryServerName = ?COUNTRY_SERVER_NAME(CC),
      S = lists:keyfind(CountryServerName, #country_server.name, CountryServerList),

      {Status, Reason} = case S of
        false -> {error, no_such_country_server};
        _     -> case S#country_server.substatus of
                   running -> S#country_server.name ! {cmd, Trace};
                   _       -> ok
                 end,

                 {ok, undefined}
      end,

      RequestHandlerPid ! #cmd_response{from_server = CountryServerName, cmd = Trace, status = Status, reason = Reason},

      %% Only update the CountryServerList if the trace state has changed
      case S#country_server.trace == trace_state_to_boolean(Trace) of
        true  -> CountryServerList;
        false -> lists:keyreplace(CountryServerName, #country_server.name, CountryServerList, update_trace(S, Trace))
      end
  end,

  wait_for_msgs(ServerStatusList1).



%% ---------------------------------------------------------------------------------------------------------------------
%% List servers by status
started_servers(CountryServerList)  -> filter_by_status(CountryServerList, started).
starting_servers(CountryServerList) -> filter_by_status(CountryServerList, starting).

%% ---------------------------------------------------------------------------------------------------------------------
%% Filter server list by status
filter_by_status(CountryServerList, Status) ->
  filter_by_status(CountryServerList, Status, []).

filter_by_status([], _Status, Acc)        -> Acc;
filter_by_status([S | Rest], Status, Acc) ->
  filter_by_status(Rest, Status, Acc ++ case S#country_server.status of Status -> [S]; _ -> [] end).
  
%% ---------------------------------------------------------------------------------------------------------------------
%% Get country server name from pid.  Returns an atom
get_server_name_from_pid(CountryServerPid, CountryServerList) ->
  case lists:keyfind(CountryServerPid, #country_server.pid, CountryServerList) of
    false -> unknown_pid;
    Rec   -> Rec#country_server.name
end.


%% ---------------------------------------------------------------------------------------------------------------------
%% Convert the initial list of countries into a list of initialised country_server records with status stopped
initialise_country_server_list(CountryList) ->
  [ set_server_init(CC, Name, Cont) || {CC, Name, Cont} <- CountryList ].


%% ---------------------------------------------------------------------------------------------------------------------
%% Determine the ZIP file sizes for all the countries in the server list
fetch_zip_file_sizes(CountryServerList) ->
  lists:foreach(
    fun(Svr) ->
      spawn(import_files, http_head_request, [Svr#country_server.country_code, ".zip", get(trace)])
    end,
    CountryServerList
  ),

  CountryServerList.


%% ---------------------------------------------------------------------------------------------------------------------
%% Wait for HTTP HEAD responses
wait_for_head_responses(CSList) -> wait_for_head_responses(CSList, [], [], 0).

%% All HEAD responses received and the RetryList is empty
wait_for_head_responses([], Acc, [], _) -> Acc;

%% Retry limit exceeded
wait_for_head_responses([], Acc, RetryList, RetryCount) when RetryCount >= ?RETRY_LIMIT ->
  %% Some HEAD requests just won't work today... This is annoying, but not fatal
  %% Set the zip file sizes to zero for all the remaining countries
  Acc ++ [ update_zip_size(Svr, 0) || Svr <- RetryList ];

%% Most of the HEAD responses have been received, but a few failed
wait_for_head_responses([], Acc, RetryList, RetryCount) ->
  %% Wait for an arbitrary amount of time before retrying HEAD requests
  io:format("Retrying HEAD requests for ~w countries~n",[length(RetryList)]),

  receive
  after ?RETRY_WAIT ->
    wait_for_head_responses(fetch_zip_file_sizes(RetryList), Acc, [], RetryCount + 1)
  end;

%% Haven't finished sending off the HEAD requests
wait_for_head_responses(CSList, Acc, RetryList, RetryCount) ->
  % ?TRACE("Waiting for HEAD response for ~p remaining countries",[length(CSList)]),
  
  {CSList1, Acc1, RetryList1} = receive
    {Tag, Part2, Filename, Ext} ->
      %% Find the server to which this message belongs in the CountryServerList
      Svr = lists:keyfind(?COUNTRY_SERVER_NAME(Filename), #country_server.name, CSList),

      %% Did the HEAD request succeed?
      case Tag of
        % Yup, so in this case Part2 of the message tuple will contain the file's content length
        ok ->
          %% Remove the current server from the CSlist, add it to the accumulator, and the RetryList remains unmodified
          {lists:keydelete(?COUNTRY_SERVER_NAME(Filename), #country_server.name, CSList),
           Acc ++ [update_zip_size(Svr, Part2)],
           RetryList};

        % Nope, so something went wrong with the HEAD request
        error ->
          %% Write error message to the console
          case Part2 of
            {status_code, StatusCode} -> io:format("Received HTTP status code ~w when requesting content length of ~s~s~n",[StatusCode, Filename, Ext]);
            {other, Reason}           -> io:format("Unable to determine ZIP file size for ~s~s. Reason: ~p~n",[Filename, Ext, Reason])
          end,

          %% Remove the current server from the CSlist, the accumulator remains unmodieid, and the failed server is
          %% added to the RetryList
          {lists:keydelete(?COUNTRY_SERVER_NAME(Filename), #country_server.name, CSList),
           Acc,
           RetryList ++ [Svr]}
      end
  end,

  wait_for_head_responses(CSList1, Acc1, RetryList1, RetryCount).


%% ---------------------------------------------------------------------------------------------------------------------
%% Start all country servers
start_all_country_servers(CountryServerList) ->
  [ start_country_server(Svr) || Svr <- CountryServerList ].

%% Start a country server
start_country_server(Svr) when Svr#country_server.status == stopped ->
  CountryServer = ?COUNTRY_SERVER_NAME(Svr#country_server.country_code),
  ?TRACE("Starting country server ~s",[CountryServer]),

  case whereis(CountryServer) of
    undefined -> Svr#country_server{pid = country_server:init(CountryServer, Svr#country_server.trace), status = starting};
    _Pid      -> Svr
  end;

%% Ignore any country server that has a status other than 'stopped'
start_country_server(Svr) -> Svr.

%% ---------------------------------------------------------------------------------------------------------------------
%% Stop all country servers
stop_all_country_servers(CountryServerList) ->
  [ stop_country_server(Svr) || Svr <- CountryServerList ].

%% Stop a country server
stop_country_server(Svr) when Svr#country_server.status == started ->
  ?TRACE("Stopping ~p",[Svr#country_server.name]),
  Svr#country_server.name ! {cmd, shutdown},
  set_server_stopped(Svr);

%% Ignore any country server that has a status other than 'started'
stop_country_server(Svr) -> Svr.



%% ---------------------------------------------------------------------------------------------------------------------
%% Convert trace state atom to Boolean atom
trace_state_to_boolean(trace_on)  -> true;
trace_state_to_boolean(trace_off) -> false.


%% =====================================================================================================================
%%
%%                                    S O R T   S E R V E R   S T A T U S   L I S T
%%
%% =====================================================================================================================

%% ---------------------------------------------------------------------------------------------------------------------
%% Sort server status records by various column headings
sort_servers_by(continent,    A, B) -> simple_sort(A#country_server.continent,    B#country_server.continent);
sort_servers_by(country_name, A, B) -> simple_sort(A#country_server.country_name, B#country_server.country_name);
sort_servers_by(country_code, A, B) -> simple_sort(A#country_server.country_code, B#country_server.country_code);
sort_servers_by(city_count,   A, B) -> simple_sort(A#country_server.city_count,   B#country_server.city_count);
sort_servers_by(mem_usage,    A, B) -> simple_sort(A#country_server.mem_usage,    B#country_server.mem_usage);
sort_servers_by(zip_size,     A, B) -> simple_sort(A#country_server.zip_size,     B#country_server.zip_size);
sort_servers_by(startup_time, A, B) -> simple_sort(A#country_server.startup_time, B#country_server.startup_time).

%% By default, all atoms are greater than integers. Need to reverse this.
simple_sort(A, _) when is_atom(A) -> false;
simple_sort(_, B) when is_atom(B) -> true;
simple_sort(A, B)                 -> A > B.


