-module(country_manager).

-author("Chris Whealy <chris.whealy@sap.com>").
-revision("Revision: 1.0.0").
-created("Date: 2018/03/02 09:22:03").
-created_by("chris.whealy@sap.com").

-export([
    init/1
  , start/1
]).


-include("../include/trace.hrl").
-include("../include/now.hrl").
-include("../include/utils.hrl").

-define(COUNTRY_SERVER_NAME(CountryCode), list_to_atom("country_server_" ++ string:lowercase(CountryCode))).

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
  Me = self(),
  process_flag(trap_exit, true),

  % Debug and network level trace flags
  put(trace, false),
  put(network_trace, false),

  ?TRACE("Starting country manager (~p) with ~w country servers at ~s",[self(), length(CountryList), format_datetime(?NOW)]),

  CountryServerList = [
    %% Build the country server list with all servers in the initial status of stopped
    %% Also fire off a HEAD request for each country ZIP file in order to discover its size
    (fun(ReturnPid, CC, CName, Cont, Trace) ->
       spawn(import_files, http_head_request,[ReturnPid, CC, ".zip", Trace]),

       #country_server{
         name         = ?COUNTRY_SERVER_NAME(CC)
       , country_name = CName
       , continent    = Cont
       , country_code = CC
       , status       = stopped
       , trace        = false
       }
     end)
    (Me, CountryCode, CountryName, Continent, get(trace))
    ||
    {CountryCode, CountryName, Continent} <- CountryList
  ],

  CountryServerList1 = wait_for_head_responses(CountryServerList),

  wait_for_msgs(lists:sort(fun(A,B) -> sort_country_server(A,B) end, CountryServerList1)).



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
          set_server_status(CountryServerList, CountryServerPid, crashed, see_logs, 0, [], 0)
      end;


    %% * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
    %% Messages from country servers reporting the how their startup sequence is progressing
    %%
    %% Initialisation
    {starting, init, CountryServer, StartTime} ->
      set_server_status(CountryServerList, CountryServer, starting, init, init, [], StartTime);

    %% File import (either full text file or internal FCP files)
    {starting, Substatus, CountryServer, progress, Progress} ->
      set_server_status(CountryServerList, CountryServer, starting, Substatus, Progress, [], undefined);

    %% Distribution of cities across city servers
    {starting, Substatus, CountryServer, new_child, Id} ->
      set_server_status(CountryServerList, CountryServer, starting, Substatus, complete, Id, undefined);

    {starting, country_file_download, CountryCode} ->
      set_server_status(CountryServerList, ?COUNTRY_SERVER_NAME(CountryCode), starting, country_file_download, 0, [], undefined);
  
    {starting, Substatus, CountryServer} ->
      set_server_status(CountryServerList, CountryServer, starting, Substatus, complete, [], undefined);
  
    %% Start up complete, server now running
    {started, running, CountryServer, CityCount, StartupComplete} ->
      ?TRACE("Country server ~p is up and running",[CountryServer]),
      set_server_running(CountryServerList, CountryServer, CityCount, StartupComplete);
    

    %% * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
    %% STATUS commands
    %%
    %% Status commmands from some request handler
    {cmd, status, RequestHandlerPid} when is_pid(RequestHandlerPid) ->
      ?TRACE("Server status requested from request handler ~p",[RequestHandlerPid]),
      RequestHandlerPid ! {country_server_list, CountryServerList, trace_on, get(trace), network_trace, get(network_trace)},
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
      RequestHandlerPid ! #cmd_response{from_server = country_manager, cmd = shutdown_all, status = ok},
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
               , trace        = false
               },

          RequestHandlerPid ! #cmd_response{from_server = country_manager, cmd = shutdown, status = ok, reason = T1},

          lists:keyreplace(CountryServer, #country_server.name, CountryServerList, T1)
      end;

    %% * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
    %% START messages
    %%
    %% (Re)start a specific country server
    {cmd, start, CountryCode, RequestHandlerPid} when is_pid(RequestHandlerPid) ->
      CountryServer = ?COUNTRY_SERVER_NAME(CountryCode),

      ?TRACE("Starting country server ~s",[CountryServer]),

      case whereis(CountryServer) of
        undefined ->
          T  = lists:keyfind(CountryServer, #country_server.name, CountryServerList),
          T1 = T#country_server{pid = country_server:init(CountryServer, T#country_server.trace, get(network_trace)), status = starting},

          RequestHandlerPid ! #cmd_response{from_server = country_manager, cmd = shutdown, status = ok, reason = T1},

          lists:keyreplace(CountryServer, #country_server.name, CountryServerList, T1);

        _Pid ->
          io:format("~p already started~n",[CountryServer]),
          CountryServerList
      end;

    %% * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
    %% Network level trace on/off command
    %%
    {cmd, network_trace, TraceState,  RequestHandlerPid} when is_pid(RequestHandlerPid) ->
      case TraceState of
        on  -> put(network_trace, true);
        off -> put(network_trace, false)
      end,

      RequestHandlerPid ! #cmd_response{from_server = country_manager, cmd = network_trace, status = ok},
      CountryServerList;


    %% * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
    %% Debug trace on/off commands
    %%
    %% Turn trace on/off for the country manager
    {cmd, trace, TraceState,  RequestHandlerPid} when is_pid(RequestHandlerPid) ->
      case TraceState of
        on  -> put(trace, true);
        off -> put(trace, false)
      end,

      RequestHandlerPid ! #cmd_response{from_server = country_manager, cmd = set_debug, status = ok},
      CountryServerList;

    %% Turn trace on/off for an individual country server
    %% The value bound to Trace must be either trace_on or trace_off
    {cmd, Trace, CC, RequestHandlerPid} when is_pid(RequestHandlerPid) ->
      CountryServerName = ?COUNTRY_SERVER_NAME(CC),
      S = lists:keyfind(CountryServerName, #country_server.name, CountryServerList),

      {Status, Reason, Trace1} = case S of
        false ->
          {error, no_such_country_server, trace_off};
        _ ->
          case S#country_server.substatus of
            running ->
              ?TRACE("Sending ~p to ~p",[Trace, CountryServerName]),
              S#country_server.name ! {cmd, Trace},
              {ok, undefined, Trace};
            _ ->
              {ok, undefined, Trace}
          end
        end,

      RequestHandlerPid ! #cmd_response{from_server = CountryServerName, cmd = Trace, status = Status, reason = Reason},

      %% Only update the CountryServerList if the trace state has changed
      case S#country_server.trace == trace_state_to_boolean(Trace1) of
        true  -> CountryServerList;
        false -> lists:keyreplace(CountryServerName, #country_server.name, CountryServerList, update_trace(S, Trace1))
      end
  end,

  wait_for_msgs(ServerStatusList1).


%% ---------------------------------------------------------------------------------------------------------------------
%% Update ZIP file size in a country server status record
update_zip_size(Rec, ZipSize) ->
  #country_server{
    name           = Rec#country_server.name
  , country_name   = Rec#country_server.country_name
  , continent      = Rec#country_server.continent
  , country_code   = Rec#country_server.country_code
  , pid            = Rec#country_server.pid
  , status         = Rec#country_server.status
  , substatus      = Rec#country_server.substatus
  , progress       = Rec#country_server.progress
  , children       = Rec#country_server.children
  , city_count     = Rec#country_server.city_count
  , started_at     = Rec#country_server.started_at
  , start_complete = Rec#country_server.start_complete
  , trace          = Rec#country_server.trace
  , mem_usage      = Rec#country_server.mem_usage
  , zip_size       = ZipSize
  }.


%% ---------------------------------------------------------------------------------------------------------------------
%% Update trace flag in a country server status record
update_trace(Rec, TraceState) ->
  #country_server{
    name           = Rec#country_server.name
  , country_name   = Rec#country_server.country_name
  , continent      = Rec#country_server.continent
  , country_code   = Rec#country_server.country_code
  , pid            = Rec#country_server.pid
  , status         = Rec#country_server.status
  , substatus      = Rec#country_server.substatus
  , progress       = Rec#country_server.progress
  , children       = Rec#country_server.children
  , city_count     = Rec#country_server.city_count
  , started_at     = Rec#country_server.started_at
  , start_complete = Rec#country_server.start_complete
  , trace          = trace_state_to_boolean(TraceState)
  , mem_usage      = Rec#country_server.mem_usage
  , zip_size       = Rec#country_server.zip_size
  }.


%% ---------------------------------------------------------------------------------------------------------------------
%% Update country server status record to "running"
set_server_running(CountryServerList, Name, CityCount, StartComplete) ->
  Rec = lists:keyfind(Name, #country_server.name, CountryServerList),

  ServerStatus = #country_server{
    name           = Rec#country_server.name
  , country_name   = Rec#country_server.country_name
  , continent      = Rec#country_server.continent
  , country_code   = Rec#country_server.country_code
  , pid            = Rec#country_server.pid
  , status         = started
  , substatus      = running
  , progress       = 100
  , children       = Rec#country_server.children
  , city_count     = CityCount
  , started_at     = Rec#country_server.started_at
  , start_complete = StartComplete
  , trace          = Rec#country_server.trace
  , mem_usage      = memory_usage(Rec#country_server.name)
  , zip_size       = Rec#country_server.zip_size
  },

  ?TRACE("Updated status is now ~p",[format_country_server_record(ServerStatus)]),

  lists:keyreplace(Name, #country_server.name, CountryServerList, ServerStatus).


%% ---------------------------------------------------------------------------------------------------------------------
%% Update status of a given server without time stamp
%% When a server crashes, we only get the Pid that used to exist
set_server_status(CountryServerList, Pid, crashed, Substatus, _, _, _) when is_pid(Pid) ->
  Rec = lists:keyfind(Pid, #country_server.pid, CountryServerList),

  ServerStatus = #country_server{
    name         = Rec#country_server.name
  , country_name = Rec#country_server.country_name
  , continent    = Rec#country_server.continent
  , country_code = Rec#country_server.country_code
  , status       = crashed
  , substatus    = Substatus
  , trace        = false
  , mem_usage    = 0
  , zip_size     = Rec#country_server.zip_size
  },

  lists:keyreplace(Pid, #country_server.pid, CountryServerList, ServerStatus);

%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
%% When a server is stopped (either due to no cities, or manual command, we still have its name available
set_server_status(CountryServerList, Name, stopped, Substatus, _, _, _) ->
  Rec = lists:keyfind(Name, #country_server.name, CountryServerList),

  ServerStatus = #country_server{
    name         = Name
  , country_name = Rec#country_server.country_name
  , continent    = Rec#country_server.continent
  , country_code = Rec#country_server.country_code
  , status       = stopped
  , substatus    = Substatus
  , trace        = false
  , mem_usage    = 0
  , zip_size     = Rec#country_server.zip_size
  },

  lists:keyreplace(Name, #country_server.name, CountryServerList, ServerStatus);

%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
%% Server identified by its name
set_server_status(CountryServerList, Name, Status, Substatus, Progress, Children, Time) ->
  Rec = lists:keyfind(Name, #country_server.name, CountryServerList),

  ServerStatus = #country_server{
    name         = Name
  , country_name = Rec#country_server.country_name
  , continent    = Rec#country_server.continent
  , country_code = Rec#country_server.country_code
  , pid          = whereis(Name)
  , status       = Status
  , substatus    = Substatus

  , progress = case Progress of
      init     -> 0;
      complete -> 100;
      P        -> Rec#country_server.progress + P
    end 

  , children = case is_list(Rec#country_server.children) of
      true  -> case Children of
                 [] -> Rec#country_server.children;
                 Id -> Rec#country_server.children ++ [Id]
               end;
      false -> case Children of
                [] -> [];
                Id -> [Id]
              end
    end

  , city_count = Rec#country_server.city_count

  , started_at = case Substatus of
      init -> Time;
      _    -> Rec#country_server.started_at
    end

  , start_complete = case Substatus of
      running -> Time;
      _       -> Rec#country_server.start_complete
    end

  , trace = Rec#country_server.trace

  , mem_usage = case Status of
      started -> memory_usage(Rec#country_server.name);
      _       -> 0
    end

  , zip_size  = Rec#country_server.zip_size
  },

  ?TRACE("Updated country_server is now ~p",[format_country_server_record(ServerStatus)]),

  lists:keyreplace(Name, #country_server.name, CountryServerList, ServerStatus).


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
%% Sort country server records into alphabetic order by continent
sort_country_server(A,B) when A#country_server.continent < B#country_server.continent ->
  true;

sort_country_server(A,B) when A#country_server.continent > B#country_server.continent ->
  false;

sort_country_server(A,B) ->
  A#country_server.country_name < B#country_server.country_name.



%% ---------------------------------------------------------------------------------------------------------------------
%% Wait for HTTP HEAD responses
wait_for_head_responses(CSList) -> wait_for_head_responses(CSList, []).

wait_for_head_responses([], Acc) -> Acc;

wait_for_head_responses(CSList, Acc) ->
  % ?TRACE("Waiting for HEAD response for ~p remaining countries",[length(CSList)]),

  {S, Filename1} = receive
    {ok, ContentLength, Filename, _Ext} ->
      % ?TRACE("Got content length ~w for ~s~s",[ContentLength,Filename,_Ext]),
      {update_zip_size(lists:keyfind(?COUNTRY_SERVER_NAME(Filename), #country_server.name, CSList), ContentLength), Filename};

    {error, {status_code, StatusCode}, Filename, Ext} ->
      io:format("Received HTTP status code ~w when requesting content length of ~s~s~n",[StatusCode, Filename, Ext]),
      {lists:keyfind(?COUNTRY_SERVER_NAME(Filename), #country_server.name, CSList), Filename};
      
    {error, {other, Reason}, Filename, Ext} ->
      io:format("Unable to determine ZIP file size for ~s~s. Reason: ~p~n",[Filename, Ext, Reason]),
      {lists:keyfind(?COUNTRY_SERVER_NAME(Filename), #country_server.name, CSList), Filename}
  end,

  wait_for_head_responses(lists:keydelete(?COUNTRY_SERVER_NAME(Filename1), #country_server.name, CSList), Acc ++ [S]).


%% ---------------------------------------------------------------------------------------------------------------------
%% Create printable format of a country_server record
format_country_server_record(R) ->
  lists:flatten([io_lib:format("~p = ~p, ",[K,V]) || {K,V} <- kv_country_server_record(R)]).


%% ---------------------------------------------------------------------------------------------------------------------
%% Stop all country servers
stop_all_country_servers(CountryServerList) ->
  [ stop_country_server(Svr) || Svr <- CountryServerList ].

%% Stop a started country server
stop_country_server(Svr) when Svr#country_server.status == started ->
  ?TRACE("Stopping ~p",[Svr#country_server.name]),
  Svr#country_server.name ! {cmd, shutdown},
  Svr#country_server{
    pid            = undefined
  , status         = stopped
  , substatus      = undefined
  , progress       = undefined
  , children       = undefined
  , started_at     = undefined
  , start_complete = undefined
  , trace          = false
  , mem_usage      = undefined
  };

%% Ignore country servers that have a status other than 'started'
stop_country_server(Svr) -> Svr.



%% ---------------------------------------------------------------------------------------------------------------------
%% Convert trace state to Boolean atom
trace_state_to_boolean(trace_on)  -> true;
trace_state_to_boolean(trace_off) -> false.
