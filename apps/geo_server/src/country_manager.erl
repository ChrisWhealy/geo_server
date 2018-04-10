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
-include("../include/time_utils.hrl").
-include("../include/rec_country_server.hrl").

-define(COUNTRY_SERVER_NAME(CountryCode), list_to_atom("country_server_" ++ string:lowercase(CountryCode))).

%% -----------------------------------------------------------------------------
%%                             P U B L I C   A P I
%% -----------------------------------------------------------------------------

%% -----------------------------------------------------------------------------
%% Initialise the country manager
init(CountryList) ->
  case whereis(?MODULE) of
    undefined -> register(?MODULE, spawn(?MODULE, start, [CountryList]));
    _         -> already_registered
  end,

  {ok, whereis(?MODULE), []}.


%% -----------------------------------------------------------------------------
%% Start the country manager
%%
%% This process is responsible for starting and then managing each of the
%% individual country servers
start(CountryList) ->
  Me = self(),
  process_flag(trap_exit, true),

  % Debug trace
  put(trace, false),

  ?TRACE("Country manager (~p) starting ~w country servers at ~s",[self(), length(CountryList), format_datetime(?NOW)]),

  %% In this list comprehension, build the country server list with all servers
  %% in the initial status of stopped and also fire off a HEAD request for each
  %% country ZIP file in order to discover its size
  CountryServerList = [
    (fun(ReturnPid, CC, CName, Cont) ->
       spawn(import_files, http_head_request,[ReturnPid, CC, ".zip", get(trace)]),

       #country_server{
         name         = ?COUNTRY_SERVER_NAME(CC)
       , country_name = CName
       , continent    = Cont
       , status       = stopped
       }
     end)
    (Me, CountryCode, CountryName, Continent)
    ||
    {CountryCode, CountryName, Continent} <- CountryList
  ],

  CountryServerList1 = wait_for_head_responses(CountryServerList),

  wait_for_msgs(lists:sort(fun(A,B) -> sort_country_server(A,B) end, CountryServerList1)).


%% -----------------------------------------------------------------------------
%% Country manager receive loop
wait_for_msgs(CountryServerList) ->
  %% If the CountryServerList is emmpty, then either none of the country servers
  %% have started yet, or we are shutting down
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
    %% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    %% Termination messages from country servers
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


    %% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    %% Status messages from country servers

    %% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    %% Initialisation
    {starting, init, CountryServer, StartTime} ->
      set_server_status(CountryServerList, CountryServer, starting, init, init, [], StartTime);

    %% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    %% File import (either full text file or internal FCA/FCP files)
    {starting, Substatus, CountryServer, progress, Progress} ->
      set_server_status(CountryServerList, CountryServer, starting, Substatus, Progress, [], undefined);

    %% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    %% Distribution of cities across city servers
    {starting, Substatus, CountryServer, new_child, Id} ->
      set_server_status(CountryServerList, CountryServer, starting, Substatus, complete, Id, undefined);

    {starting, country_file_download, CountryCode} ->
      set_server_status(CountryServerList, ?COUNTRY_SERVER_NAME(CountryCode), starting, country_file_download, 0, [], undefined);
  
    {starting, Substatus, CountryServer} ->
      set_server_status(CountryServerList, CountryServer, starting, Substatus, complete, [], undefined);
  
    %% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    %% Start up complete, server now running
    {started, running, CountryServer, StartupComplete} ->
      set_server_status(CountryServerList, CountryServer, started, running, complete, [], StartupComplete);
    

    %% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    %% Turn trace on/off just for the country manager
    {cmd, trace, on}  -> put(trace, true),  CountryServerList;
    {cmd, trace, off} -> put(trace, false), CountryServerList;
  
    %% Turn trace on/off for the country manager and its country servers
    {cmd, trace_on, all}  ->
      put(trace, true),
      lists:foreach(fun(S) -> S#country_server.name ! {cmd, trace_on} end, CountryServerList),
      CountryServerList;
    
    {cmd, trace_off, all} ->
      put(trace, false),
      lists:foreach(fun(S) -> S#country_server.name ! {cmd, trace_off} end, CountryServerList),
      CountryServerList;

    %% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    %% Status commmands either from the console or some request handler
    {cmd, status} ->
      format_server_status_list(CountryServerList, all),
      CountryServerList;

    {cmd, status, starting} ->
      ?TRACE("Status of starting servers requested from console"),
      format_server_status_list(starting_servers(CountryServerList), starting),
      CountryServerList;

    {cmd, status, started} ->
      ?TRACE("Status of started servers requested from console"),
      format_server_status_list(started_servers(CountryServerList), started),
      CountryServerList;
  
    {cmd, status, RequestHandlerPid} when is_pid(RequestHandlerPid) ->
      ?TRACE("Sending server status to request handler ~p",[RequestHandlerPid]),
      RequestHandlerPid ! {country_server_list, CountryServerList, trace_on, get(trace)},
      CountryServerList;

    {cmd, status, CountryCode} when length(CountryCode) == 2 ->
      ?TRACE("Status of server ~s requested from console",[CountryCode]),
      ThisCountryServer = ?COUNTRY_SERVER_NAME(CountryCode),

      case whereis(ThisCountryServer) of
        undefined ->
          io:format("No server found for country code ~s~n",[CountryCode]);

        _ ->
          Rec = lists:keyfind(ThisCountryServer, #country_server.name, CountryServerList),
          format_server_status_list([Rec], ThisCountryServer)
      end,

      CountryServerList;

    {cmd, status, started, RequestHandlerPid} ->
      ?TRACE("List of started servers requested by ~p",[RequestHandlerPid]),
      RequestHandlerPid ! {started_servers, started_servers(CountryServerList)},
      CountryServerList;

    %% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    %% Shutdown all country servers, then shut down country manager
    {cmd, shutdown} ->
      %% Set process dictionary flag
      put(shutdown, true),
      ?TRACE("Shutdown country manager"),
      [ Svr#country_server.name ! {cmd, shutdown} || Svr <- CountryServerList ],
      CountryServerList;

    %% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    %% Shutdown a specific country server
    {cmd, shutdown, CountryServer} ->
      ?TRACE("Shutdown country server ~p",[CountryServer]),

      case whereis(CountryServer) of
        undefined -> io:format("Server shutdown failed: ~p not started~n",[CountryServer]);
        _Pid      -> CountryServer ! {cmd, shutdown}
      end,

      CountryServerList;

    %% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    %% (Re)start specific country server
    {cmd, start, CountryServer} ->
      ?TRACE("Start country server ~s",[CountryServer]),

      case whereis(CountryServer) of
        undefined ->
          T  = lists:keyfind(CountryServer, #country_server.name, CountryServerList),
          T1 = T#country_server{pid = country_server:init(CountryServer), status = starting},
          lists:keyreplace(CountryServer, #country_server.name, CountryServerList, T1);

        _Pid ->
          io:format("Server start failed: ~p already started~n",[CountryServer]),
          CountryServerList
      end
  end,

  wait_for_msgs(ServerStatusList1).



%% -----------------------------------------------------------------------------
%%                           P R I V A T E   A P I
%% -----------------------------------------------------------------------------

%% -----------------------------------------------------------------------------
%% Update ZIP file size in a country server status record
update_zip_size(Rec, ZipSize) ->
  #country_server{
    name           = Rec#country_server.name
  , country_name   = Rec#country_server.country_name
  , continent      = Rec#country_server.continent
  , pid            = Rec#country_server.pid
  , status         = Rec#country_server.status
  , substatus      = Rec#country_server.substatus
  , progress       = Rec#country_server.progress
  , children       = Rec#country_server.children
  , started_at     = Rec#country_server.started_at
  , start_complete = Rec#country_server.start_complete
  , zip_size       = ZipSize
  }.


%% -----------------------------------------------------------------------------
%% Update status of a given server without time stamp
%% When a server crashes, we only get its Pid
set_server_status(CountryServerList, Pid, crashed, Substatus, _, _, _) when is_pid(Pid) ->
  Rec = lists:keyfind(Pid, #country_server.pid, CountryServerList),

  ServerStatus = #country_server{
    name         = Rec#country_server.name
  , country_name = Rec#country_server.country_name
  , continent    = Rec#country_server.continent
  , status       = crashed
  , substatus    = Substatus
  , zip_size     = Rec#country_server.zip_size
  },

  lists:keyreplace(Pid, #country_server.pid, CountryServerList, ServerStatus);

%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
%% When a server is stopped (either due to no cities, or manual command, we
%% still have its name available
set_server_status(CountryServerList, Name, stopped, Substatus, _, _, _) ->
  Rec = lists:keyfind(Name, #country_server.name, CountryServerList),

  ServerStatus = #country_server{
    name         = Name
  , country_name = Rec#country_server.country_name
  , continent    = Rec#country_server.continent
  , status       = stopped
  , substatus    = Substatus
  , zip_size     = Rec#country_server.zip_size
  },

  lists:keyreplace(Name, #country_server.name, CountryServerList, ServerStatus);
  
%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
%% Server identified by its name
set_server_status(CountryServerList, Name, Status, Substatus, Progress, Children, Time) ->
  Rec = lists:keyfind(Name, #country_server.name, CountryServerList),

  ?TRACE("Updating status for country_server record ~p",[Rec]),
  ?TRACE("Name = ~p, Status = ~p, Substatus = ~p, Progress = ~p, Children = ~p, Time = ~p",[Name, Status, Substatus, Progress, Children, Time]),

  ServerStatus = #country_server{
    name         = Name
  , country_name = Rec#country_server.country_name
  , continent    = Rec#country_server.continent
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

  , started_at = case Substatus of
      init -> Time;
      _    -> Rec#country_server.started_at
    end

  , start_complete = case Substatus of
      running -> Time;
      _       -> Rec#country_server.start_complete
    end

  , zip_size = Rec#country_server.zip_size
  },

  lists:keyreplace(Name, #country_server.name, CountryServerList, ServerStatus).

%% -----------------------------------------------------------------------------
%% Format server status list
format_server_status_list(CountryServerList, Status) when is_atom(Status) ->
  Str1 = io_lib:format("~n* * * * * * * * * * Country Manager Status (~s) * * * * * * * * * *~n", [atom_to_list(Status)]),
  Str2 = io_lib:format("~nCountry server count = ~w~n~n",[length(CountryServerList)]),

  format_server_status_list(CountryServerList, [Str1] ++ [Str2]);

format_server_status_list([], Acc) ->
  io:format("~s~n",[lists:flatten(Acc)]);

format_server_status_list([S | Rest], Acc) when is_record(S, country_server) ->
  Str1 = io_lib:format("~p (~p) ~p/~p",
                       [S#country_server.name,   S#country_server.pid,
                        S#country_server.status, S#country_server.substatus]),
  
  Str2 = case S#country_server.status of
    starting -> io_lib:format(", Progress: ~p%", [S#country_server.progress]);
    _        -> ""
  end,

  Str3 = io_lib:format(", Started at: ~s", [format_datetime(S#country_server.started_at)]),

  Str4 = case S#country_server.start_complete of
      undefined -> "";
      Completed -> io_lib:format(", Start duration: ~s",
                                 [format_seconds(time_diff(Completed, S#country_server.started_at))])
  end,

  Str5 = case length(S#country_server.children) of
    0 -> io_lib:format("~n",[]);
    _ -> io_lib:format(", Children = ~p~n", [S#country_server.children])
  end,

  format_server_status_list(Rest, Acc ++ [Str1] ++ [Str2] ++ [Str3] ++ [Str4] ++ [Str5]).

%% -----------------------------------------------------------------------------
%% List servers by status
started_servers(CountryServerList)  -> filter_by_status(CountryServerList, started).
starting_servers(CountryServerList) -> filter_by_status(CountryServerList, starting).

%% -----------------------------------------------------------------------------
%% Filter server list by status
filter_by_status(CountryServerList, Status) ->
  filter_by_status(CountryServerList, Status, []).

filter_by_status([], _Status, Acc)        -> Acc;
filter_by_status([S | Rest], Status, Acc) ->
  filter_by_status(Rest, Status, Acc ++ case S#country_server.status of Status -> [S]; _ -> [] end).
  
%% -----------------------------------------------------------------------------
%% Get country server name from pid.  Returns an atom
get_server_name_from_pid(CountryServerPid, CountryServerList) ->
  case lists:keyfind(CountryServerPid, #country_server.pid, CountryServerList) of
    false -> unknown_pid;
    Rec   -> Rec#country_server.name
end.


%% -----------------------------------------------------------------------------
%% Sort country server records intoi alphabetic order by continent
sort_country_server(A,B) when A#country_server.continent < B#country_server.continent ->
  true;

sort_country_server(A,B) when A#country_server.continent > B#country_server.continent ->
  false;

sort_country_server(A,B) ->
  A#country_server.country_name < B#country_server.country_name.



%% -----------------------------------------------------------------------------
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

