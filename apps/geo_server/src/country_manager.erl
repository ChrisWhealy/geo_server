-module(country_manager).

-author("Chris Whealy <chris.whealy@sap.com>").
-revision("Revision: 0.1").
-created("Date: 2018/03/02 09:22:03").
-created_by("chris.whealy@sap.com").

-export([
    init/1
  , start/1
  , wait_for_msgs/1
]).

-include("../include/trace.hrl").
-include("../include/now.hrl").
-include("../include/time_utils.hrl").
-include("../include/country_server.hrl").

-define(COUNTRY_SERVER_NAME(CC), list_to_atom("country_server_" ++ string:lowercase(CC))).

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
  process_flag(trap_exit, true),

  % Debug trace
  put(trace, false),

  ?TRACE("Country manager (~p) starting ~w country servers at ~p",[self(), length(CountryList), format_datetime(?NOW)]),

  CountryServerList = [
    #country_server{name = ?COUNTRY_SERVER_NAME(CC), pid = country_server:init(CC)}
    ||
    CC <- CountryList
  ],

  wait_for_msgs(CountryServerList).


%% -----------------------------------------------------------------------------
%% Country manager receive loop
%% An empty server status list means that all the country servers have shutdown;
%% therefore, the country manager should also shutdown
wait_for_msgs([]) ->
  exit(normal);

%% -----------------------------------------------------------------------------
%% Country manager receive loop
wait_for_msgs(ServerStatusList) ->
  ServerStatusList1 = receive
    %% -------------------------------------------------------------------------
    %% Termination messages from country servers
    {'EXIT', CountryServerPid, Reason} ->
      case Reason of
        {no_cities, DeadServerName} ->
          io:format("Country server ~p terminated: no_cities~n", [DeadServerName]),
          lists:keydelete(DeadServerName, #country_server.name, ServerStatusList);

        {country_file_error, ReasonStr} ->
          io:format("Error reading full country file. ~s~n", [ReasonStr]),
          ServerStatusList;

        {fca_country_file_error, Reason} ->
          io:format("Error reading internal FCA file. ~p~n", [Reason]),
          ServerStatusList;

        {fcp_country_file_error, Reason} ->
          io:format("Error reading internal FCP file. ~p~n", [Reason]),
          ServerStatusList;

        _ ->
          io:format("Country server ~p (~p) terminated for reason '~p'~n",
                    [get_server_name_from_pid(CountryServerPid, ServerStatusList), CountryServerPid, Reason]),
          exit(Reason),
          ServerStatusList
      end;


    %% -------------------------------------------------------------------------
    %% Status messages from country servers
    {starting, init, CountryServer, StartTime} ->
      Rec = lists:keyfind(CountryServer, #country_server.name, ServerStatusList),
      ?TRACE("Country server ~p (~p) started at ~s",[CountryServer, Rec#country_server.pid, format_datetime(StartTime)]),
      set_server_status(ServerStatusList, CountryServer, starting, file_import, 0, [], StartTime);

    {starting, file_import, CountryServer, complete} ->
      ?TRACE("File import for country server ~p complete",[CountryServer]),
      set_server_status(ServerStatusList, CountryServer, starting, file_import, complete, []);

    {starting, file_import, CountryServer, progress, Progress} ->
      % ?TRACE("~p progress step(s) made importing data for country server ~p",[Progress, CountryServer]),
      set_server_status(ServerStatusList, CountryServer, starting, file_import, Progress, []);

    {starting, file_distribution, CountryServer} ->
      ?TRACE("Country server ~p starting file_distribution",[CountryServer]),
      set_server_status(ServerStatusList, CountryServer, starting, file_distribution, complete, []);

    {starting, file_distribution, CountryServer, new_child, Id} ->
      ?TRACE("Country server ~p starting new child ~c",[CountryServer, Id]),
      set_server_status(ServerStatusList, CountryServer, starting, file_distribution, complete, Id);

    {started, running, CountryServer, StartupComplete} ->
      ?TRACE("Country server ~p start up completed at ~s",[CountryServer, format_datetime(StartupComplete)]),
      set_server_status(ServerStatusList, CountryServer, started, running, complete, [], StartupComplete);
    

    %% -------------------------------------------------------------------------
    %% Trace on/off for country manager and its country servers
    {cmd, trace_on}  -> put(trace, true),  ServerStatusList;
    {cmd, trace_off} -> put(trace, false), ServerStatusList;

    {cmd, trace_on, all}  ->
      put(trace, true),
      lists:foreach(fun(S) -> S#country_server.name ! {cmd, trace_on} end, ServerStatusList),
      ServerStatusList;
    
    {cmd, trace_off, all} ->
      put(trace, false),
      lists:foreach(fun(S) -> S#country_server.name ! {cmd, trace_off} end, ServerStatusList),
      ServerStatusList;

    %% -------------------------------------------------------------------------
    %% Status commmands either from the console or the request handler
    {cmd, status} ->
      format_server_status_list(ServerStatusList, all),

      ServerStatusList;

    {cmd, status, Status} when is_atom(Status) ->
      ServerList = case Status of
        starting -> starting_servers(ServerStatusList);
        started  -> started_servers(ServerStatusList);
        _        -> []
      end,

      format_server_status_list(ServerList, Status),
      ServerStatusList;

    {cmd, status, CC} ->
      ThisCountryServer = ?COUNTRY_SERVER_NAME(CC),

      case whereis(ThisCountryServer) of
        undefined ->
          io:format("No server found for country code ~s~n",[CC]);

        _ ->
          Rec = lists:keyfind(ThisCountryServer, #country_server.name, ServerStatusList),
          format_server_status_list([Rec], ThisCountryServer)
      end,

      ServerStatusList;

    {cmd, status, started, RequestHandlerPid} ->
      RequestHandlerPid ! {started_servers, started_servers(ServerStatusList)},
      ServerStatusList;

    %% -------------------------------------------------------------------------
    %% Shutdown all country servers, then shut down country manager
    {cmd, shutdown} ->
      [ Svr#country_server.name ! {cmd, shutdown} || Svr <- ServerStatusList ],
      ServerStatusList;

    %% -------------------------------------------------------------------------
    %% Shutdown a specific country server
    {cmd, shutdown, CC} ->
      CountryServer = ?COUNTRY_SERVER_NAME(CC),

      case whereis(CountryServer) of
        undefined -> io:format("~p not found~n",[CountryServer]);
        _Pid      -> CountryServer ! {cmd, shutdown}
      end,

      ServerStatusList;

    %% -------------------------------------------------------------------------
    %% (Re)start specific country server
    {cmd, start, CC} ->
      CountryServer = ?COUNTRY_SERVER_NAME(CC),

      case whereis(CountryServer) of
        undefined ->
          ServerStatusList ++ [#country_server{name = CountryServer, pid = country_server:init(CC)}];
        _Pid ->
          io:format("~p already started~n",[CountryServer]),
          ServerStatusList
      end
  end,

  wait_for_msgs(ServerStatusList1).

%% -----------------------------------------------------------------------------
%% Update status of a given server without time stamp
set_server_status(ServerStatusList, Name, stopped, shutdown, _, _) ->
  % Leave start times undefined
  ServerStatus = #country_server{name = Name, status = stopped, substatus = shutdown, progress = 0, children = []},
  lists:keyreplace(Name, #country_server.name, ServerStatusList, ServerStatus);

set_server_status(ServerStatusList, Name, crashed, Reason, _, _) ->
  % Leave start times undefined
  ServerStatus = #country_server{name = Name, status = crashed, substatus = Reason, progress = 0, children = []},
  lists:keyreplace(Name, #country_server.name, ServerStatusList, ServerStatus);

set_server_status(ServerStatusList, Name, Status, Substatus, Progress, Children) ->
  Rec = lists:keyfind(Name, #country_server.name, ServerStatusList),

  P1 = case Progress of
    complete -> 100;
    P        -> Rec#country_server.progress + P
  end,

  ServerStatus = #country_server{
    name      = Name
  , pid       = whereis(Name)
  , status    = Status
  , substatus = Substatus
  , progress  = P1
  , children  = case Children of
      [] -> Rec#country_server.children;
      Id -> Rec#country_server.children ++ [Id]
    end
  , started_at     = Rec#country_server.started_at
  , start_complete = Rec#country_server.start_complete
  },

  lists:keyreplace(Name, #country_server.name, ServerStatusList, ServerStatus).

%% -----------------------------------------------------------------------------
%% Update status of a given server with time stamp
%% Server is still starting
set_server_status(ServerStatusList, Name, starting, file_import, _, _, Time) ->
  ServerStatus = #country_server{
    name           = Name
  , pid            = whereis(Name)
  , status         = starting
  , substatus      = file_import
  , progress       = 0
  , children       = []
  , started_at     = Time
  , start_complete = undefined
  },

  lists:keyreplace(Name, #country_server.name, ServerStatusList, ServerStatus);

%% Server has started
set_server_status(ServerStatusList, Name, started, running, _, _, Time) ->
  Rec = lists:keyfind(Name, #country_server.name, ServerStatusList),

  ServerStatus = #country_server{
    name           = Name
  , pid            = whereis(Name)
  , status         = started
  , substatus      = running
  , progress       = 100
  , children       = Rec#country_server.children
  , started_at     = Rec#country_server.started_at
  , start_complete = Time
  },

  lists:keyreplace(Name, #country_server.name, ServerStatusList, ServerStatus).

%% -----------------------------------------------------------------------------
%% Format server status list
format_server_status_list(ServerStatusList, Status) when is_atom(Status) ->
  Str1 = io_lib:format("~n* * * * * * * * * * Country Manager Status (~s) * * * * * * * * * *~n", [atom_to_list(Status)]),
  Str2 = io_lib:format("~nCountry server count = ~w~n~n",[length(ServerStatusList)]),

  format_server_status_list(ServerStatusList, [Str1] ++ [Str2]);

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
started_servers(ServerStatusList)  -> filter_by_status(ServerStatusList, started).
starting_servers(ServerStatusList) -> filter_by_status(ServerStatusList, starting).

%% -----------------------------------------------------------------------------
%% Filter server list by status
filter_by_status(ServerStatusList, Status) ->
  filter_by_status(ServerStatusList, Status, []).

filter_by_status([], _Status, Acc)        -> Acc;
filter_by_status([S | Rest], Status, Acc) ->
  filter_by_status(Rest, Status, Acc ++ case S#country_server.status of Status -> [S]; _ -> [] end).
  
%% -----------------------------------------------------------------------------
%% Get country server name from pid.  Returns an atom
get_server_name_from_pid(CountryServerPid, ServerStatusList) ->
  case lists:keyfind(CountryServerPid, #country_server.pid, ServerStatusList) of
    false -> unknown_pid;
    Rec   -> Rec#country_server.name
end.


