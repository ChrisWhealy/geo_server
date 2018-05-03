%% ---------------------------------------------------------------------------------------------------------------------
%% Reset a crashed server back to its initial conditions
reset_crashed_server(Rec) ->
  %% Ensure that the country server process is dead
  case whereis(Rec#country_server.name) of
    undefined -> ok;
    Pid       -> exit(Pid, reset)
  end,

  %% Return a new record defining the server's new initial state
  #country_server{
    name           = Rec#country_server.name
  , country_name   = Rec#country_server.country_name
  , continent      = Rec#country_server.continent
  , country_code   = Rec#country_server.country_code
  , status         = stopped
  , zip_size       = Rec#country_server.zip_size
  }.


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
  , startup_time   = Rec#country_server.startup_time
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
  , startup_time   = Rec#country_server.startup_time
  , trace          = trace_state_to_boolean(TraceState)
  , mem_usage      = Rec#country_server.mem_usage
  , zip_size       = Rec#country_server.zip_size
  }.


%% ---------------------------------------------------------------------------------------------------------------------
%% Initialise server status record with status 'stopped' and trace 'false'
set_server_init(CountryCode, CountryName, Continent) ->
  #country_server{
      name         = ?COUNTRY_SERVER_NAME(CountryCode)
    , country_name = CountryName
    , continent    = Continent
    , country_code = CountryCode
    , status       = stopped
    , trace        = false
  }.


%% ---------------------------------------------------------------------------------------------------------------------
%% Set server status to stopped
set_server_stopped(Rec) ->
  Rec#country_server{
    pid          = undefined
  , status       = stopped
  , substatus    = undefined
  , progress     = undefined
  , children     = undefined
  , started_at   = undefined
  , startup_time = undefined
  , mem_usage    = undefined
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
  , startup_time   = time_diff(StartComplete, Rec#country_server.started_at)
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
  ?TRACE("Changing status for ~p to ~p ~p",[Name, Status, Substatus]),
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

  , startup_time = Rec#country_server.startup_time
  , trace        = Rec#country_server.trace

  , mem_usage = case Status of
      started -> memory_usage(Rec#country_server.name);
      _       -> 0
    end

  , zip_size  = Rec#country_server.zip_size
  },

  % ?TRACE("Updated country_server is now ~p",[format_country_server_record(ServerStatus)]),

  lists:keyreplace(Name, #country_server.name, CountryServerList, ServerStatus).


%% ---------------------------------------------------------------------------------------------------------------------
%% Create printable format of a country_server record
format_country_server_record(R) ->
  lists:flatten([io_lib:format("~p = ~p, ",[K,V]) || {K,V} <- kv_country_server_record(R)]).


