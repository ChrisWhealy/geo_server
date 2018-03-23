-module(country_server).

-author("Chris Whealy <chris.whealy@sap.com>").
-revision("Revision: 0.1").
-created("Date: 2018/02/16 13:24:21").
-created_by("chris.whealy@sap.com").

-export([
    init/1
  , start/2
]).

-include("../include/trace.hrl").
-include("../include/geoname.hrl").
-include("../include/file_paths.hrl").
-include("../include/now.hrl").

%% -----------------------------------------------------------------------------
%%                             P U B L I C   A P I
%% -----------------------------------------------------------------------------


%% -----------------------------------------------------------------------------
%% Initialise the country server
init(CC) ->
  ServerName = list_to_atom("country_server_" ++ string:lowercase(CC)),
  CountryServerPid = spawn_link(?MODULE, start, [CC, ServerName]),
  register(ServerName, CountryServerPid),
  CountryServerPid.

%% -----------------------------------------------------------------------------
%% Start the country server
start(CC, ServerName) ->
  process_flag(trap_exit, true),
  
  %% Store my own server name and country code
  put(my_name, ServerName),
  put(cc, CC),

  %% Switch trace off for normal operation
  put(trace, false),
  
  %% Inform country manager that this server is starting up
  country_manager ! {starting, init, ServerName, ?NOW},

  %% Ensure that the country directory exists, then check if the country file
  %% needs to be updated
  TargetDir = ?TARGET_DIR ++ CC ++ "/",
  ?TRACE("Starting country server ~s in ~s",[CC, TargetDir]),

  filelib:ensure_dir(TargetDir),
  import_files:check_for_update(CC),

  {FeatureClassA, CityServerList} = case import_files:country_file(CC) of
    {fca, FCA_Response, fcp, FCP_Response} ->
      %% Check for errors importing internal FCA file
      FCA = case FCA_Response of
        {error, FCA_Reason} -> exit({fca_country_file_error, FCA_Reason});
        FCA_Data            -> FCA_Data
      end,

      %% Check for errors importing internal FCP file
      FCP = case FCP_Response of
        {error, FCP_Reason} -> exit({fcp_country_file_error, FCP_Reason});
        FCP_Data            -> FCP_Data
      end,

      country_manager ! {starting, file_import, ServerName, complete},

      {FCA, distribute_cities(FCP)};

    %% Check for errors importing full country text file
    {error, Reason} ->
      exit({country_file_error, io_lib:format("~p",[Reason])}),
      {[],[]}
  end,

  
  %% Inform country manager that start up is complete
  country_manager ! {started, running, ServerName, ?NOW},

  wait_for_msg(CityServerList, FeatureClassA).



%% -----------------------------------------------------------------------------
%% If the CityServerList is empty, then either this country has no cities with
%% populations large enough to appear in a search, or all the city servers have
%% shut down.  Therefore, the country server should also shut down
wait_for_msg([], _) ->
  exit({no_cities, get(my_name)});

wait_for_msg(CityServerList, FeatureClassA) ->
  CityServerList1 = receive
    %% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    %% Termination messages from child processes
    {'EXIT', CityServerPid, Reason} ->
      %% Is the pid that just shut down the hierarchy server?
      case get(hierarchy_server_pid) == CityServerPid of
        %% Yup, hierarchy server shutdown does not need to be reported
        true ->
          CityServerList;

        %% Nope, so one of the city servers must have died
        false ->
          CityServerId = get_child_id(CityServerPid, CityServerList),

          case Reason of
            normal -> ok;
            _      -> io:format("~p (~p) terminated with reason ~p~n", [CityServerId, CityServerPid, Reason])
          end,

          lists:keydelete(CityServerPid, 2, CityServerList)
      end;


    %% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    %% Server commands
    {cmd, city_list, Id} ->
      Id1 = string:uppercase(Id),

      case lists:keyfind(Id1, 4, CityServerList) of
        {pid, Pid, id, Id1} -> Pid ! {cmd, city_list};
        _                   -> io:format("Country ~s has no cities starting with ~s~n",[get(cc), Id])
      end,

      CityServerList;

    {cmd, Cmd} ->
      case Cmd of
        city_list  -> send_cmd_to_all(CityServerList, city_list);
        city_stats -> send_cmd_to_all(CityServerList, city_stats);
        shutdown   -> send_cmd_to_all(CityServerList, shutdown);

        trace_on   -> put(trace, true),  send_cmd_to_all(CityServerList, trace_on);
        trace_off  -> put(trace, false), send_cmd_to_all(CityServerList, trace_off);

        child_list -> io:format("Country server ~s uses child processes~n~s~n",[get(cc), format_proc_list(CityServerList)]);
        _          -> io:format("~s received unknown command ~p~n",[get(my_name), Cmd])
      end,
      
      CityServerList;
        
    %% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    %% Query
    {query, {search_term, Query, whole_word, _, starts_with, _} = QS, RequestHandlerPid} ->
      ?TRACE("Country server ~s (~p) received query \"~s\" from request handler ~p", [get(cc), self(), Query, RequestHandlerPid]),
      RequestHandlerPid ! handle_query(CityServerList, QS),
      CityServerList

  end,

  wait_for_msg(CityServerList1, FeatureClassA).




%% -----------------------------------------------------------------------------
%%                            P R I V A T E   A P I
%% -----------------------------------------------------------------------------

%% -----------------------------------------------------------------------------
%% Wait for responses from city servers

%% Response from last city server
wait_for_results(0, ResultList) ->
  ?TRACE("Country ~s search complete: found ~w results", [get(cc), length(ResultList)]),
  {results, ResultList};

%% Response from city server 
wait_for_results(N, ResultList) ->
  ?TRACE("Country server ~s waiting for ~w more results", [get(cc), N]),

  ResultList1 = ResultList ++ receive
    %% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    %% Response from city server - which might be an empty list
    {results, _Id, SearchResults} ->
      case length(SearchResults) of
        0 -> ?TRACE("No match in ~s for letter ~c",[get(cc), _Id]);
        L -> ?TRACE("Found ~w results in ~s",[L, get(cc)])
      end,

      SearchResults;

    Whatever ->
      io:format("Country server ~s received an unexpected message ~p~n",[get(cc), Whatever]),
      []
  end,

  wait_for_results(N-1, ResultList1).



%% -----------------------------------------------------------------------------
%% Handle query from client
%% If the starts_with parameter is false, then the "contains" search option has
%% been selected.  Therefore, the query myst be sent to all city servers for
%% this country
handle_query(CityServerList, {search_term, Query, whole_word, _, starts_with, false} = QS) ->
  ?TRACE("Sending query to all city servers"),
  send_query_to_all(CityServerList, {QS, get(cc), Query, self()}),
  wait_for_results(length(CityServerList), []);


%% -----------------------------------------------------------------------------
%% Handle query from client
%% If the starts_with parameter is true, then query need only be sent to the
%% city server handling cities starting with the first letter of the search term
handle_query(CityServerList, {search_term, [Char1 | _] = Query, whole_word, _, starts_with, true} = QS) ->
  Id = string:uppercase([Char1]),

  %% Do we have a child process for the first letter of this query?
  case lists:keyfind(Id, 4, CityServerList) of
    %% Yup, so pass query down to the relevant child process
    {pid, ChildPid, id, Id} ->
      ?TRACE("Sending query to ~s city server for letter ~s",[get(cc), Id]),
      ChildPid ! {QS, get(cc), Query, self()},
      wait_for_results(1, []);

    %% Nope, so ignore query
    false ->
      ?TRACE("Country ~s has no city server for letter ~s", [get(cc), Id]),
      {results, []}

  end.


%% -----------------------------------------------------------------------------
%% Create a child process to handle the cities belonging to successive letters
%% of the alphabet
%%
distribute_cities(FCP) ->
  country_manager ! {starting, file_distribution, get(my_name)},
  distribute_cities(FCP, []).

%% CityServerList is a list of {pid, <0.1.0>, id, "A"}
distribute_cities([], CityServerList) -> CityServerList;

%% At the moment, we only care about records with feature classes "A" or "P"
%% (population centres and administrative areas)
distribute_cities([C | Rest], CityServerList) ->
  [Char1 | _] = string:uppercase(C#geoname_int.name),

  %% Extend CityServerList each time a city name starting with new letter of the
  %% alphabet is encountered
  CityServerList1 = CityServerList ++ case lists:keyfind([Char1], 4, CityServerList) of
    %% No child process exists yet for city names starting with this letter
    false ->
      country_manager ! {starting, file_distribution, get(my_name), new_child, Char1},
      [{pid, spawn_link(city_server, init, [self(), Char1, [C]]), id, [Char1]}];

    %% A child process already exists to handle cities starting with this letter,
    %% so send the curemnt record to that process.  We do not expect a reply
    {pid, ChildPid, id, _} ->
      % ?TRACE("Adding record for letter ~c",[Char1]),
      ChildPid ! {add, C},
      []
  end,

  distribute_cities(Rest, CityServerList1).


%% -----------------------------------------------------------------------------
%% Send either a command or a query to all children
send_cmd_to_all(CityServerList, Cmd)            -> lists:foreach(fun({pid, Pid, id, _}) -> Pid ! {cmd, Cmd} end, CityServerList).
send_query_to_all(CityServerList, QueryDetails) -> lists:foreach(fun({pid, Pid, id, _}) -> Pid ! QueryDetails end, CityServerList).



%% -----------------------------------------------------------------------------
%% Format process list
format_proc_list(CityServerList) -> format_proc_list(CityServerList, "").

format_proc_list([], Acc)                          -> lists:flatten(Acc);
format_proc_list([{pid, Pid, id, Id} | Rest], Acc) -> format_proc_list(Rest, Acc ++ io_lib:format("~p ~s~n",[Pid,Id])).



%% -----------------------------------------------------------------------------
%% Get child process id.  Returns an atom
get_child_id(CityServerPid, CityServerList) ->
  case lists:keyfind(CityServerPid, 2, CityServerList) of
    {pid, _, id, Id} -> list_to_atom(Id);
    false            -> unknown_city_server_pid
  end.

