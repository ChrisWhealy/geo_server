-module(geo_server_app).
-behaviour(application).

-author("Chris Whealy <chris.whealy@sap.com>").
-revision("Revision: 1.0.0").
-created("Date: 2018/02/02 13:15:19").
-created_by("chris.whealy@sap.com").

-export([
    start/2
	, stop/1
]).

-define(HTTP_PARALLEL_REQ_LIMIT, {max_sessions,      10}).
-define(HTTP_REQ_POOL_SIZE,      {max_pipeline_size, 25}).

-define(DEFAULT_HTTP_PORT, 8080).

-include("../include/file_paths.hrl").
-include("../include/trace.hrl").

%% -----------------------------------------------------------------------------
%% Start the geo_server application
%% -----------------------------------------------------------------------------
start(_Type, _Args) ->
  process_flag(trap_exit, true),
  put(trace, true),

  ?TRACE("Application start. Type = ~p, Args =~p",[_Type, _Args]),
  
	%% Get port number from the environment
  Port = case os:getenv("PORT") of
    false ->
      ?TRACE("Environment variable PORT not set.  Defaulting to ~w",[?DEFAULT_HTTP_PORT]),
      ?DEFAULT_HTTP_PORT;

    P ->
      {Int,_} = string:to_integer(P),
      Int
  end,


  %% Start iBrowse and set parallel connection limit
  ?TRACE("Starting iBrowse"),
  ibrowse:start(),
  ibrowse:set_dest(?GEONAMES_HOST, ?GEONAMES_PORT, [?HTTP_PARALLEL_REQ_LIMIT, ?HTTP_REQ_POOL_SIZE]),

  %% If trace mode is on, set iBrowse trace mode on too
  % case get(trace) of
  %   true -> ibrowse:trace_on(?GEONAMES_HOST, ?GEONAMES_PORT);
  %   _    -> ok
  % end,

  %% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	%% Get countryInfo.txt from GeoNames.org
  ?TRACE("Importing country information"),
  spawn_link(import_files, import_country_info, [self()]),

  Countries =
		receive
      {'EXIT', ChildPid, Reason} ->
        case Reason of
          {retry_limit_exceeded, RetryList} ->
            io:format("Retry limit exceeded downloading ~p~n",[RetryList]);

          {parse_error, Reason} ->
            io:format("Error ~p parsing countryInfo.txt",[Reason]);

          _ ->
            io:format("Error ~p received from child proces ~p",[Reason, ChildPid])
        end,
        
        exit({error, Reason});

			{country_list, L} -> L
    end,

  %% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  %% Define routes
  Dispatch = cowboy_router:compile([
		{'_', [
      %% Browser interfaces
      {"/",                    handle_root,                []}
      ,{"/server_status",       handle_server_status,       []}
      ,{"/client_info",         handle_client_info,         []}
      ,{"/search",              handle_search,              []}

      %% Command API
      ,{"/country_manager_cmd", handle_country_manager_cmd, []}
      ,{"/country_server_cmd",  handle_country_server_cmd,  []}

      %% Handle static files
      ,{"/server_info",         cowboy_static, {priv_file, geo_server, "html/server_info.html"}}
      ,{"/js/server_info.js",   cowboy_static, {priv_file, geo_server, "js/server_info.js"}}
      ,{"/css/server_info.css", cowboy_static, {priv_file, geo_server, "css/server_info.css"}}
		]}
	]),

  ?TRACE("Starting Cowboy server on port ~w",[Port]),
  cowboy:start_clear(my_http_listener, [{port, Port}], #{env => #{dispatch => Dispatch}}),

  ?TRACE("Starting supervisor"),
	geo_server_sup:start(Countries).

%% -----------------------------------------------------------------------------
%% Stop the geo_server application
%% -----------------------------------------------------------------------------
stop(_State) ->
	geo_server_sup:stop(_State).


