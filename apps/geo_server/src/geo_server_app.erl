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

-include("../include/file_paths.hrl").
-include("../include/trace.hrl").

%% -----------------------------------------------------------------------------
%% Start the geo_server application
%% -----------------------------------------------------------------------------
start(_Type, _Args) ->
  process_flag(trap_exit, true),
  put(trace, true),

  ?TRACE("Application start. Type = ~p, Args =~p",[_Type, _Args]),
  
	%% Get environment variables
  Port = case os:getenv("PORT") of
    false -> ?TRACE("Environment variable PORT not set.  Defaulting to 8080"), 8080;
    P     -> {Int,_} = string:to_integer(P), Int
  end,


  %% Start iBrowse and set parallel connection limit
  ?TRACE("Starting iBrowse"),
  ibrowse:start(),
  ibrowse:set_dest(?GEONAMES_HOST, 80, [?HTTP_PARALLEL_REQ_LIMIT, ?HTTP_REQ_POOL_SIZE]),

  %% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	%% Import and then parse countryInfo.txt from GeoNames.org
  ?TRACE("Importing country information"),
  spawn_link(import_files, import_country_info, [self()]),

  Countries =
		receive
      {'EXIT', ChildPid, Reason} ->
        case Reason of
          {retry_limit_exceeded, RetryList} ->
            io:format("Retry limit exceeded downloading ~p~n",[RetryList]);

          SomeError ->
            io:format("Error ~p received from child proces ~p",[SomeError, ChildPid])
        end,
        
        exit({error, Reason});

			{country_list, L} ->
        L;

			{error, Reason} ->
        exit({error, Reason}), []
    end,

  %% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  %% Define routes
  Dispatch = cowboy_router:compile([
		{'_', [
			{"/",            default_handler,     []}
     ,{"/server_info", server_info_handler, []}
     ,{"/server_cmd",  server_cmd_handler,  []}
     ,{"/client_info", client_info_handler, []}
     ,{"/search",      request_handler,     []}
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


