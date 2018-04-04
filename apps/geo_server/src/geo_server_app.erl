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
  put(trace, true),
  ?TRACE("Application start"),
  
	%% Get environment variables
  Port = case os:getenv("PORT") of
    false -> ?TRACE("Environment variable PORT not set.  Defaulting to 8080"), 8080;
    P     -> {Int,_} = string:to_integer(P), Int
  end,


  %% Start iBrowse and set connection limits
  ?TRACE("Starting iBrowse for connecting to GeoNames.org"),
  ibrowse:start(),
  ibrowse:set_dest(?GEONAMES_HOST, 80, [?HTTP_PARALLEL_REQ_LIMIT, ?HTTP_REQ_POOL_SIZE]),

  %% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	%% Import and then parse countryInfo.txt from GeoNames.org
  ?TRACE("Importing country information"),
  spawn(import_files, import_country_info, [self()]),

  Countries =
		receive
			{country_list, L} -> L;
			{error, Reason}   -> exit({error, Reason}), []
    end,

  % Test server with only one country
  % Countries = ["GB"],

  %% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  %% Define routes
  Dispatch = cowboy_router:compile([
		{'_', [
			{"/",            default_handler, []}
     ,{"/server_info", server_info_handler, []}
     ,{"/client_info", client_info_handler, []}
     ,{"/search",      request_handler, []}
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


