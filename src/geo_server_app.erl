-module(geo_server_app).
-behaviour(application).

-export([
    start/2
	, stop/1
]).

-define(PORT, 8080).

-define(HTTP_PARALLEL_REQ_LIMIT, {max_sessions,      10}).
-define(HTTP_REQ_POOL_SIZE,      {max_pipeline_size, 25}).

-include("../include/file_paths.hrl").

%% -----------------------------------------------------------------------------
%% Start the geo_server application
%% -----------------------------------------------------------------------------
start(_Type, _Args) ->
	%% Get environment variables
  % {ok, Env} = application:get_env(env),

  %% Start iBrowse and set connection limits
  ibrowse:start(),
  ibrowse:set_dest(?GEONAMES_HOST, 80, [?HTTP_PARALLEL_REQ_LIMIT, ?HTTP_REQ_POOL_SIZE]),

  %% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	%% Import and then parse countryInfo.txt from GeoNames.org
  spawn(import_files, import_country_info, [self()]),

  Countries =
		receive
			{country_list, L} -> L;
			{error, Reason}   -> exit({error, Reason}), []
		end,

  %% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  %% Define routes
  Dispatch = cowboy_router:compile([
		{'_', [
			{"/",            default_handler, []}
     ,{"/client_info", client_info_handler, []}
     ,{"/search",      request_handler, []}
		]}
	]),

  {ok, _} = cowboy:start_clear(my_http_listener, [{port, ?PORT}], #{env => #{dispatch => Dispatch}} ),

	geo_server_sup:start(Countries).

%% -----------------------------------------------------------------------------
%% Stop the geo_server application
%% -----------------------------------------------------------------------------
stop(_State) ->
	geo_server_sup:stop(_State).


