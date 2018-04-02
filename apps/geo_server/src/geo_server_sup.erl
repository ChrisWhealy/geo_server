-module(geo_server_sup).
-behaviour(supervisor).

-export([
   start/1
 , stop/1
 , init/1
]).

-define(RESTART_STRATEGY, one_for_one).
-define(INTENSITY, 1).
-define(PERIOD, 5).

-define(SUP_FLAGS, {?RESTART_STRATEGY, ?INTENSITY, ?PERIOD}).

start(Countries) ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, Countries).

%% -----------------------------------------------------------------------------
%% Server startup callback
%% -----------------------------------------------------------------------------
init(Countries) ->
  {ok, {?SUP_FLAGS, [ {country_manager, {country_manager, init, [Countries]}, permanent, brutal_kill, supervisor, [country_manager]} ]
       }
  }.

stop(_State) ->
  country_manager ! {cmd, shutdown}.

