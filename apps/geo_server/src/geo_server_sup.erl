-module(geo_server_sup).
-behaviour(supervisor).

-author("Chris Whealy <chris.whealy@sap.com>").
-revision("Revision: 1.0.0").
-created("Date: 2018/02/02 13:17:39").
-created_by("chris.whealy@sap.com").

-export([
   start/1
 , stop/1
 , init/1
]).

-include("../include/trace.hrl").

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
  % Trace flow
  put(trace, true),

  ?TRACE("Supervisor starting with ~p countries",[length(Countries)]),
  {ok, {?SUP_FLAGS, [ {country_manager, {country_manager, init, [Countries]}, permanent, brutal_kill, supervisor, [country_manager]} ]
       }
  }.

stop(_State) ->
  country_manager ! {cmd, shutdown}.

