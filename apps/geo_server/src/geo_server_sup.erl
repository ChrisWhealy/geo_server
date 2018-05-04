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

%% -----------------------------------------------------------------------------
%% Server callbacks
%% -----------------------------------------------------------------------------
start(Countries) ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, Countries).

init(Countries) ->
  % Trace flow
  put(trace, true),

  ?TRACE("Supervisor initialising country_manager with ~p countries",[length(Countries)]),
  {ok, {?SUP_FLAGS, [ {country_manager, {country_manager, init, [Countries]}, permanent, brutal_kill, supervisor, [country_manager]} ]
       }
  }.

stop(_State) ->
  ?TRACE("Supervisor shutting down"),

  %% Tell the country_manager to shut down
  country_manager ! {cmd, terminate, self()},

  % Wait for shutdown response
  receive
    {cmd_response, _FromServer, _Cmd, goodbye, _Reason, _Payload} ->
      exit(normal);

    SomeVal ->
      io:format("geo_server supervisor received an unexpected message after issuing the 'terminate' command: ~p~n",[SomeVal]),
      exit({supervisor_shutdown_error, SomeVal})
  end.

