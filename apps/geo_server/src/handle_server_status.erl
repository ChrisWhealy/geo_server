-module(handle_server_status).
-behavior(cowboy_handler).

-author("Chris Whealy <chris.whealy@sap.com>").
-revision("Revision: 1.0.0").
-created("Date: 2018/04/03 09:31:24").
-created_by("chris.whealy@sap.com").

-export([init/2]).

%% Include record definitions first
-include("../include/rec_cmd_response.hrl").
-include("../include/rec_country_server.hrl").


%% Include various utilities
-include("../include/default_http_response.hrl").
-include("../include/utils_format_binary.hrl").
-include("../include/utils_json_transform.hrl").
-include("../include/utils_time.hrl").
-include("../include/utils_format_time.hrl").

-define(HTTP_GET, <<"GET">>).

%% =====================================================================================================================
%%
%%                                                 P U B L I C   A P I
%%
%% =====================================================================================================================

init(Req=#{method := ?HTTP_GET}, _State) ->
  country_manager ! {cmd, status, self()},

  ServerStatusDetails = receive
    {country_server_list, ServerStatusList, trace_on, Trace} ->
      server_status_details(ServerStatusList, Trace)
  end,

  {ok, cowboy_req:reply(200, ?CONTENT_TYPE_JSON, ServerStatusDetails, Req), _State};


init(Req, _State) ->
  {ok, ?METHOD_NOT_ALLOWED_RESPONSE(Req), _State}.

 
%% =====================================================================================================================
%%
%%                                                P R I V A T E   A P I
%%
%% =====================================================================================================================


%% ---------------------------------------------------------------------------------------------------------------------
%% Format server status list
server_status_details(ServerStatusList, Trace) ->
  CountryManagerTrace = make_json_prop(country_manager_trace, Trace),
  MemoryUsage         = make_json_prop(erlang_memory_usage,   format_as_binary_units(erlang:memory(total))),

  Servers = make_json_prop(servers, make_json_array([ record_to_json(country_server, S) || S <- ServerStatusList ])),

  make_json_obj([CountryManagerTrace, MemoryUsage, Servers]).

