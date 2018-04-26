-module(handle_country_server_cmd).
-behavior(cowboy_handler).

-author("Chris Whealy <chris.whealy@sap.com>").
-revision("Revision: 1.0.0").
-created("Date: 2018/04/06 14:52:17").
-created_by("chris.whealy@sap.com").

-export([init/2]).


-include("../include/trace.hrl").
-include("../include/default_http_response.hrl").
-include("../include/utils.hrl").

-define(HTTP_GET, <<"GET">>).

-define(COUNTRY_SERVER_NAME(CC), list_to_atom("country_server_" ++ string:lowercase(binary_to_list(CC)))).

%% =====================================================================================================================
%%
%%                                                 P U B L I C   A P I
%%
%% =====================================================================================================================

init(Req=#{method := ?HTTP_GET}, State) ->
  put(trace, false),
	#{cmd := Cmd} = cowboy_req:match_qs([cmd], Req),
  ?TRACE("Got command ~p from client",[Cmd]),

  %% Commands directed at country servers are routed through the country manager
  case Cmd of
    <<"shutdown_all">> -> country_manager ! {cmd, shutdown_all, self()};

    <<"start">>     -> send_country_server_cmd(Req, start);
    <<"shutdown">>  -> send_country_server_cmd(Req, shutdown);
    <<"reset">>     -> send_country_server_cmd(Req, reset);
    <<"trace_on">>  -> send_country_server_cmd(Req, trace_on);
    <<"trace_off">> -> send_country_server_cmd(Req, trace_off)
  end,

  %% Wait for command response
  JsonResp = receive
    CmdResponse when is_record(CmdResponse, cmd_response) ->
      record_to_json(cmd_response, CmdResponse);

    SomeVal ->
      record_to_json(cmd_response,
                     #cmd_response{from_server = country_manager, cmd = Cmd, status = error, reason = SomeVal})
  end,

  {ok, cowboy_req:reply(200, ?CONTENT_TYPE_JSON, JsonResp, Req), State};


init(Req, State) ->
  {ok, ?METHOD_NOT_ALLOWED_RESPONSE(Req), State}.


%% =====================================================================================================================
%%
%%                                                P R I V A T E   A P I
%%
%% =====================================================================================================================

%% Send a command to a country server via the country manager
send_country_server_cmd(Req, Cmd) ->
  #{country_code := CC} = cowboy_req:match_qs([country_code], Req),
  country_manager ! {cmd, Cmd, binary_to_list(CC), self()}.


