-module(handle_country_manager_cmd).
-behavior(cowboy_handler).

-author("Chris Whealy <chris.whealy@sap.com>").
-revision("Revision: 1.0.0").
-created("Date: 2018/04/17 11:43:29").
-created_by("chris.whealy@sap.com").

-export([init/2]).

-include("../include/default_http_response.hrl").
-include("../include/utils.hrl").

-define(HTTP_GET, <<"GET">>).


%% =====================================================================================================================
%%
%%                                                 P U B L I C   A P I
%%
%% =====================================================================================================================

init(Req=#{method := ?HTTP_GET}, State) ->
	#{cmd := Cmd} = cowboy_req:match_qs([cmd], Req),

  %% Send the command to the country manager
  case Cmd of
    %% The name of the server to be started must also appear in the query string
    <<"set_debug">> ->
      #{param := Param} = cowboy_req:match_qs([param], Req),

      case Param of
        <<"true">>  -> country_manager ! {cmd, trace, on,  self()};
        <<"false">> -> country_manager ! {cmd, trace, off, self()}
      end;

    <<"start_all">>    -> country_manager ! {cmd, start_all,    self()};
    <<"shutdown_all">> -> country_manager ! {cmd, shutdown_all, self()}
  end,

  %% Wait for command response
  JsonResp = receive
    CmdResponse when is_record(CmdResponse, cmd_response) ->
      record_to_json(cmd_response, CmdResponse);

    SomeVal ->
      CmdResp = #cmd_response{from_server = country_manager, cmd = Cmd, status = error, reason = SomeVal},
      record_to_json(cmd_response, CmdResp)
  end,

  {ok, cowboy_req:reply(200, ?CONTENT_TYPE_JSON, JsonResp, Req), State};


init(Req, State) ->
  {ok, ?METHOD_NOT_ALLOWED_RESPONSE(Req), State}.


%% =====================================================================================================================
%%
%%                                                P R I V A T E   A P I
%%
%% =====================================================================================================================


