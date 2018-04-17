-module(handle_country_manager_cmd).
-behavior(cowboy_handler).

-author("Chris Whealy <chris.whealy@sap.com>").
-revision("Revision: 1.0.0").
-created("Date: 2018/04/17 11:43:29").
-created_by("chris.whealy@sap.com").

-export([init/2]).

-include("../include/trace.hrl").
-include("../include/default_http_response.hrl").
-include("../include/utils.hrl").

-define(HTTP_GET, <<"GET">>).


%% -----------------------------------------------------------------------------
%%                             P U B L I C   A P I
%% -----------------------------------------------------------------------------

init(Req=#{method := ?HTTP_GET}, State) ->
  put(trace, false),
	#{cmd := Cmd} = cowboy_req:match_qs([cmd], Req),
  ?TRACE("Got command ~p from client",[Cmd]),

  %% Send the command to the country manager
  JsonResp = case Cmd of
    %% The name of the server to be started must also appear in the query string
    <<"set_debug">> ->
      #{param := Param} = cowboy_req:match_qs([param], Req),

      case Param of
        <<"true">>  -> country_manager ! {cmd, trace, on};
        <<"false">> -> country_manager ! {cmd, trace, off}
      end,

      list_to_binary([<<"{\"status\":\"ok\", \"cmd\":\"set_debug\", \"param\":\"">>, Param, <<"\"}">>])
  end,

  {ok, cowboy_req:reply(200, ?CONTENT_TYPE_JSON, JsonResp, Req), State};


init(Req, State) ->
  {ok, ?METHOD_NOT_ALLOWED_RESPONSE(Req), State}.


%% -----------------------------------------------------------------------------
%%                           P R I V A T E   A P I
%% -----------------------------------------------------------------------------


