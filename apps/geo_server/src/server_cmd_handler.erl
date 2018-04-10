-module(server_cmd_handler).
-behavior(cowboy_handler).

-author("Chris Whealy <chris.whealy@sap.com>").
-revision("Revision: 1.0.0").
-created("Date: 2018/04/06 14:52:17").
-created_by("chris.whealy@sap.com").

-export([init/2]).

-include("../include/trace.hrl").
-include("../include/default_http_response.hrl").

-define(HTTP_GET, <<"GET">>).
-define(QS_PARAMETERS, [server_name, cmd]).


%% -----------------------------------------------------------------------------
%%                             P U B L I C   A P I
%% -----------------------------------------------------------------------------

init(Req=#{method := ?HTTP_GET}, State) ->
  put(trace, false),
	#{server_name := ServerName, cmd := Cmd} = cowboy_req:match_qs(?QS_PARAMETERS, Req),
  ?TRACE("Got command from client: ~p ~p",[Cmd,ServerName]),

  country_manager ! {cmd, as_atom(Cmd), as_atom(ServerName)},

  {ok, cowboy_req:reply(200, ?CONTENT_TYPE_HTML, redirect_to_server_info(), Req), State};


init(Req, State) ->
  {ok, ?METHOD_NOT_ALLOWED_RESPONSE(Req), State}.


%% -----------------------------------------------------------------------------
%%                           P R I V A T E   A P I
%% -----------------------------------------------------------------------------

redirect_to_server_info() ->
  list_to_binary("<html><head><meta http-equiv=\"refresh\" content=\"0; url=/server_info\"/></head></html>").

as_atom(V) when is_binary(V) -> list_to_atom(binary_to_list(V));
as_atom(V) when is_atom(V)   -> V;
as_atom(V)                   -> list_to_atom(V).

