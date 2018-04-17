-module(handle_server_cmd).
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


%% -----------------------------------------------------------------------------
%%                             P U B L I C   A P I
%% -----------------------------------------------------------------------------

init(Req=#{method := ?HTTP_GET}, State) ->
  put(trace, false),
	#{cmd := Cmd} = cowboy_req:match_qs([cmd], Req),
  ?TRACE("Got command ~p from client",[Cmd]),

  %% Send the command to the country manager
  case Cmd of
    %% The name of the server to be started must also appear in the query string
    <<"start">> ->
      #{country_code := CC} = cowboy_req:match_qs([country_code], Req),
      country_manager ! {cmd, start, binary_to_list(CC), self()};

    <<"shutdown">> ->
      #{country_code := CC} = cowboy_req:match_qs([country_code], Req),
      country_manager ! {cmd, shutdown, binary_to_list(CC), self()};

    <<"shutdown_all">> ->
      country_manager ! {cmd, shutdown_all, self()}
    end,

  %% Wait for country manager response
  JsonResp = receive
    {cmd_response, Resp} when is_atom(Resp) ->
      list_to_binary(["{\"cmd\":\"", Cmd, "\", \"response\":\"", Resp ,"\""]);

    {cmd_response, Resp} when is_record(Resp, country_server) ->
      record_to_json(country_server, Resp);

    SomeVal ->
      list_to_binary(["{\"cmd\":\"", Cmd, "\", \"response\":\"Unexpected msg ", SomeVal ,"\""])
    end,

  {ok, cowboy_req:reply(200, ?CONTENT_TYPE_JSON, JsonResp, Req), State};


init(Req, State) ->
  {ok, ?METHOD_NOT_ALLOWED_RESPONSE(Req), State}.


%% -----------------------------------------------------------------------------
%%                           P R I V A T E   A P I
%% -----------------------------------------------------------------------------


