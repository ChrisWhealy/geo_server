-module(client_info_handler).
-behavior(cowboy_handler).

-export([init/2]).

-include("../include/default_http_response.hrl").

init(Req=#{method := <<"GET">>}, State) -> {ok, ?CLIENT_INFO_RESPONSE(Req), State};
init(Req, State)                        -> {ok, ?METHOD_NOT_ALLOWED_RESPONSE(Req), State}.


