%% ----------------------------------------------------------------------------
%% Standard HTTP response information
%% ----------------------------------------------------------------------------
-define(HTTP_STATUS_OK,                 200).
-define(HTTP_STATUS_METHOD_NOT_ALLOWED, 405).

-define(HTTP_PERMITTED_METHODS, #{<<"allow">> => <<"GET">>}).

-define(HTTP_HDR_PLAIN_TEXT, #{<<"content-type">> => <<"text/plain">>}).
-define(SERVER_RUNNING,      <<"The geo_server is running">>).

-define(DEFAULT_RESPONSE(Req),     cowboy_req:reply(?HTTP_STATUS_OK, ?HTTP_HDR_PLAIN_TEXT, ?SERVER_RUNNING, Req)).
-define(CLIENT_INFO_RESPONSE(Req), cowboy_req:reply(?HTTP_STATUS_OK, ?HTTP_HDR_PLAIN_TEXT, list_to_binary(io_lib:format("~p~n",[Req])), Req)).

-define(METHOD_NOT_ALLOWED_RESPONSE(Req), cowboy_req:reply(?HTTP_STATUS_METHOD_NOT_ALLOWED, ?HTTP_PERMITTED_METHODS, Req)).



