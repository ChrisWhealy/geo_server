-module(request_handler).
-behavior(cowboy_handler).

-author("Chris Whealy <chris.whealy@sap.com>").
-revision("Revision: 1.0.0").
-created("Date: 2018/02/03 10:45:47").
-created_by("chris.whealy@sap.com").

-export([init/2]).

-include("../include/trace.hrl").
-include("../include/geoname.hrl").
-include("../include/country_server.hrl").

-define(SERVER_NAME(Cc), list_to_atom("country_server_" ++ string:lowercase(Cc))).

-define(QS_PARAMETERS, [search_term, whole_word, starts_with]).

%% -----------------------------------------------------------------------------
%%                             P U B L I C   A P I
%% -----------------------------------------------------------------------------

%% -----------------------------------------------------------------------------
init(Req, _State) ->
	%% Debug trace
	put(trace, false),

	#{search_term := P1, whole_word := P2, starts_with := P3} = cowboy_req:match_qs(?QS_PARAMETERS, Req),

	QS = {search_term, binary_to_list(P1),
	      whole_word,  binary_to_atom(P2, latin1),
        starts_with, binary_to_atom(P3, latin1)},
        
  ?TRACE("~c* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *",[$\n]),
  ?TRACE("~p", [QS]),

  % Are the query string parameters valid?
  Response = case validate_qs_parms(QS) of
    %% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    true ->
      % Get list of started country servers from the country_manager
      country_manager ! {cmd, status, started, self()},
	
      ServerList = receive
        {started_servers, S} -> S;
        _ -> []
      end,

      Ref = make_ref(),

      %% Send query to all started country servers
      [ Svr#country_server.name ! {query, Ref, QS, self()} || Svr <- ServerList ],

      ResultList = wait_for_results(Ref, length(ServerList)),

      cowboy_req:reply(200,
        #{<<"content-type">>                => <<"text/json">>,
          <<"access-control-allow-origin">> => <<"http://localhost:12345">>},
        list_to_binary(geoname_to_json(ResultList)),
        Req);

    %% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    false ->
      cowboy_req:reply(400,
        #{<<"content-type">>                => <<"text/json">>,
          <<"access-control-allow-origin">> => <<"http://localhost:12345">>},
        list_to_binary(format_bad_request(QS)),
        Req)

  end,

	{ok, Response, []}.





%% -----------------------------------------------------------------------------
%%                            P R I V A T E   A P I
%% -----------------------------------------------------------------------------

%% -----------------------------------------------------------------------------
wait_for_results(Ref, N) -> wait_for_results(Ref, N, []).

wait_for_results(_Ref, 0, Acc) ->
	?TRACE("Search complete. ~w results received",[length(Acc)]),
	Acc;

wait_for_results(Ref, N, Acc) ->
	receive
		{results, Ref, ResultList} -> wait_for_results(Ref, N-1, Acc ++ ResultList);
		{error,   Ref, Reason}     -> wait_for_results(Ref, N-1, Acc ++ Reason)
  end.

%% -----------------------------------------------------------------------------
%% Convert a geoname_int record to a JSON string
geoname_to_json(GeonameList) -> geoname_to_json(GeonameList, []).

geoname_to_json([], Acc)         -> "[" ++ lists:flatten(lists:join(",", Acc)) ++ "]";
geoname_to_json([G | Rest], Acc) ->
	JsonStr = "{ \"name\": \""       ++ G#geoname_int.name ++ "\", "
	          "\"lat\": "            ++ G#geoname_int.latitude ++ ", "
	          "\"lng\": "            ++ G#geoname_int.longitude ++ ", "
	          "\"featureClass\": \"" ++ G#geoname_int.feature_class ++ "\", "
	          "\"featureCode\": \""  ++ G#geoname_int.feature_code ++ "\", "
	          "\"countryCode\": \""  ++ G#geoname_int.country_code ++ "\", "
	          "\"admin1Txt\": \""    ++ val_or_null(G#geoname_int.admin1_txt) ++ "\", "
	          "\"admin2Txt\": \""    ++ val_or_null(G#geoname_int.admin2_txt) ++ "\", "
	          "\"admin3Txt\": \""    ++ val_or_null(G#geoname_int.admin3_txt) ++ "\", "
	          "\"admin4Txt\": \""    ++ val_or_null(G#geoname_int.admin4_txt) ++ "\", "
						"\"timezone\": \""     ++ G#geoname_int.timezone ++ "\" }",

	geoname_to_json(Rest, Acc ++ [JsonStr]).

val_or_null(undefined) -> "null";
val_or_null(Val)       -> Val.

%% -----------------------------------------------------------------------------
%% Validate the query string parameter values
validate_qs_parms({search_term, _, whole_word, WW, starts_with, SW}) ->
  is_boolean(WW) and is_boolean(SW).

format_bad_request({search_term, _, whole_word, WW, starts_with, SW}) ->
  MsgStr = [format_bad_boolean(whole_word, WW),
            format_bad_boolean(starts_with, SW)],

  "{ \"error\": \"Bad request\","
  "  \"reason\": " ++ list_to_json_array(MsgStr) ++ " }".

format_bad_boolean(_, V) when is_boolean(V) -> ok;
format_bad_boolean(K, V)                    -> io_lib:format("Parameter '~p' contains invalid Boolean value '~p'",[K, V]).

list_to_json_array(L) -> list_to_json_array(L,[]).

list_to_json_array([], Acc)            -> "[" ++ string:join(Acc, ",") ++ "]";
list_to_json_array([ok | Tail], Acc)   -> list_to_json_array(Tail, Acc);
list_to_json_array([Head | Tail], Acc) -> list_to_json_array(Tail, Acc ++ ["\"" ++ Head ++ "\""]).
