%% =====================================================================================================================
%%
%%                                       J S O N   T R A N S F O R M A T I O N S
%%
%% =====================================================================================================================


%% =====================================================================================================================
%% Convert various datatypes to JavaScript-ready binary strings
%% =====================================================================================================================

%% ---------------------------------------------------------------------------------------------------------------------
%% Convert all values to binary ensuring that string values are delimited by double quotes

%% Erlang 'undefined' become JavaScript 'null'
as_binary(undefined) -> <<"null">>;

%% Assume existing binary values are already formatted correctly (might be dangerous!)
as_binary(V) when is_binary(V) -> V;

%% Numbers do not need to be delimited
as_binary(V) when is_integer(V); is_float(V) -> list_to_binary(io_lib:format("~p",[V]));

as_binary(V) when is_atom(V) -> list_to_binary([<<"\"">>, atom_to_list(V), <<"\"">>]);
as_binary(V) when is_pid(V)  -> list_to_binary([<<"\"">>, pid_to_list(V), <<"\"">>]);

%% For strings, io_lib:format adds double quotes automatically
as_binary(V) -> list_to_binary(lists:flatten(io_lib:format("~p",[V]))).



%% =====================================================================================================================
%% Transform a list into a JSON array
%% =====================================================================================================================

%% ---------------------------------------------------------------------------------------------------------------------
%% Make a JSON array from the supplied list of values
%% Each element in Vs must be quote delimited string, object or another array
make_json_array([])                  -> <<"[]">>;
make_json_array(Vs) when is_list(Vs) -> list_to_binary([<<"[">>, lists:join(<<",">>,Vs), <<"]">>]);
make_json_array(V)                   -> list_to_binary([<<"[">>, V,                      <<"]">>]).

%% ---------------------------------------------------------------------------------------------------------------------
%% Make a JSON property from the supplied name and value
make_json_prop(PropName, PropValue) -> list_to_binary(lists:flatten([as_binary(PropName), <<":">>, as_binary(PropValue)])).
  

%% ---------------------------------------------------------------------------------------------------------------------
%% Make a JSON object from the supplied list of properties
%% Each property must already be in the form <<"\"prop_name\":"\value\"">>
make_json_obj([]) -> <<"{}">>;

make_json_obj(Props) when is_list(Props) -> list_to_binary([<<"{">>, lists:flatten(lists:join(<<",">>, Props)), <<"}">>]);
make_json_obj(Prop)                      -> list_to_binary([<<"{">>, Prop, <<"}">>]).



%% =====================================================================================================================
%%
%% Convert various records into a simple JSON object
%%
%% =====================================================================================================================

%% ---------------------------------------------------------------------------------------------------------------------
%% Various command responses
%%
%% The payload field contains a single country_server record
record_to_json(cmd_response, Rec) when is_record(Rec#cmd_response.payload, country_server) ->
  %% Check if the "reason" field contains a country_server record
  KV = kv_cmd_response_record(Rec#cmd_response{
         payload = record_to_json(country_server, Rec#cmd_response.payload)
       }),
  kv_to_json(KV);


%% The payload field contains a list of country_server records
%% It is assumed that if the payload field contains a list, that it can only be a list of country_server records
record_to_json(cmd_response, Rec) when is_list(Rec#cmd_response.payload) ->
  KV = kv_cmd_response_record(Rec#cmd_response{
         payload = make_json_array([ record_to_json(country_server, Svr) || Svr <- Rec#cmd_response.payload ])
       }),
  kv_to_json(KV);


%% Some other command response
record_to_json(cmd_response, Rec) -> kv_to_json(kv_cmd_response_record(Rec));


%% ---------------------------------------------------------------------------------------------------------------------
%% A country_server record
record_to_json(country_server, Rec) ->
  %% Format the Erlang specific date time field in the record
  R1 = Rec#country_server{ started_at = format_datetime(Rec#country_server.started_at) },
  kv_to_json(kv_country_server_record(R1)).



%% ---------------------------------------------------------------------------------------------------------------------
%% Create a KV list from a country_server record and a record instance
kv_cmd_response_record(R) -> lists:zip(record_info(fields, cmd_response), tl(tuple_to_list(R))).

%% Create a KV list from a country_server record and a record instance
kv_country_server_record(R) -> lists:zip(record_info(fields, country_server), tl(tuple_to_list(R))).

%% Assemble the JSON object from the KV list
kv_to_json(KVList) -> kv_to_json(KVList, []).

kv_to_json([], Acc)             -> make_json_obj(Acc);
kv_to_json([{K,V} | Rest], Acc) -> kv_to_json(Rest, lists:append(Acc, [make_json_prop(K, V)])).


