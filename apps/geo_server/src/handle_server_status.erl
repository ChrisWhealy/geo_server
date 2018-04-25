-module(handle_server_status).
-behavior(cowboy_handler).

-author("Chris Whealy <chris.whealy@sap.com>").
-revision("Revision: 1.0.0").
-created("Date: 2018/04/03 09:31:24").
-created_by("chris.whealy@sap.com").

-export([init/2]).

-include("../include/trace.hrl").
-include("../include/default_http_response.hrl").
-include("../include/utils.hrl").

-define(HTTP_GET, <<"GET">>).

%% ---------------------------------------------------------------------------------------------------------------------
%%                                                P U B L I C   A P I
%% ---------------------------------------------------------------------------------------------------------------------

init(Req=#{method := ?HTTP_GET}, _State) ->
  put(trace, true),

  country_manager ! {cmd, status, self()},

  ServerStatusDetails = receive
    {country_server_list, ServerStatusList,
     trace_on,            Trace,
     network_trace,       NetworkTrace} -> server_status_details(ServerStatusList, Trace, NetworkTrace)
  end,

  {ok, cowboy_req:reply(200, ?CONTENT_TYPE_JSON, ServerStatusDetails, Req), _State};


init(Req, _State) ->
  {ok, ?METHOD_NOT_ALLOWED_RESPONSE(Req), _State}.

 
%% ---------------------------------------------------------------------------------------------------------------------
%%                                               P R I V A T E   A P I
%% ---------------------------------------------------------------------------------------------------------------------


%% ---------------------------------------------------------------------------------------------------------------------
%% Format server status list
server_status_details(ServerStatusList, Trace, NetTrace) ->
  CountryManagerTrace = make_json_prop(country_manager_trace, Trace),
  NetworkTrace        = make_json_prop(network_trace,         NetTrace),
  MemoryUsage         = make_json_prop(erlang_memory_usage,   format_as_binary_units(erlang:memory(total))),

  % Servers = make_json_prop(servers, servers_by_continent(ServerStatusList)),
  Servers = make_json_prop(servers, servers_by_size(ServerStatusList)),

  make_json_obj([CountryManagerTrace, NetworkTrace, MemoryUsage, Servers]).

%% ---------------------------------------------------------------------------------------------------------------------
%% Transform server status list into an array of JSON objects sorted by Zip size

%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
servers_by_size(ServerStatusList) ->
  servers_by_size(lists:sort(fun(A,B) -> sort_by_size(A,B) end, ServerStatusList), []).

servers_by_size([], Acc)         -> make_json_array(Acc);
servers_by_size([S | Rest], Acc) -> servers_by_size(Rest, lists:append(Acc, [record_to_json(country_server, S)])).


%% ---------------------------------------------------------------------------------------------------------------------
%% Transform server status list into an array of JSON objects sorted by continent

%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
servers_by_continent(ServerStatusList) ->
  servers_by_continent(ServerStatusList, [], [], undefined).

%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
servers_by_continent([], CountryAcc, ContinentAcc, PrevContinent) ->
  %% Close off the last continent table and add it to the accumulator and return the whole result as a JSON array
  ContinentProp = make_json_prop(continent, get_continent_name(PrevContinent)),
  CountriesProp = make_json_prop(countries, make_json_array(CountryAcc)),
  make_json_array(lists:append(ContinentAcc, [make_json_obj([ContinentProp, CountriesProp])]));

%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
%% Add country to existing continent table
servers_by_continent([S | Rest], CountryAcc, ContinentAcc, PrevContinent) when PrevContinent == S#country_server.continent ->
  %% Transform this country server record into a JSON object and append it to the country accumulator
  servers_by_continent(Rest, lists:append(CountryAcc, [record_to_json(country_server, S)]), ContinentAcc, S#country_server.continent);

%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
%% Add country to new continent table
servers_by_continent([S | Rest], CountryAcc, ContinentAcc, PrevContinent) ->
  %% If there's anything in the current country accumulator, then first transform it into a JSON array, then make it a
  %% JSON object, and finally add it to the new continent accumulator
  ContinentAcc1 = case CountryAcc of
    [] ->
      ContinentAcc;

    _  ->
      ContinentProp = make_json_prop(continent, get_continent_name(PrevContinent)),
      CountriesProp = make_json_prop(countries, make_json_array(CountryAcc)),
      lists:append(ContinentAcc, [make_json_obj([ContinentProp, CountriesProp])])
  end,

  %% Start a new country accumulator using the current country
  servers_by_continent(Rest, [record_to_json(country_server, S)], ContinentAcc1, S#country_server.continent).


%% ---------------------------------------------------------------------------------------------------------------------
%% Get country code from server name
get_country_code_from_server_name(ServerName) ->
  SvrStr = atom_to_list(ServerName),
  string:uppercase(lists:nthtail(length(SvrStr)-2, SvrStr)).


%% ---------------------------------------------------------------------------------------------------------------------
%% Sort server status records by descending zip file size
sort_by_size(A,B) -> 
  sort_by_size_int(A#country_server.mem_usage, A#country_server.zip_size, 
                   B#country_server.mem_usage, B#country_server.zip_size).

sort_by_size_int(A_mem, A_zip, B_mem, B_zip) when A_mem == undefined; B_mem == undefined -> A_zip > B_zip;
sort_by_size_int(A_mem, A_zip, B_mem, B_zip) when A_mem == 0;         B_mem == 0         -> A_zip > B_zip;

sort_by_size_int(A_mem, _A_zip, B_mem, _B_zip) -> A_mem > B_mem.

%% ---------------------------------------------------------------------------------------------------------------------
%% Translate 2 character continent code into continent name
get_continent_name("AF") -> <<"\"Africa\"">>;
get_continent_name("AN") -> <<"\"Antarctica\"">>;
get_continent_name("AS") -> <<"\"Asia\"">>;
get_continent_name("EU") -> <<"\"Europe\"">>;
get_continent_name("NA") -> <<"\"North America\"">>;
get_continent_name("OC") -> <<"\"Oceana\"">>;
get_continent_name("SA") -> <<"\"South America\"">>;
get_continent_name(V)    -> list_to_binary([<<"\"Unknown (">>, V, <<")\"">>]).


