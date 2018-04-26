-module(handle_server_status).
-behavior(cowboy_handler).

-author("Chris Whealy <chris.whealy@sap.com>").
-revision("Revision: 1.0.0").
-created("Date: 2018/04/03 09:31:24").
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

init(Req=#{method := ?HTTP_GET}, _State) ->
  country_manager ! {cmd, status, self()},

  ServerStatusDetails = receive
    {country_server_list, ServerStatusList, trace_on, Trace} ->
      server_status_details(ServerStatusList, Trace)
  end,

  {ok, cowboy_req:reply(200, ?CONTENT_TYPE_JSON, ServerStatusDetails, Req), _State};


init(Req, _State) ->
  {ok, ?METHOD_NOT_ALLOWED_RESPONSE(Req), _State}.

 
%% =====================================================================================================================
%%
%%                                                P R I V A T E   A P I
%%
%% =====================================================================================================================


%% ---------------------------------------------------------------------------------------------------------------------
%% Format server status list
server_status_details(ServerStatusList, Trace) ->
  CountryManagerTrace = make_json_prop(country_manager_trace, Trace),
  MemoryUsage         = make_json_prop(erlang_memory_usage,   format_as_binary_units(erlang:memory(total))),
  Servers             = make_json_prop(servers, servers_by_size(ServerStatusList)),

  make_json_obj([CountryManagerTrace, MemoryUsage, Servers]).

%% ---------------------------------------------------------------------------------------------------------------------
%% Transform server status list into an array of JSON objects sorted by Zip size

%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
servers_by_size(ServerStatusList) ->
  servers_by_size(lists:sort(fun(A,B) -> sort_by_size(A,B) end, ServerStatusList), []).

servers_by_size([], Acc)         -> make_json_array(Acc);
servers_by_size([S | Rest], Acc) -> servers_by_size(Rest, lists:append(Acc, [record_to_json(country_server, S)])).


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


