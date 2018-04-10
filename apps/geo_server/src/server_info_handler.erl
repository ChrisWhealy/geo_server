-module(server_info_handler).
-behavior(cowboy_handler).

-author("Chris Whealy <chris.whealy@sap.com>").
-revision("Revision: 1.0.0").
-created("Date: 2018/04/03 09:31:24").
-created_by("chris.whealy@sap.com").

-export([init/2]).

-include("../include/trace.hrl").
-include("../include/default_http_response.hrl").
-include("../include/rec_geoname.hrl").
-include("../include/rec_country_server.hrl").
-include("../include/time_utils.hrl").
-include("../include/html_utils.hrl").
-include("../include/format_utils.hrl").

-define(HTTP_GET, <<"GET">>).

-define(TEXT_WHITE,    <<"color: #FFFFFF;">>).
-define(TEXT_LARGER,   <<"font-size: larger;">>).

-define(BG_RED,        <<"background-color: #EE4466;">>).
-define(BG_GREEN,      <<"background-color: #90EE90;">>).
-define(BG_BLUE,       <<"background-color: #1478DB;">>).
-define(BG_YELLOW,     <<"background-color: #FFFF00;">>).
-define(BG_LIGHT_GREY, <<"background-color: #EEEEEE;">>).

%% -----------------------------------------------------------------------------
%%                             P U B L I C   A P I
%% -----------------------------------------------------------------------------

init(Req=#{method := ?HTTP_GET}, _State) ->
  put(trace, true),

  country_manager ! {cmd, status, self()},

  Response = receive
    {country_server_list, ServerStatusList, trace_on, TraceOn} ->
      server_status_details(ServerStatusList, TraceOn)
  end,

  {ok, cowboy_req:reply(200, ?CONTENT_TYPE_HTML, Response, Req), _State};


init(Req, _State) ->
  {ok, ?METHOD_NOT_ALLOWED_RESPONSE(Req), _State}.


%% -----------------------------------------------------------------------------
%%                           P R I V A T E   A P I
%% -----------------------------------------------------------------------------


%% -----------------------------------------------------------------------------
%% Format server status list
server_status_details(ServerStatusList, TraceOn) ->
  CountryManagerTrace = make_p([<<"Country manager trace on: ">>, make_cb(trace_on, TraceOn)]),

  ServerCount    = make_p([<<"Available Country Servers = ">>,   integer_to_binary(length(ServerStatusList))]),
  StartedServers = make_p([<<"Started servers = ">>,             integer_to_binary(length(started_servers(ServerStatusList)))]),
  MemoryUsage    = make_p([<<"Erlang runtime memory usage = ">>, list_to_binary(format_as_binary_units(erlang:memory(total)))]),
  
  {tables,       ServerTableList,
   total_cities, TotalCityCount} = server_status_list_as_table(ServerStatusList),

  CityCountEl = make_p([<<"Total number of cities in running servers = ">>, integer_to_binary(TotalCityCount)]),

  list_to_binary([CountryManagerTrace, ServerCount, StartedServers, CityCountEl, MemoryUsage, ServerTableList]).

%% -----------------------------------------------------------------------------
%% Format server status list

%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
server_status_list_as_table(ServerStatusList) ->
  server_status_list_as_table(ServerStatusList, [], [], 0, undefined).

%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
server_status_list_as_table([], ThisTable, Acc, TotalCityCount, _) ->
  %% Close off the last continent table, add it to the accumulator and return
  %% the final list of tables
  {tables,       lists:append(Acc, make_table([{border,1},{cellpadding,3},{cellspacing,0}], ThisTable)),
   total_cities, TotalCityCount};

%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
%% Add country to existing continent table
server_status_list_as_table([S | Rest], ThisTable, Acc, TotalCityCount, PrevContinent) when PrevContinent == S#country_server.continent ->
  % ?TRACE("~s belongs to previous continent ~s",[S#country_server.country_name, S#country_server.continent]),
  ThisCityCount = get_city_count(S),

  % Transform this record into an HTML table row
  % ?TRACE("Formatting country_server record ~s",[format_country_server_record(S)]),
  ThisRow = make_server_row(S, ThisCityCount),

  server_status_list_as_table(Rest, lists:append(ThisTable, [ThisRow]), Acc, TotalCityCount + ThisCityCount, S#country_server.continent);

%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
%% Add country to new continent table
server_status_list_as_table([S | Rest], ThisTable, Acc, TotalCityCount, _PrevContinent) ->
  % ?TRACE("~s does not belong to previous continent ~s",[S#country_server.country_name, _PrevContinent]),
  ThisCityCount = get_city_count(S),

  %% If there's anything in the current continent table, make a table out of it,
  %% add it to the accumulator
  Acc1 = case ThisTable of
    [] -> Acc;
    _  -> lists:append(Acc, [make_table([{border,1},{cellpadding,3},{cellspacing,0}], ThisTable), make_br()])
  end,

  %% Create a new continent table
  ThisTable1 = [make_continent_hdr(S#country_server.continent)
              , make_column_hdr()
              , make_server_row(S, ThisCityCount)],

  server_status_list_as_table(Rest, ThisTable1, Acc1, TotalCityCount + ThisCityCount, S#country_server.continent).



%% -----------------------------------------------------------------------------
%% Make continent header
make_continent_hdr(ContinentCode) ->
  make_tr(make_td([{colspan,12}, {style, delimit([?TEXT_WHITE, ?TEXT_LARGER, ?BG_BLUE])}], get_continent_name(ContinentCode))).


%% -----------------------------------------------------------------------------
%% Make column header
make_column_hdr() ->
  make_tr([make_th("Action")
         , make_th("ISO")
         , make_th("Country")
         , make_th("Status")
         , make_th("Substatus")
         , make_th("Progress")
         , make_th("City Count")
         , make_th("City Servers")
         , make_th("Started At")
         , make_th("Startup Time")
         , make_th("Memory Usage")
         , make_th("ZIP file size")]).

%% -----------------------------------------------------------------------------
%% Make server table row
make_server_row(S, ThisCityCount) ->
  Bg_colour = case S#country_server.status of
    started ->
      case S#country_server.substatus of
        running -> {style, delimit(?BG_GREEN)};
        _       -> {style, delimit(?BG_YELLOW)}
      end;
    starting -> {style, delimit(?BG_YELLOW)};
    crashed  -> {style, delimit(?BG_RED)};
    _        -> {style, delimit(?BG_LIGHT_GREY)}
  end,

  make_tr([make_td(start_stop_button(S))
         , make_td(Bg_colour, get_country_code_from_server_name(S#country_server.name))
         , make_td(Bg_colour, S#country_server.country_name)
         , make_td(Bg_colour, S#country_server.status)
         , make_td(Bg_colour, S#country_server.substatus)
         , make_td({align, right}, S#country_server.progress)
         , make_td({align, right}, format_city_count(ThisCityCount))
         , make_td({align, right}, len(S#country_server.children))
         , make_td(format_datetime(S#country_server.started_at))
         , make_td({align, right}, format_seconds(time_diff(S#country_server.start_complete, S#country_server.started_at)))
         , make_td({align, right}, memory_usage(S#country_server.pid))
         , make_td({align, right}, format_as_binary_units(S#country_server.zip_size))]).

%% -----------------------------------------------------------------------------
%% Get city count from a country server.
%% This request will only be answered if the country server has a substatus of
%% running
format_city_count(0) -> <<"Unknown">>;
format_city_count(N) -> integer_to_binary(N).

get_city_count(S) when S#country_server.substatus == running ->
  %% Get city count from country server
  S#country_server.name ! {cmd, city_count, self()},

  receive
    {city_count, undefined} -> 0;
    {city_count, N}         -> N
  end;

get_city_count(_) -> 0.


%% -----------------------------------------------------------------------------
%% Start/Stop server button
start_stop_button(S) ->
  {Btn, CmdInput} = case S#country_server.status of
    started -> {make_input([{type,submit},{value,<<"Stop">>}]), 
                make_input([{type,hidden},{name,<<"cmd">>},{value,<<"shutdown">>}])};
    _       -> {make_input([{type,submit},{value,<<"Start">>}]), 
                make_input([{type,hidden},{name,<<"cmd">>},{value,<<"start">>}])}
  end,

  NameInput = make_input([{type,hidden},{name,<<"server_name">>},{value,as_binary(S#country_server.name)}]),

  make_form([{action,<<"/server_cmd">>}], [Btn, NameInput, CmdInput]).

%% -----------------------------------------------------------------------------
%% Get memory usage of a particular process
memory_usage(undefined) ->
  format_as_binary_units(0);

memory_usage(Pid) ->
  Mem = case process_info(Pid, memory) of
    undefined   -> 0;
    {memory, N} -> N
  end,

  format_as_binary_units(Mem).

started_servers(ServerStatusList) -> filter_by_status(ServerStatusList, started).

%% -----------------------------------------------------------------------------
%% Filter server list by status
filter_by_status(ServerStatusList, Status) ->
  filter_by_status(ServerStatusList, Status, []).

filter_by_status([], _Status, Acc)        -> Acc;
filter_by_status([S | Rest], Status, Acc) ->
  filter_by_status(Rest, Status, lists:append(Acc, case S#country_server.status of Status -> [S]; _ -> [] end)).
    
  
%% -----------------------------------------------------------------------------
%% Get length of a list that might be undefined
len(undefined)         -> undefined;
len(L) when is_list(L) -> length(L).


%% -----------------------------------------------------------------------------
%% Translate 2 character continent code into continent name
get_continent_name("AF") -> <<"Africa">>;
get_continent_name("AN") -> <<"Antarctica">>;
get_continent_name("AS") -> <<"Asia">>;
get_continent_name("EU") -> <<"Europe">>;
get_continent_name("NA") -> <<"North America">>;
get_continent_name("OC") -> <<"Oceana">>;
get_continent_name("SA") -> <<"South America">>;
get_continent_name(V)    -> list_to_binary([<<"Unknown (">>, V, <<")">>]).


%% -----------------------------------------------------------------------------
%% Get country code from server name
get_country_code_from_server_name(ServerName) ->
  SvrStr = atom_to_list(ServerName),
  string:uppercase(lists:nthtail(length(SvrStr)-2, SvrStr)).


%% -----------------------------------------------------------------------------
%% Printable format of a country_server record
format_country_server_record(R) ->
  lists:flatten([io_lib:format("~p = ~p, ",[K,V]) || {K,V} <- kv_country_server_record(R)]).

%% Create a KV list from a country_server record and a record instance
kv_country_server_record(R) ->
  lists:zip(record_info(fields,country_server), tl(tuple_to_list(R))).

