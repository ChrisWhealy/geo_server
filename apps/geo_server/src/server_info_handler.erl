-module(server_info_handler).
-behavior(cowboy_handler).

-export([init/2]).

-include("../include/trace.hrl").
-include("../include/default_http_response.hrl").
-include("../include/geoname.hrl").
-include("../include/country_server.hrl").
-include("../include/time_utils.hrl").

-define(KB, 1024).
-define(MB, ?KB * 1024).
-define(GB, ?MB * 1024).

%% -----------------------------------------------------------------------------
%%                             P U B L I C   A P I
%% -----------------------------------------------------------------------------

init(Req=#{method := <<"GET">>}, State) ->
  put(trace, true),
  ?TRACE("Getting server status from country_manager"),

  country_manager ! {cmd, status, self()},

  Response = receive
    ServerStatusList ->
      ?TRACE("Got a list of ~w servers from the country_manager",[length(ServerStatusList)]),
      server_status_list_as_table(ServerStatusList)
  end,

  {ok, cowboy_req:reply(200, ?CONTENT_TYPE_HTML, Response, Req), State};


init(Req, State) ->
  {ok, ?METHOD_NOT_ALLOWED_RESPONSE(Req), State}.


%% -----------------------------------------------------------------------------
%%                           P R I V A T E   A P I
%% -----------------------------------------------------------------------------


%% -----------------------------------------------------------------------------
%% Format server status list
server_status_list_as_table(ServerStatusList) ->
  ServerCount = <<
    <<"<p>">>/binary,
    <<"Active Country Servers = ">>/binary,
    (integer_to_binary(length(ServerStatusList)))/binary,
    <<"</p>">>/binary
  >>,

  MemoryUsage = <<
    <<"<p>">>/binary,
    <<"Erlang runtime memory usage = ">>/binary,
    (list_to_binary(format_as_binary_units(erlang:memory(total))))/binary,
    <<"</p>">>/binary
  >>,

  Table = <<"<table>">>,

  TR     = <<"<tr>">>,
  Col1   = <<"<th>Name</th>">>,
  Col2   = <<"<th>Pid</th>">>,
  Col3   = <<"<th>Status</th>">>,
  Col4   = <<"<th>Substatus</th>">>,
  Col5   = <<"<th>Progress</th>">>,
  Col6   = <<"<th>Children</th>">>,
  Col7   = <<"<th>Started At</th>">>,
  Col8   = <<"<th>Startup Time</th>">>,
  Col9   = <<"<th>Memory Usage</th>">>,
  TR_end = <<"</tr>">>,

  HdrList = [TR, Col1, Col2, Col3, Col4, Col5, Col6, Col7, Col8, Col9, TR_end],

  server_status_list_as_table(ServerStatusList, [ServerCount] ++ [MemoryUsage] ++ [Table] ++ [HdrList]).

server_status_list_as_table([], Acc) ->
  list_to_binary(Acc ++ [<<"</table>">>]);

server_status_list_as_table([S | Rest], Acc) ->
  % Transform this record into an HTML table row
  ThisRow = [<<"<tr>">>]
    ++ [make_td(S#country_server.name)]
    ++ [make_td(S#country_server.pid)]
    ++ [make_td(S#country_server.status)]
    ++ [make_td(S#country_server.substatus)]
    ++ [make_td(S#country_server.progress)]
    ++ [make_td(S#country_server.children)]
    ++ [make_td(format_datetime(S#country_server.started_at))]
    ++ [make_td(format_seconds(time_diff(S#country_server.start_complete, S#country_server.started_at)))]
    ++ [make_td(memory_usage(S#country_server.pid))]
    ++ [<<"</tr>">>],

  server_status_list_as_table(Rest, Acc ++ ThisRow).



%% -----------------------------------------------------------------------------
%% Make a table cell from the supplied data
make_td(V) when is_list(V) ->
  << <<"<td>">>/binary, (list_to_binary(io_lib:format("~s",[V])))/binary, <<"</td>">>/binary >>;

make_td(V) ->
  << <<"<td>">>/binary, (list_to_binary(io_lib:format("~p",[V])))/binary, <<"</td>">>/binary >>.


%% -----------------------------------------------------------------------------
%% Get memory usage of a particular process
memory_usage(Pid) ->
  Mem = case process_info(Pid, memory) of
    undefined   -> 0;
    {memory, N} -> N
  end,

  format_as_binary_units(Mem).


%% -----------------------------------------------------------------------------
%% Format size in binary units Kb, Mb or Gb
format_as_binary_units(N) when N < ?KB -> io_lib:format("~w bytes",[N]);
format_as_binary_units(N) when N < ?MB -> format_as_binary_units(N, ?KB, "Kb");
format_as_binary_units(N) when N < ?GB -> format_as_binary_units(N, ?MB, "Mb");
format_as_binary_units(N)              -> format_as_binary_units(N, ?GB, "Gb").

format_as_binary_units(N, Unit, UnitStr) ->
  WholeUnits = N div Unit,
  Rem = (N - (WholeUnits * Unit)) / Unit,
  io_lib:format("~.2f ~s",[WholeUnits + Rem, UnitStr]).


