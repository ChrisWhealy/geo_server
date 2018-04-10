
%% -----------------------------------------------------------------------------
%% Recod to track status of a country server
%% -----------------------------------------------------------------------------
-record(country_server, {
  name                   %% Server name    :: atom()
, country_name           %% Country name   :: string()
, continent              %% Continent code :: string()
, pid                    %% Server pid     :: pid()
, status                 %% Main status    :: atom()
, substatus              %% Substatus      :: atom()
, progress               %% Progress %     :: integer
, children               %% List of child processes :: List[char()]
, started_at             %% Custom timestamp for server start time
, start_complete         %% Custom timestamp for server start up completion time
, zip_size               %% Size of country ZIP file
}).

