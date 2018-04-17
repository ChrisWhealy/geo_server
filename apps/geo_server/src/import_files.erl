-module(import_files).

-author("Chris Whealy <chris.whealy@sap.com>").
-revision("Revision: 1.0.0").
-created("Date: 2018/02/08 11:15:07").
-created_by("chris.whealy@sap.com").

-export([
    import_country_info/1
  , http_get_request/4
  , http_head_request/4
  , write_file/2, write_file/4
  , handle_zip_file/4
  , check_for_update/1
]).

-include("../include/trace.hrl").
-include("../include/http_status_codes.hrl").
-include("../include/file_paths.hrl").
-include("../include/now.hrl").
-include("../include/utils.hrl").

-define(STALE_AFTER, 86400).
-define(RETRY_WAIT,  5000).
-define(RETRY_LIMIT, 3).


%% -----------------------------------------------------------------------------
%%                             P U B L I C   A P I
%% -----------------------------------------------------------------------------

%% -----------------------------------------------------------------------------
%% Import the country info file and send list of countries back to the application
import_country_info(ApplicationPid) -> 
  %% Debug trace output
  put(trace, true),

  {ok, PWD} = file:get_cwd(),
  ?TRACE("Present working directory is ~s", [PWD]),

  %% Download the country info file
  spawn(?MODULE, http_get_request, [self(), "countryInfo", ".txt", get(trace)]),
  retry(wait_for_resources(1, text), text, 0),
  
  ApplicationPid ! case parse_countries_file("countryInfo", ".txt") of
    {ok, Countries} -> {country_list, Countries};
    {error, Reason} -> exit({parse_error, Reason})
  end.


%% -----------------------------------------------------------------------------
%% HTTP HEAD request
%%
%% Used to discover country ZIP file size
http_head_request(CallerPid, Filename, Ext, Trace) ->
  case Trace of
    true -> put(trace, true);
    _    -> put(trace, false)
  end,

  Url = ?GEONAMES_URL ++ Filename ++ Ext,

  %% Send the HEAD request, and fire the response back to the CallerPid
  CallerPid ! case ibrowse:send_req(Url, [], head) of
    {ok, StatusCode, Hdrs, _Body} ->
      % ?TRACE("HTTP ~s for HEAD request to ~s",[StatusCode, Url]),

      case StatusCode of
        "200" -> {ok, get_content_length(Hdrs), Filename, Ext};
        _     -> {error, {status_code, string:to_integer(StatusCode)}, Filename, Ext}
      end;

    {error, Reason} ->
      ?TRACE("Error ~p for ~s",[Reason, Url]),
      {error, {other, Reason}, Filename, Ext}
  end.
    

%% -----------------------------------------------------------------------------
%% Always perform a conditional HTTP GET request
%%
%% This function is typically spawned, so for debug purposes, it inherits the
%% debug trace setting from its parent
http_get_request(CallerPid, Filename, Ext, Trace) ->
  case Trace of
    true -> put(trace, true);
    _    -> put(trace, false)
  end,

  Url = ?GEONAMES_URL ++ Filename ++ Ext,

  %% Check to see if we have an ETag for this file
  Response = case read_etag_file(Filename) of
    missing -> 
      ?TRACE("ETag file missing, sending normal GET request for ~s",[Url]),
      ibrowse:send_req(Url, [], get);

    Etag ->
      ?TRACE("Sending conditional GET request for ~s",[Url]),
      ibrowse:send_req(Url, [{"If-None-Match", Etag}], get)
  end,
    
  CallerPid ! case Response of
    {ok, "200", Hdrs, Body} ->
      ?TRACE("HTTP 200 for ~s",[Url]),
      {ok, Filename, Ext, get_etag(Hdrs), Body};

    {ok, "304",_Hdrs,_Body} ->
      ?TRACE("HTTP 304 for ~s",[Url]),
      {not_modified, Filename, Ext};
      
    {ok, StatusCode, _Hdrs, _Body} ->
      ?TRACE("HTTP ~s for ~s",[StatusCode, Url]),
      {error, {status_code, string:to_integer(StatusCode)}, Filename, Ext};

    {error, Reason} ->
      ?TRACE("Error ~p for ~s",[Reason, Url]),
      {error, {other, Reason}, Filename, Ext}
  end.
    

%% -----------------------------------------------------------------------------
%% Check if the file needs to be updated. Files are considered stale after 24 hrs
check_for_update(CountryCode) ->
  case is_stale(?COUNTRY_FILE_FULL(CountryCode)) of
    true ->
      ?TRACE("Country file ~s.txt is stale.  Checking for new version",[CountryCode]),
      country_manager ! {starting, country_file_download, CountryCode},
      spawn(?MODULE, http_get_request, [self(), CountryCode, ".zip", get(trace)]),
      retry(wait_for_resources(1, zip), zip, 0);

    false -> []
  end.


%% -----------------------------------------------------------------------------  
%% Unzip only the data file from a zipped country file, then throw away the ZIP
%% file.
handle_zip_file(Dir, File, Ext, Body) ->
  ZipFileName = Dir ++ File ++ Ext,
  TxtFileName = Dir ++ File ++ ".txt",

  ?TRACE("Writing ZIP file ~s",[ZipFileName]),
  write_file(Dir, File, Ext, Body),
  
  ?TRACE("Unzipping ~s to create ~s",[ZipFileName, TxtFileName]),
  case zip:unzip(ZipFileName, [{file_list, [File ++ ".txt"]}, {cwd, Dir}]) of
    {ok, [TxtFileName]} -> ok;
    {error, Reason}     -> exit({country_zip_file_error, ZipFileName, Reason})
  end,

  %% Delete the ZIP file
  file:delete(ZipFileName).


%% -----------------------------------------------------------------------------
%% Write file to disc
write_file(FQFilename, Content) ->
  case file:write_file(FQFilename, Content) of
    ok              -> ok;
    {error, Reason} -> io:format("Writing file ~s failed. ~p~n",[FQFilename, Reason])
  end.

write_file(Dir, Filename, Ext, Content) ->
  case file:write_file(Dir ++ Filename ++ Ext, Content) of
    ok              -> ok;
    {error, Reason} -> io:format("Writing file ~s failed. ~p~n",[Dir ++ Filename ++ Ext, Reason])
  end.


%% -----------------------------------------------------------------------------
%%                            P R I V A T E   A P I
%% -----------------------------------------------------------------------------

%% -----------------------------------------------------------------------------
%% Retry download of files that previously failed
retry([], _, _) -> done;

retry(RetryList, _, RetryCount) when RetryCount >= ?RETRY_LIMIT ->
  exit({retry_limit_exceeded, RetryList});

retry(RetryList, Ext, RetryCount) ->
  ?TRACE("Waiting ~ws before retrying download of ~w files",[(?RETRY_WAIT div 1000), length(RetryList)]),
  Parent = self(),

  receive
  after ?RETRY_WAIT ->
    lists:foreach(fun({F,Ext1}) -> spawn(?MODULE, http_get_request, [Parent, F, Ext1, get(trace)]) end, RetryList),
    retry(wait_for_resources(length(RetryList), Ext), Ext, RetryCount + 1)
  end.


%% -----------------------------------------------------------------------------
%% Determine whether or not a local file has gone stale
is_stale(Filename) ->
  case file_age(Filename) of
    N when N > ?STALE_AFTER -> true;
    _                       -> false
  end.

%% -----------------------------------------------------------------------------
%% Determine age of file in seconds.  If the file does not exist, then assume
%% its modification date is midnight on Jan 1st, 1970
file_age(Filename) ->
  {Date,{H,M,S}} = case filelib:last_modified(Filename) of
    0 -> {{1970,1,1}, {0,0,0}};
    T -> T
  end,

  %% Find time difference between the file's last modified time and now
  %% Convert standard DateTime to custom DateTime format by adding microseconds
  time_diff(?NOW, {Date,{H,M,S,0}}).

%% -----------------------------------------------------------------------------
%% Wait for 'Count' HTTP resource to be returned
wait_for_resources(Count, text) -> wait_for_resources(Count, write_file, []);
wait_for_resources(Count, zip)  -> wait_for_resources(Count, handle_zip_file, []).

wait_for_resources(0,    _Fun, RetryList) -> RetryList;
wait_for_resources(Count, Fun, RetryList) ->
  RetryList1 = receive
    %% New version of the file has been receievd
    {ok, Filename, Ext, Etag, Body} ->
      %% Each country file or the hierarchy file is written to its own directory
      TargetDir = ?TARGET_DIR ++ Filename ++ "/",
      filelib:ensure_dir(TargetDir),

      ?TRACE("Received ~s with ETag ~s",[Filename ++ Ext, Etag]),
      
      %% If an ETag is included in the HTTP response, then write it to disc
      case Etag of
        missing -> done;
        _       -> write_file(?COUNTRY_FILE_ETAG(Filename), Etag)
      end,

      %% Call handler for this file type    
      ?MODULE:Fun(TargetDir, Filename, Ext, Body),
      RetryList;
    
    %% This file is unchanged since the last refresh
    {not_modified, _Filename, _Ext} ->
      ?TRACE("~s is unchanged since last request", [_Filename ++ _Ext]),
      RetryList;
  
    %% Various error conditions
    {error, SomeReason, Filename, Ext} ->
      case SomeReason of
        {status_code, StatusCode} ->
          {_, Desc} = http_status_code(StatusCode),
          io:format("HTTP ~w \"~s\": ~s~n", [StatusCode, Desc, ?GEONAMES_URL ++ Filename ++ Ext]);

        {other, req_timedout} ->
          io:format("Error: Request timed out for ~s~s~n", [?GEONAMES_URL ++ Filename, Ext]);
      
        {other, {conn_failed, {error, _Reason}}} ->
          io:format("Error: Connection to ~s~s failed.  Host is down or unreachable.~n", [?GEONAMES_URL ++ Filename, Ext]);
      
        {other, Reason} ->
          io:format("Error: ~w requesting ~s~s~n", [Reason, ?GEONAMES_URL ++ Filename, Ext])
      end,

      RetryList ++ [{Filename, Ext}]
  end,

  wait_for_resources(Count-1, Fun, RetryList1).


%% -----------------------------------------------------------------------------
%% Read the countryInfo.txt file and fetch each individual country file
parse_countries_file(Filename, Ext) ->
  parse_countries_file(file:open(?TARGET_DIR ++ Filename ++ "/" ++ Filename ++ Ext, [read])).
  
% Generate a list of country codes
parse_countries_file({ok, IoDevice})  -> {ok, read_countries_file(IoDevice, [])};
parse_countries_file({error, Reason}) -> {error, Reason}.


%% -----------------------------------------------------------------------------
%% Read the countries file and create a list of country codes skipping any lines
%% that import_country_info with a hash character
read_countries_file(IoDevice, []) ->
  read_countries_file(IoDevice, io:get_line(IoDevice,""), []).

read_countries_file(IoDevice, eof, Acc) ->
  file:close(IoDevice),
  Acc;

read_countries_file(IoDevice, DataLine, Acc) ->
  LineTokens = string:split(DataLine,"\t",all),
  [[Char1 | _] | _] = LineTokens,
  read_countries_file(IoDevice, io:get_line(IoDevice,""), get_country_info(Char1, LineTokens, Acc)).

%% Extract ISO country code, country name and continent code
%% These are the 1st, 5th and 9th columns of countryInfo.txt
get_country_info($#,_Tokens, Acc) -> Acc;
get_country_info(_,  Tokens, Acc) ->
  lists:append(Acc, [{lists:nth(1,Tokens), lists:nth(5,Tokens), lists:nth(9,Tokens)}]).

%% -----------------------------------------------------------------------------
%% Read local eTag file
read_etag_file({ok, IoDevice}) -> read_etag_file(IoDevice, io:get_line(IoDevice,""), "");
read_etag_file({error, _})     -> missing;
read_etag_file(Filename)       ->
  ?TRACE("Trying to open Etag file for ~s",[?COUNTRY_FILE_ETAG(Filename)]),
  read_etag_file(file:open(lists:append([?COUNTRY_FILE_ETAG(Filename)]), [read])).

read_etag_file(IoDevice, eof, "")   -> file:close(IoDevice), missing;
read_etag_file(IoDevice, eof, Etag) -> file:close(IoDevice), Etag;
read_etag_file(IoDevice, Etag, "")  -> file:close(IoDevice), Etag.

%% -----------------------------------------------------------------------------
%% Extract eTag from HTTP headers
get_etag([])                    -> missing;
get_etag([{"ETag", Etag} | _])  -> Etag;
get_etag([{"etag", Etag} | _])  -> Etag;
get_etag([{_Hdr, _Val} | Rest]) -> get_etag(Rest).

%% -----------------------------------------------------------------------------
%% Extract content-length from HTTP headers
get_content_length([])                                      -> missing;
get_content_length([{"Content-Length", ContentLength} | _]) -> list_to_integer(ContentLength);
get_content_length([{"content-length", ContentLength} | _]) -> list_to_integer(ContentLength);
get_content_length([{_Hdr, _Val} | Rest])                   -> get_content_length(Rest).

