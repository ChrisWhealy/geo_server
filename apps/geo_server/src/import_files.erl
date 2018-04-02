-module(import_files).

-author("Chris Whealy <chris.whealy@sap.com>").
-revision("Revision: 0.1").
-created("Date: 2018/02/08 11:15:07").
-created_by("chris.whealy@sap.com").

-export([
    import_country_info/1
  , http_get_request/4
  , write_file/4
  , handle_zip_file/4
  , check_for_update/1
  , country_file/1
]).

-include("../include/trace.hrl").
-include("../include/http_status_codes.hrl").
-include("../include/geoname.hrl").
-include("../include/file_paths.hrl").
-include("../include/now.hrl").
-include("../include/time_utils.hrl").

-define(PROGRESS_FRACTION, 0.01).
-define(STALE_AFTER, 86400).
-define(RETRY_WAIT, 5000).

-define(KB, 1024).
-define(MB, ?KB * 1024).
-define(GB, ?MB * 1024).

%% Feature class P records (Population centres) having a pop[ulation below this
%% limit will not be included in a country's FCP file
-define(MIN_POPULATION, 500).


%% -----------------------------------------------------------------------------
%%                             P U B L I C   A P I
%% -----------------------------------------------------------------------------

%% -----------------------------------------------------------------------------
%% Import the country info file and send list of countries back to the application
import_country_info(ApplicationPid) -> 
  %% Assume that trace output is switched off
  put(trace, false),

  {ok, PWD} = file:get_cwd(),
  ?TRACE("Present working directory is ~s", [PWD]),

  %% Download the country info file
  spawn(?MODULE, http_get_request, [self(), "countryInfo", ".txt", get(trace)]),
  retry(wait_for_resources(1, text), text),
  
  ApplicationPid ! case parse_countries_file("countryInfo", ".txt") of
    {ok, Countries} -> {country_list, Countries};
    {error, Reason} -> {error, Reason}
  end.


%% -----------------------------------------------------------------------------
%%                            P R I V A T E   A P I
%% -----------------------------------------------------------------------------

%% -----------------------------------------------------------------------------
%% Retry download of files that previously failed
retry([], _) -> done;

retry(RetryList, Ext) ->
  ?TRACE("Waiting ~ws before retrying download of ~w files",[(?RETRY_WAIT div 1000), length(RetryList)]),
  Parent = self(),

  receive
  after ?RETRY_WAIT ->
    lists:foreach(fun({F,Ext1}) -> spawn(?MODULE, http_get_request, [Parent, F, Ext1, get(trace)]) end, RetryList),
    retry(wait_for_resources(length(RetryList), Ext), Ext)
  end.


%% -----------------------------------------------------------------------------
%% Check if the file needs to be updated. Files are considered stale after 24 hrs
check_for_update(CountryCode) ->
  case is_stale(?COUNTRY_FILE_FULL(CountryCode)) of
    true ->
      ?TRACE("Country file ~s.txt might be stale.  Checking for new version",[CountryCode]),
      spawn(?MODULE, http_get_request, [self(), CountryCode, ".zip", get(trace)]),
      retry(wait_for_resources(1, zip), zip);

    false -> []
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

  %% Find time difference between flie last modified time and now
  %% Convert standard DateTime to custom DateTime format by adding microseconds
  time_diff(?NOW, {Date,{H,M,S,0}}).

%% -----------------------------------------------------------------------------
%% Wait for a resource to be returned by HTTP request
wait_for_resources(Count, text) ->
  ?TRACE("Waiting for ~w text resources~n",[Count]),
  wait_for_resources(Count, write_file, []);
  
wait_for_resources(Count, zip)  ->
  ?TRACE("Waiting for ~w zip resources~n",[Count]),
  wait_for_resources(Count, handle_zip_file, []).
  
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
        _       -> write_file(TargetDir, Filename, ".etag", Etag)
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
      
        {other, Reason} ->
          io:format("Error: ~w requesting ~s~s~n", [Reason, ?GEONAMES_URL ++ Filename, Ext])
      end,

      RetryList ++ [{Filename, Ext}]
  end,

  wait_for_resources(Count-1, Fun, RetryList1).



%% -----------------------------------------------------------------------------  
%% Unzip only the data file from a zipped country file, then throw away the ZIP
%% file.
handle_zip_file(Dir, File, Ext, Body) ->
  ?TRACE("Country file ~s in directory ~s with content length ~w",[File, Dir, length(Body)]),

  ZipFile = Dir ++ File ++ Ext,
  TxtFile = Dir ++ File ++ ".txt",
  IntFile = Dir ++ File ++ "_int.txt",

  ?TRACE("Writing ZIP file ~s",[ZipFile]),
  
  write_file(Dir, File, Ext, Body),
  
  ?TRACE("Unzipping ~s to create ~s",[ZipFile, TxtFile]),

  {ok, [TxtFile]} = zip:unzip(ZipFile, [{file_list, [File ++ ".txt"]}, {cwd, Dir}]),

  %% If the ZIP file has an associated "_int.txt" file, delete that also
  case file:delete(IntFile) of
    ok         -> ok;
    {error, _} -> meh
  end,

  ok = file:delete(ZipFile).



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
      ?TRACE("ETag missing, sending normal GET request for ~s",[Url]),
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
%% Import either the internal feature class A & P country files if they exist,
%% else import the full country text file.
%% It is assumed that if the internal FCP file exists, then the internal FCA
%% file also exists.
country_file(CC) -> 
  country_file(CC, filelib:file_size(?COUNTRY_FILE_FCP(CC))).

%% -----------------------------------------------------------------------------
%% Import country file.
%% A non-zero size of the internal FCP country file is used to switch between
%% importing the internal files, or importing the full country file
country_file(CC, 0) ->
  ?TRACE("Internal FCA/FCP country files do not exist"),
  Filename = ?COUNTRY_FILE_FULL(CC),
  Filesize = filelib:file_size(Filename),

  ?TRACE("Importing ~s from full country file ~s", [format_as_binary_units(Filesize), Filename]),

  country_file(CC, file:open(Filename, [read]), Filesize);

%% Internal FCA and FCP country files exist
country_file(CC, FCP_Filesize) ->
  FCP_Filename = ?COUNTRY_FILE_FCP(CC),

  FCA_Filename = ?COUNTRY_FILE_FCA(CC),
  FCA_Filesize = filelib:file_size(FCA_Filename),

  ?TRACE("Importing ~s from internal FCA country file ~s",[format_as_binary_units(FCA_Filesize), FCA_Filename]),
  FCA_File = read_internal_country_file(file:read_file(FCA_Filename)),

  ?TRACE("Importing ~s from internal FCP country file ~s",[format_as_binary_units(FCP_Filesize), FCP_Filename]),
  FCP_File = read_internal_country_file(file:read_file(FCP_Filename)),

  % Restructure response to be consistent with country_file/3
  {fca, FCA_File, fcp, FCP_File}.

%% -----------------------------------------------------------------------------
%% Read full country file
country_file(CC, {ok, IoDevice}, Filesize) ->
  {FCA, FCP} = read_country_file(CC, IoDevice, {[],[]}, Filesize, trunc(Filesize * ?PROGRESS_FRACTION)),
  {fca, FCA, fcp, FCP};

country_file(CC, {error, Reason}, _) ->
  {error, io_lib:format("File ~s~s.txt: ~p",[?TARGET_DIR, CC, Reason])}.


%% -----------------------------------------------------------------------------
%% Read internal FCA or FCP country files
read_internal_country_file({ok, Data})      -> parse_internal_country_file(Data);
read_internal_country_file({error, Reason}) -> {error, Reason}.


%% -----------------------------------------------------------------------------
%% Parse internal FCA or FCP country file
%% Both files are Erlang lists of type #geoname_int
parse_internal_country_file(BinData) ->
  StrData        = erlang:binary_to_list(BinData),
  {ok, Terms, _} = erl_scan:string(StrData),
  {ok, Exprs}    = erl_parse:parse_exprs(Terms),
  {value, L, _}  = erl_eval:exprs(Exprs, []),

  L.


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
  LineTokens = string:tokens(DataLine,"\t"),
  [[Char1 | _] | _] = LineTokens,
  read_countries_file(IoDevice, io:get_line(IoDevice,""), get_country_code(Char1, LineTokens, Acc)).

get_country_code($#, _,                     Acc) -> Acc;
get_country_code(_,  [CountryCode | _Rest], Acc) -> Acc ++ [CountryCode].




%% -----------------------------------------------------------------------------
%% Read a country file and create a list of geoname records
read_country_file(CC, IoDevice, ListPair, Filesize, Stepsize) ->
  read_country_file(CC, IoDevice, io:get_line(IoDevice,""), ListPair, Filesize, Stepsize, 0).

%% Reached EOF, so close the input file, dump FCA and FCP records to disc
read_country_file(CC, IoDevice, eof, {FeatureClassA, FeatureClassP}, _, _, _) ->
  file:close(IoDevice),

  %% Now that we have a list of FCA and FCP records, import_country_info the country hierarchy
  %% server in order to add supplementary admin text to the FCP records.
  %% This server is only needed when an FCP file is being created
  HierarchyServer = list_to_atom("country_hierarchy_" ++ string:lowercase(CC)),
  country_hierarchy:init(HierarchyServer, FeatureClassA),

  %% Remember hierarchy server pid
  put(hierarchy_server_pid, whereis(HierarchyServer)),

  FeatureClassP1 = supplement_fcp_admin_text(HierarchyServer, FeatureClassP),

  %% Stop the hierarchy server because it is no longer needed
  HierarchyServer ! {cmd, stop},

  %% Since the feature code lists are Erlang terms, they must always be written
  %% to file with a terminating "." otherwise, when being read back in again,
  %% they will generate a syntax error in the term parser
  file:write_file(?COUNTRY_FILE_FCA(CC), io_lib:format("~p.",[FeatureClassA])),
  file:write_file(?COUNTRY_FILE_FCP(CC), io_lib:format("~p.",[FeatureClassP1])),

  country_manager ! {starting, file_import, get(my_name), complete},
  {FeatureClassA, FeatureClassP1};

%% Read country file when not eof
read_country_file(CC, IoDevice, DataLine, {FeatureClassA, FeatureClassP}, Filesize, Stepsize, Progress) ->
  {FCA_Filesize, Progress1} = report_progress(Filesize, Stepsize, length(DataLine), Progress),
  Rec = make_geoname_record(string:split(DataLine,"\t"), 1, #geoname_int{}),

  %% Do we want to keep this record?
  ListPair = case keep_geoname_record(Rec) of
    false     -> {FeatureClassA,          FeatureClassP};
    {true, a} -> {FeatureClassA ++ [Rec], FeatureClassP};
    {true, p} -> {FeatureClassA,          FeatureClassP ++ [Rec]}
  end,

  read_country_file(CC, IoDevice, io:get_line(IoDevice,""), ListPair, FCA_Filesize, Stepsize, Progress1).



%% -----------------------------------------------------------------------------
%% Read local eTag file
read_etag_file({ok, IoDevice}) -> read_etag_file(IoDevice, io:get_line(IoDevice,""), "");
read_etag_file({error, _})     -> missing;
read_etag_file(Filename)       -> read_etag_file(file:open(?TARGET_DIR ++ Filename ++ ".etag", [read])).

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
%% Write file to disc
write_file(Dir, Filename, Ext, Content) ->
  case file:write_file(Dir ++ Filename ++ Ext, Content) of
    ok              -> ok;
    {error, Reason} -> io:format("Writing file ~s failed. ~p~n",[Dir ++ Filename ++ Ext, Reason])
  end.


%% -----------------------------------------------------------------------------
%% Format size in binary units Kb, Mb or Gb
format_as_binary_units(N) when N < ?KB -> io_lib:format("~w bytes",[N]);
format_as_binary_units(N) when N < ?MB -> format_as_binary_units_int(N, ?KB, "Kb");
format_as_binary_units(N) when N < ?GB -> format_as_binary_units_int(N, ?MB, "Mb");
format_as_binary_units(N)              -> format_as_binary_units_int(N, ?GB, "Gb").

format_as_binary_units_int(N, Unit, UnitStr) ->
  WholeUnits = N div Unit,
  Rem = (N - (WholeUnits * Unit)) / Unit,
  io_lib:format("~.2f ~s",[WholeUnits + Rem, UnitStr]).


%% -----------------------------------------------------------------------------
%% Send a message to the country_manager for each unit of progress
report_progress(Filesize, Stepsize, Linesize, Progress) ->
  Chunk = Linesize + Progress,

  % Have we read enough data to report more progress?
  case (Chunk div Stepsize) > 0 of
    true -> country_manager ! {starting, file_import, get(my_name), progress, (Chunk div Stepsize)};
    _    -> ok
  end,

  {Filesize - Linesize, Chunk rem Stepsize}.
  


%% -----------------------------------------------------------------------------
%% Transform one line from a country file into a geoname record
%% Various fields are skipped to minimise the record size
make_geoname_record([[]],         _, Acc) -> Acc;

make_geoname_record([_V | Rest],  1, Acc) -> make_geoname_record(string:split(Rest,"\t"),  2, Acc#geoname_int{id   = val_or_undef(_V)});
make_geoname_record([_V | Rest],  2, Acc) -> make_geoname_record(string:split(Rest,"\t"),  3, Acc#geoname_int{name = val_or_undef(_V)});

%%make_geoname_record([_V | Rest],  3, Acc) -> make_geoname_record(string:split(Rest,"\t"),  4, Acc#geoname{asciiname      = val_or_undef(_V)});
make_geoname_record([_V | Rest],  3, Acc) -> make_geoname_record(string:split(Rest,"\t"),  4, Acc);

%%make_geoname_record([_V | Rest],  4, Acc) -> make_geoname_record(string:split(Rest,"\t"),  5, Acc#geoname{alternatenames = val_or_undef(_V)});
make_geoname_record([_V | Rest],  4, Acc) -> make_geoname_record(string:split(Rest,"\t"),  5, Acc);

make_geoname_record([_V | Rest],  5, Acc) -> make_geoname_record(string:split(Rest,"\t"),  6, Acc#geoname_int{latitude       = val_or_undef(_V)});
make_geoname_record([_V | Rest],  6, Acc) -> make_geoname_record(string:split(Rest,"\t"),  7, Acc#geoname_int{longitude      = val_or_undef(_V)});
make_geoname_record([_V | Rest],  7, Acc) -> make_geoname_record(string:split(Rest,"\t"),  8, Acc#geoname_int{feature_class  = val_or_undef(_V)});
make_geoname_record([_V | Rest],  8, Acc) -> make_geoname_record(string:split(Rest,"\t"),  9, Acc#geoname_int{feature_code   = val_or_undef(_V)});
make_geoname_record([_V | Rest],  9, Acc) -> make_geoname_record(string:split(Rest,"\t"), 10, Acc#geoname_int{country_code   = val_or_undef(_V)});

%%make_geoname_record([_V | Rest], 10, Acc) -> make_geoname_record(string:split(Rest,"\t"), 11, Acc#geoname{cc2            = val_or_undef(_V)});
make_geoname_record([_V | Rest], 10, Acc) -> make_geoname_record(string:split(Rest,"\t"), 11, Acc);

make_geoname_record([_V | Rest], 11, Acc) -> make_geoname_record(string:split(Rest,"\t"), 12, Acc#geoname_int{admin1         = val_or_undef(_V)});
make_geoname_record([_V | Rest], 12, Acc) -> make_geoname_record(string:split(Rest,"\t"), 13, Acc#geoname_int{admin2         = val_or_undef(_V)});
make_geoname_record([_V | Rest], 13, Acc) -> make_geoname_record(string:split(Rest,"\t"), 14, Acc#geoname_int{admin3         = val_or_undef(_V)});
make_geoname_record([_V | Rest], 14, Acc) -> make_geoname_record(string:split(Rest,"\t"), 15, Acc#geoname_int{admin4         = val_or_undef(_V)});

make_geoname_record([_V | Rest], 15, Acc) -> make_geoname_record(string:split(Rest,"\t"), 16, Acc#geoname_int{population     = val_or_undef(_V)});

%make_geoname_record([_V | Rest], 16, Acc) -> make_geoname_record(string:split(Rest,"\t"), 17, Acc#geoname{elevation      = val_or_undef(_V)});
make_geoname_record([_V | Rest], 16, Acc) -> make_geoname_record(string:split(Rest,"\t"), 17, Acc);

%make_geoname_record([_V | Rest], 17, Acc) -> make_geoname_record(string:split(Rest,"\t"), 18, Acc#geoname{dem            = val_or_undef(_V)});
make_geoname_record([_V | Rest], 17, Acc) -> make_geoname_record(string:split(Rest,"\t"), 18, Acc);

make_geoname_record([_V | Rest], 18, Acc) -> make_geoname_record(string:split(Rest,"\t"), 19, Acc#geoname_int{timezone       = val_or_undef(_V)});

%make_geoname_record([_V | Rest], 19, Acc) -> make_geoname_record(string:split(Rest,"\t"),  0, Acc#geoname{modified       = val_or_undef(_V)}).
make_geoname_record([_V | Rest], 19, Acc) -> make_geoname_record(string:split(Rest,"\t"),  0, Acc).

val_or_undef([]) -> undefined;
val_or_undef(V)  -> V.




%% -----------------------------------------------------------------------------
%% Filter out geoname records that not related to countries or administrative areas
keep_geoname_record(Rec) ->
  keep_feature_codes_for_class(Rec#geoname_int.feature_class, Rec, list_to_integer(Rec#geoname_int.population)).


%% -----------------------------------------------------------------------------
%% Administrative areas
keep_feature_codes_for_class("A", Rec, _Pop) ->
  case Rec#geoname_int.feature_code of
    "ADM1"  -> {true, a};
    "ADM2"  -> {true, a};
    "ADM3"  -> {true, a};
    "ADM4"  -> {true, a};
    "ADM5"  -> {true, a};
    "ADMD"  -> {true, a};
    "PCL"   -> {true, a};
    "PCLD"  -> {true, a};
    "PCLF"  -> {true, a};
    "PCLI"  -> {true, a};
    "PCLS"  -> {true, a};
    _       -> false
  end;

%% Only keep population centres having a population greater than some limit
%% For smaller countries, the situation might exist in which the administrative
%% centres have a population above the threshold, but all the individual
%% population centres are below the threshold
keep_feature_codes_for_class("P", Rec, Pop) when Pop > ?MIN_POPULATION ->
  case Rec#geoname_int.feature_code of
    "PPL"   -> {true, p};
    "PPLA"  -> {true, p};
    "PPLA2" -> {true, p};
    "PPLA3" -> {true, p};
    "PPLA4" -> {true, p};
    "PPLC"  -> {true, p};
    "PPLG"  -> {true, p};
    "PPLS"  -> {true, p};
    "PPLX"  -> {true, p};
    _       -> false
  end;

keep_feature_codes_for_class(_, _, _) -> false.

%% -----------------------------------------------------------------------------
%% Supplement FCP records with additional admin text
supplement_fcp_admin_text(HierarchyServer, FCP) ->
  [ HierarchyServer ! {name_lookup, FCPRec, self()} || FCPRec <- FCP ],
  wait_for_results(length(FCP), []).




%% -----------------------------------------------------------------------------
%% Wait for responses from country hierarchy server
wait_for_results(0, Acc) -> Acc;
wait_for_results(N, Acc) ->
  Acc1 = Acc ++ receive
    FCPRec when is_record(FCPRec, geoname_int) -> [FCPRec];
    _Whatever                                  -> []
  end,

  wait_for_results(N-1, Acc1).

