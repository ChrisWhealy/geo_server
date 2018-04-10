-module(country_file_handler).

-author("Chris Whealy <chris.whealy@sap.com>").
-revision("Revision: 1.0.0").
-created("Date: 2018/04/04 18:20:43").
-created_by("chris.whealy@sap.com").

-export([
  country_file/1
]).

-include("../include/trace.hrl").
-include("../include/time_utils.hrl").
-include("../include/format_utils.hrl").
-include("../include/file_paths.hrl").
-include("../include/rec_geoname.hrl").

-define(PROGRESS_FRACTION, 0.01).

%% Feature class P records (Population centres) having a pop[ulation below this
%% limit will not be included in a country's FCP file
-define(MIN_POPULATION, 500).


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
  ?TRACE("Importing ~s from internal FCP country file ~s",[format_as_binary_units(FCP_Filesize), FCP_Filename]),

  FCA_File = read_internal_country_file(file:read_file(FCA_Filename)),
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
read_internal_country_file({ok, BinData})   -> parse_internal_country_file(BinData);
read_internal_country_file({error, Reason}) -> {error, Reason}.


%% -----------------------------------------------------------------------------
%% Parse internal FCA or FCP country file
%% Both files are Erlang lists of type #geoname_int
parse_internal_country_file(BinData) ->
  country_manager ! {starting, loading_internal_file, get(my_name), progress, 0},
  StrData        = erlang:binary_to_list(BinData),

  country_manager ! {starting, scanning_internal_file, get(my_name), progress, 25},
  {ok, Terms, _} = erl_scan:string(StrData),

  country_manager ! {starting, parsing_internal_file, get(my_name), progress, 25},
  {ok, Exprs}    = erl_parse:parse_exprs(Terms),

  country_manager ! {starting, evaluating_internal_file, get(my_name), progress, 25},
  {value, L, _}  = erl_eval:exprs(Exprs, []),
  
  country_manager ! {starting, file_import, get(my_name), progress, complete},
  L.


%% -----------------------------------------------------------------------------
%% Read a country file and create a list of geoname records
read_country_file(CC, IoDevice, ListPair, Filesize, Stepsize) ->
  read_country_file(CC, IoDevice, io:get_line(IoDevice,""), ListPair, Filesize, Stepsize, 0).

%% Reached EOF, so close the input file, dump FCA and FCP records to disc
read_country_file(CC, IoDevice, eof, {FeatureClassA, FeatureClassP}, _, _, _) ->
  file:close(IoDevice),

  %% Now that we have a list of FCA and FCP records, start the country hierarchy
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

  country_manager ! {starting, file_import, get(my_name), progress, complete},
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
make_geoname_record([[]], _, Acc) -> Acc;

%% #geoname.id             => #geoname_int.id
%% #geoname.name           => #geoname_int.name
%% #geoname.asciiname      => Don't care
%% #geoname.alternatenames => Don't care
%% #geoname.latitude       => #geoname_int.latitude
%% #geoname.longitude      => #geoname_int.longitude
%% #geoname.feature_class  => #geoname_int.feature_class
%% #geoname.feature_code   => #geoname_int.feature_code
%% #geoname.country_code   => #geoname_int.country_code
%% #geoname.cc2            => Don't care
%% #geoname.admin1         => #geoname_int.admin1
%% #geoname.admin2         => #geoname_int.admin2
%% #geoname.admin3         => #geoname_int.admin3
%% #geoname.admin4         => #geoname_int.admin4
%% #geoname.population     => #geoname_int.population
%% #geoname.elevation      => Don't care
%% #geoname.dem            => Don't care
%% #geoname.timezone       => #geoname_int.timezone
%% #geoname.modified       => Don't care

make_geoname_record([V  | Rest],  1, Acc) -> make_geoname_record(string:split(Rest,"\t"),  2, Acc#geoname_int{id   = bin_or_undef(V)});
make_geoname_record([V  | Rest],  2, Acc) -> make_geoname_record(string:split(Rest,"\t"),  3, Acc#geoname_int{name = bin_or_undef(V)});
make_geoname_record([_V | Rest],  3, Acc) -> make_geoname_record(string:split(Rest,"\t"),  4, Acc);
make_geoname_record([_V | Rest],  4, Acc) -> make_geoname_record(string:split(Rest,"\t"),  5, Acc);
make_geoname_record([V  | Rest],  5, Acc) -> make_geoname_record(string:split(Rest,"\t"),  6, Acc#geoname_int{latitude       = bin_or_undef(V)});
make_geoname_record([V  | Rest],  6, Acc) -> make_geoname_record(string:split(Rest,"\t"),  7, Acc#geoname_int{longitude      = bin_or_undef(V)});
make_geoname_record([V  | Rest],  7, Acc) -> make_geoname_record(string:split(Rest,"\t"),  8, Acc#geoname_int{feature_class  = bin_or_undef(V)});
make_geoname_record([V  | Rest],  8, Acc) -> make_geoname_record(string:split(Rest,"\t"),  9, Acc#geoname_int{feature_code   = bin_or_undef(V)});
make_geoname_record([V  | Rest],  9, Acc) -> make_geoname_record(string:split(Rest,"\t"), 10, Acc#geoname_int{country_code   = bin_or_undef(V)});
make_geoname_record([_V | Rest], 10, Acc) -> make_geoname_record(string:split(Rest,"\t"), 11, Acc);
make_geoname_record([V  | Rest], 11, Acc) -> make_geoname_record(string:split(Rest,"\t"), 12, Acc#geoname_int{admin1         = bin_or_undef(V)});
make_geoname_record([V  | Rest], 12, Acc) -> make_geoname_record(string:split(Rest,"\t"), 13, Acc#geoname_int{admin2         = bin_or_undef(V)});
make_geoname_record([V  | Rest], 13, Acc) -> make_geoname_record(string:split(Rest,"\t"), 14, Acc#geoname_int{admin3         = bin_or_undef(V)});
make_geoname_record([V  | Rest], 14, Acc) -> make_geoname_record(string:split(Rest,"\t"), 15, Acc#geoname_int{admin4         = bin_or_undef(V)});
make_geoname_record([V  | Rest], 15, Acc) -> make_geoname_record(string:split(Rest,"\t"), 16, Acc#geoname_int{population     = bin_or_undef(V)});
make_geoname_record([_V | Rest], 16, Acc) -> make_geoname_record(string:split(Rest,"\t"), 17, Acc);
make_geoname_record([_V | Rest], 17, Acc) -> make_geoname_record(string:split(Rest,"\t"), 18, Acc);
make_geoname_record([V  | Rest], 18, Acc) -> make_geoname_record(string:split(Rest,"\t"), 19, Acc#geoname_int{timezone       = bin_or_undef(V)});
make_geoname_record([_V | Rest], 19, Acc) -> make_geoname_record(string:split(Rest,"\t"),  0, Acc).

%% -----------------------------------------------------------------------------
%% Transform string data to binary for use in the geoname_int records
bin_or_undef([]) -> undefined;
bin_or_undef(V)  -> list_to_binary(V).

%% -----------------------------------------------------------------------------
%% Printable format of a geoname_int record
% format_geoname_int_record(R) ->
%   lists:flatten([io_lib:format("~p = ~p, ",[K,V]) || {K,V} <- kv_geoname_int_record(R)]).

%% Create a KV list from a geoname_int record and a record instance
% kv_geoname_int_record(R) ->
%   lists:zip(record_info(fields,geoname_int), tl(tuple_to_list(R))).



%% -----------------------------------------------------------------------------
%% Filter out geoname records that not related to countries or administrative areas
keep_geoname_record(Rec) ->
  keep_feature_codes_for_class(Rec#geoname_int.feature_class, Rec, binary_to_integer(Rec#geoname_int.population)).


%% -----------------------------------------------------------------------------
%% Administrative areas
keep_feature_codes_for_class(<<"A">>, Rec, _Pop) ->
  case Rec#geoname_int.feature_code of
    <<"ADM1">>  -> {true, a};
    <<"ADM2">>  -> {true, a};
    <<"ADM3">>  -> {true, a};
    <<"ADM4">>  -> {true, a};
    <<"ADM5">>  -> {true, a};
    <<"ADMD">>  -> {true, a};
    <<"PCL">>   -> {true, a};
    <<"PCLD">>  -> {true, a};
    <<"PCLF">>  -> {true, a};
    <<"PCLI">>  -> {true, a};
    <<"PCLS">>  -> {true, a};
    _           -> false
  end;

%% Only keep population centres having a population greater than some limit
%% For smaller countries, the situation might exist in which the administrative
%% centres have a population above the threshold, but all the individual
%% population centres are below the threshold
keep_feature_codes_for_class(<<"P">>, Rec, Pop) when Pop >= ?MIN_POPULATION ->
  case Rec#geoname_int.feature_code of
    <<"PPL">>   -> {true, p};
    <<"PPLA">>  -> {true, p};
    <<"PPLA2">> -> {true, p};
    <<"PPLA3">> -> {true, p};
    <<"PPLA4">> -> {true, p};
    <<"PPLC">>  -> {true, p};
    <<"PPLG">>  -> {true, p};
    <<"PPLS">>  -> {true, p};
    <<"PPLX">>  -> {true, p};
    _           -> false
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

