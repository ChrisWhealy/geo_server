-module(country_file_handler).

-author("Chris Whealy <chris.whealy@sap.com>").
-revision("Revision: 1.0.0").
-created("Date: 2018/04/04 18:20:43").
-created_by("chris.whealy@sap.com").

-export([
    country_file/2
  ]).

-include("../include/records/geoname.hrl").

-include("../include/macros/trace.hrl").
-include("../include/macros/file_paths.hrl").

-define(PROGRESS_FRACTION, 0.01).

%% Feature class P records (Population centres) having a population below this limit will not be included in a
%% country's FCP file
-define(MIN_POPULATION, 500).


%% =====================================================================================================================
%%
%%                                                 P U B L I C   A P I
%%
%% =====================================================================================================================

%% ---------------------------------------------------------------------------------------------------------------------
%% Import the internal FCP file if it exists, else import the full country text file.
country_file(CC, CountryServerPid) when is_pid(CountryServerPid) ->
  {registered_name, CountryServerName} = erlang:process_info(CountryServerPid, registered_name),

  put(my_name, CountryServerName),
  put(trace, process_tools:read_process_dictionary(CountryServerPid, trace)),

  country_file_int(CC, filelib:file_size(?COUNTRY_FILE_FCP(CC)), CountryServerPid).



%% =====================================================================================================================
%%
%%                                                P R I V A T E   A P I
%%
%% =====================================================================================================================

%% ---------------------------------------------------------------------------------------------------------------------
%% Read full country file
country_file_int(CC, {ok, IoDevice}, Filesize) ->
  read_country_file(CC, IoDevice, {[],[]}, Filesize, trunc(Filesize * ?PROGRESS_FRACTION));

country_file_int(CC, {error, Reason}, _) ->
  {error, io_lib:format("File ~s~s.txt: ~p",[?TARGET_DIR, CC, Reason])};

%% The FCP.txt file has zero size, so download a new copy of the country's ZIP file from GeoNames.org, extract the text
%% file and generate a new FCP.txt file
country_file_int(CC, 0, CountryServerPid) when is_pid(CountryServerPid) ->
  Filesize = filelib:file_size(?COUNTRY_FILE_FULL(CC)),
  ?TRACE("Internal FCP file does not exist. Importing ~s from full country file ~s", [format:as_binary_units(Filesize), ?COUNTRY_FILE_FULL(CC)]),
  CountryServerPid ! country_file_int(CC, file:open(?COUNTRY_FILE_FULL(CC), [read]), Filesize);

%% Import the internal <CC>_fcp.txt file
country_file_int(CC, FCP_Filesize, CountryServerPid) when is_pid(CountryServerPid) ->
  ?TRACE("Importing ~s from internal FCP country file ~s",[format:as_binary_units(FCP_Filesize), ?COUNTRY_FILE_FCP(CC)]),
  {ok, [FCP_Data | _]} = file:consult(?COUNTRY_FILE_FCP(CC)),
  CountryServerPid ! FCP_Data.


%% ---------------------------------------------------------------------------------------------------------------------
%% Read a country file and create a list of geoname records
read_country_file(CC, IoDevice, ListPair, Filesize, Stepsize) ->
  read_country_file(CC, IoDevice, io:get_line(IoDevice,""), ListPair, Filesize, Stepsize, 0).

%% Reached EOF, so close the input file, dump FCA and FCP records to disc
read_country_file(CC, IoDevice, eof, {FeatureClassA, FeatureClassP}, _, _, _) ->
  file:close(IoDevice),

  %% Now that we have a list of FCA and FCP records, start the country hierarchy server in order to transfer
  %% supplementary admin text from the FCA records into the FCP records.  This server is only needed whilst the FCP file
  %% is being created.  After that, both the hiererachy_server and the FCA data can be deleted
  HierarchyServer = list_to_atom("country_hierarchy_" ++ string:lowercase(CC)),
  country_hierarchy:init(HierarchyServer, FeatureClassA),

  %% Remember the hierarchy server's pid
  put(hierarchy_server_pid, whereis(HierarchyServer)),

  FeatureClassP1 = supplement_fcp_admin_text(HierarchyServer, FeatureClassP),

  %% Stop the hierarchy server because it is no longer needed
  HierarchyServer ! {cmd, stop},

  %% Since the feature code lists are Erlang terms, they must always be written to file with a terminating "."
  %% otherwise, when being read back in again, they will generate a syntax error in the term parser
  file:write_file(?COUNTRY_FILE_FCP(CC), io_lib:format("~p.",[FeatureClassP1])),

  country_manager ! {starting, file_import, get(my_name), progress, complete},
  FeatureClassP1;

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


%% ---------------------------------------------------------------------------------------------------------------------
%% Send a message to the country_manager for each unit of progress
report_progress(Filesize, Stepsize, Linesize, Progress) ->
  Chunk = Linesize + Progress,

  % Have we read enough data to report more progress?
  case (Chunk div Stepsize) > 0 of
    true -> country_manager ! {starting, file_import, get(my_name), progress, (Chunk div Stepsize)};
    _    -> ok
  end,

  {Filesize - Linesize, Chunk rem Stepsize}.
  


%% ---------------------------------------------------------------------------------------------------------------------
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

%% ---------------------------------------------------------------------------------------------------------------------
%% Transform string data to binary for use in the geoname_int records
bin_or_undef([]) -> undefined;
bin_or_undef(V)  -> list_to_binary(V).


%% ---------------------------------------------------------------------------------------------------------------------
%% Filter out geoname records that not related to countries or administrative areas
keep_geoname_record(Rec) ->
  keep_feature_codes_for_class(Rec#geoname_int.feature_class, Rec, binary_to_integer(Rec#geoname_int.population)).


%% ---------------------------------------------------------------------------------------------------------------------
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

%% Only keep population centres having a population greater than the limit defined in ?MIN_POPULATION
%% For smaller countries, the situation might exist in which the administrative centres have a population above the
%% threshold, but all the individual population centres within it are below the threshold.  This will result in zero
%% population centres being extracted
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


%% ---------------------------------------------------------------------------------------------------------------------
%% Supplement FCP records with additional admin text
supplement_fcp_admin_text(HierarchyServer, FCP) ->
  [ HierarchyServer ! {name_lookup, FCPRec, self()} || FCPRec <- FCP ],
  wait_for_results(length(FCP), []).



%% ---------------------------------------------------------------------------------------------------------------------
%% Wait for responses from country hierarchy server
wait_for_results(0, Acc) -> Acc;
wait_for_results(N, Acc) ->
  Acc1 = Acc ++ receive
    FCPRec when is_record(FCPRec, geoname_int) -> [FCPRec];
    _Whatever                                  -> []
  end,

  wait_for_results(N-1, Acc1).


