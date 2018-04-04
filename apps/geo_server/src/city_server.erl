-module(city_server).

-author("Chris Whealy <chris.whealy@sap.com>").
-revision("Revision: 1.0.0").
-created("Date: 2018/02/19 17:31:51").
-created_by("chris.whealy@sap.com").

-export([init/3]).

-include("../include/trace.hrl").
-include("../include/geoname.hrl").


%% -----------------------------------------------------------------------------
%%                             P U B L I C   A P I
%% -----------------------------------------------------------------------------
init(ParentPid, Id, CityList) ->
  put(trace, false),
  wait_for_msg(ParentPid, Id, CityList).


%% -----------------------------------------------------------------------------
%%                            P R I V A T E   A P I
%% -----------------------------------------------------------------------------

%% -----------------------------------------------------------------------------
wait_for_msg(ParentPid, Id, CityList) ->
  receive
    %% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    %% Process commands
    {cmd, Cmd} ->
      case Cmd of
        city_list  -> io:format("~s~n", [lists:flatten([C#geoname_int.name ++ ", " || C <- CityList])]);
        city_stats -> io:format("City process ~c (~p) holds ~w cities~n", [Id, self(), length(CityList)]);
        trace_on   -> put(trace, true);
        trace_off  -> put(trace, false);
        shutdown   -> exit(normal)
      end,

      wait_for_msg(ParentPid, Id, CityList);

    %% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    %% Add new city
    {add, City} ->
      ?TRACE("Adding city ~s",[City#geoname_int.name]),
      wait_for_msg(ParentPid, Id, CityList ++ [City]);

    %% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    %% Search city list
    %%
    %% When testing regular expressions in the Erlang REPL, remember that the REPL
    %% absorbs one level of escape characters, so regular expression metacharcters
    %% such as \s or \b must be double escaped!
    {Ref, {search_term, Query, whole_word, WholeWord, starts_with, StartsWith}, _CountryCode, Query, CountryServerPid} ->
      ?TRACE("Searching for ~s ~s cities for \"~s\" amongst ~w names",[_CountryCode, Id, Query, length(CityList)]),
      {ok, MatchPattern} = re:compile(make_reg_exp(Query, WholeWord, StartsWith),[unicode, caseless]),
      Results = [C || C <- CityList, regexp_hit(re:run(C#geoname_int.name, MatchPattern))],

      CountryServerPid ! {results, Ref, Id, Results},
      wait_for_msg(CountryServerPid, Id, CityList)
  end.


%% -----------------------------------------------------------------------------
regexp_hit({match, _}) -> true;
regexp_hit(nomatch)    -> false.

%% -----------------------------------------------------------------------------
%% Construct a regular expression from the parameters
%% Second parameter = "whole word" flag
%% Third parameter  = "starts with" flag
%%
%% Double escape characters are also needed in the source code... (Weird!)
make_reg_exp(Q, true, false)  -> "\\b(" ++ Q ++ ")\\b";
make_reg_exp(Q, true,  true)  ->   "^(" ++ Q ++ ")\\b";
make_reg_exp(Q, false, true)  ->   "^(" ++ Q ++ ")";
make_reg_exp(Q, false, false) ->    "(" ++ Q ++ ")".



