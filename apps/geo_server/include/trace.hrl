% **********************************************************************************************************************
% Trace macro switches on when the compile flag 'debug' is defined
% **********************************************************************************************************************
-define(FUNCTION_SIG, io_lib:format("~s:~s/~w ",[?MODULE,?FUNCTION_NAME,?FUNCTION_ARITY])).

-define(TRACE(Str),
  case get(trace) of
    true -> (fun(S) -> io:fwrite("~s ~s~n", [?FUNCTION_SIG, S]) end)(Str);
    _    -> no_trace
  end
).

-define(TRACE(FStr,Params), 
  case get(trace) of
    true -> (fun(F, P) -> io:fwrite("~s " ++ F ++ "~n", [?FUNCTION_SIG] ++ P) end)(FStr, Params);
    _    -> no_trace
  end
).


%% ---------------------------------------------------------------------------------------------------------------------
%% Locate a value in the process dictionary of some other process
read_process_dictionary(Pid, Name) ->
  {dictionary, Dict} = erlang:process_info(Pid, dictionary),
  search_dictionary(Name, Dict).

search_dictionary(_,    [])                     -> undefined;
search_dictionary(Name, [{Name, Value} | _])    -> Value;
search_dictionary(Name, [{_, _}        | Rest]) -> search_dictionary(Name, Rest).
