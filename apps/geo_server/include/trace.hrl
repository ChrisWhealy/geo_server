% *****************************************************************************
% Trace macro switches on when the compile flag 'debug' is defined
% *****************************************************************************
-define(FUNCTION_SIG, io_lib:format("~s:~s/~w ",[?MODULE,?FUNCTION_NAME,?FUNCTION_ARITY])).

%  -ifdef(debug).
%   -define(TRACE(Str),         io:fwrite("~s ~s~n",            [?FUNCTION_SIG, Str])).
%   -define(TRACE(FStr,Params), io:fwrite("~s" ++ FStr ++ "~n", [?FUNCTION_SIG] ++ Params)).
%   -else.
% -define(TRACE(_Str),          void).
% -define(TRACE(_FStr,_Params), void).
% -endif.

-define(TRACE(Str),
  (fun(S) ->
     case get(trace) of
       true -> io:fwrite("~s ~s~n", [?FUNCTION_SIG, S]);
       _    -> no_trace
     end
   end
  )
  (Str)
).

-define(TRACE(FStr,Params), 
  (fun(F, P) ->
     case get(trace) of
       true -> io:fwrite("~s " ++ F ++ "~n", [?FUNCTION_SIG] ++ P);
       _    -> no_trace
     end
   end
  )
  (FStr, Params)
).
