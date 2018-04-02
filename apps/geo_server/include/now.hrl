%% -----------------------------------------------------------------------------
%% Since the NOW macro uses an anonymous function, its internal variables exist
%% in the scope of the calling function; therefore, the variable names used here
%% are longer to reduce the risk of an invisible name clash which then leads to
%% weird "no_match" exceptions etc.
%% -----------------------------------------------------------------------------
-define(NOW, (fun({_,_,Micro} = TS) ->
    {{YYYY_temp,MM_temp,DD_temp},{H_temp,M_temp,S_temp}} = calendar:now_to_local_time(TS),
    {{YYYY_temp,MM_temp,DD_temp},{H_temp,M_temp,S_temp,Micro}}
   end)
   (erlang:timestamp())
  ).

