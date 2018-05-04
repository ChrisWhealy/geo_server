%% =====================================================================================================================
%% Get memory usage of a particular process
%% =====================================================================================================================
memory_usage(undefined)   -> 0;
memory_usage("undefined") -> 0;
memory_usage("Undefined") -> 0;

memory_usage(Pid) when is_pid(Pid) ->
  case process_info(Pid, memory) of
    undefined   -> 0;
    {memory, N} -> N
  end;

memory_usage(ProcessName) ->
  memory_usage(whereis(ProcessName)).


