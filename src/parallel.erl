%%%----------------------------------------------------------------------
%%% File     : parallel.erl
%%% Author   : Fabrizio Castrotorres <fabrizioc1@yahoo.com>
%%% Purpose  : Higher order functions which execute in parallel
%%% Requires : Uri, Mochiweb
%%%----------------------------------------------------------------------

-module(parallel).
-purpose('Higher order functions which execute in parallel').
-author('fabrizioc1@yahoo.com').
-export([map/2]).
%%-compile([export_all]).

%%
%% Parallel map implementation
%% Since the workers execute in parallel the order of the output is different from the input
%%
map(Function,InputList) ->
    FoldFunction   = fun(Y,Acc) -> [Y|Acc] end,
    CollectorPid   = collector_start(self(),length(InputList),[],FoldFunction),
    WorkerFunction = fun(X) -> CollectorPid ! {merge,Function(X)} end,
    [spawn(fun()-> WorkerFunction(Item) end) || Item <- InputList],
    receive
        {done,Results} -> Results;
        Message -> io:format("Map terminated with unexpected message: ~p~n",[Message])        
    end.
    
%%
%% Output collector
%%
collector_start(MapPid,Size,Initial,Function) ->
    spawn(fun() -> collector_loop(MapPid,0,Size,Initial,Function) end).

collector_loop(MapPid,Count,Size,Results,_) when Count >= Size -> 
    MapPid ! {done, Results};

collector_loop(MapPid,Count,Size,Results,Function) ->
    receive
        {merge,Output} -> 
            %%io:format("Results: ~p~n",[Results]),
            collector_loop(MapPid,Count+1,Size,Function(Output,Results),Function);
        Message ->
            io:format("Collector terminating with unexpected message: ~p~n",[Message])
    end.
