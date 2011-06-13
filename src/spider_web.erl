%%%----------------------------------------------------------------------
%%% File     : spider_web.erl
%%% Author   : Fabrizio Castrotorres <fabrizioc1@yahoo.com>
%%% Requires : Uri, Mochiweb
%%%----------------------------------------------------------------------

-module(spider_web).
-purpose('Parse a web page and find web page links within it').
-author('fabrizioc1@yahoo.com').
-include_lib("uri/include/uri.hrl").
-compile([export_all]).

%%
%% SitemapUrl="http://www.sonystyle.com/webapp/wcs/stores/servlet/SYSiteMapView?storeId=10151&catalogId=10551&langId=-1".
%% {SyncTime,SyncLinks} = timer:tc(spider_web,find_common_links_sync,[SitemapUrl]).
%%
find_links_sync(SitemapUrl) ->
    ExcludeFilter = spider_config:exclude_filter(SitemapUrl),
    GetLinks      = fun(Link) -> spider:get_local_links(Link,ExcludeFilter) end,
    SitemapLinks  = GetLinks(SitemapUrl),
    ChildLinks    = lists:map(GetLinks,SitemapLinks),
    LinkCollector = fun(X,Acc) -> lists:append(Acc,X) end,
    spider:unique_links(lists:foldl(LinkCollector,[],ChildLinks)).

%%
%% spider:write_links(AllLinks,"find_common_links_async.txt"), 
%% {AsyncTime,AsyncLinks} = timer:tc(spider_web,find_common_links_async,[SitemapUrl]).
%%
find_links_async(SitemapUrl) ->
    ExcludeFilter = spider_config:exclude_filter(SitemapUrl),
    SitemapLinks  = spider:get_local_links(SitemapUrl,ExcludeFilter),
    CollectorPid  = collector_start(self(),SitemapLinks),
    [spawn(fun()-> get_links(CollectorPid,Link,ExcludeFilter) end) || Link <- SitemapLinks],
    receive
        {done,Links} -> Links
    end.

%%
%% Shortcuts to retrieve child links from a url
%%
get_links(Url) ->
    ExcludeFilter = spider_config:exclude_filter(Url),    
    spider:get_local_links(Url,ExcludeFilter).
    
get_links(ParentPid,Url) ->
    ParentPid ! {self(),merge,Url,get_links(Url)}.

get_links(ParentPid,Url,ExcludeFilter) ->
    ParentPid ! {self(),merge,Url,spider:get_local_links(Url,ExcludeFilter)}.
    
get_links_function(Url) ->
    ExcludeFilter = spider_config:exclude_filter(Url),    
    fun(X) -> spider:get_local_links(X,ExcludeFilter) end.

%%
%% May want to move collector to another file
%%
collector_start(ReplyPid,ParentLinks) ->
    spawn(fun() -> collector_loop(ReplyPid,ParentLinks,[],[]) end).

collector_loop(ReplyPid,ParentLinks,CollectedLinks,CurrentLinks) when length(ParentLinks) =:= length(CollectedLinks) -> 
    UniqueLinks = sets:to_list(sets:from_list(CurrentLinks)),
    ReplyPid ! {done, UniqueLinks};

collector_loop(ReplyPid,ParentLinks,CollectedLinks,CurrentLinks) ->
    receive
        {_From,merge,Url,FoundLinks} ->
            %%io:format("[Collector] Merging links from ~p~n",[Url]),
            %%NewCollectedLinks = lists:sort(CollectedLinks++[Url]), 
            %%MergedLinks = sets:to_list(sets:from_list(CurrentLinks++FoundLinks)),
            NewCurrentLinks = CurrentLinks++FoundLinks,                        
            collector_loop(ReplyPid,ParentLinks,[Url|CollectedLinks],NewCurrentLinks);
        {From,stop} ->
            io:format("[Collector] Received: stop, Sender: ~p~n",[From]),
            spider:write_links(CurrentLinks);
        {From,list} ->
            io:format("[Collector] Received: list, Sender: ~p~n",[From]),
            io:format("Links: ~p~n",[CurrentLinks]),
            collector_loop(ReplyPid,ParentLinks,CollectedLinks,CurrentLinks)
    end.
