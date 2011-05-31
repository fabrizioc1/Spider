%%%----------------------------------------------------------------------
%%% File     : spider.erl
%%% Author   : Fabrizio Castrotorres <fabrizioc1@yahoo.com>
%%% Requires : Uri, Mochiweb
%%%----------------------------------------------------------------------

-module(spider).
-purpose('Parse a web page and find web page links within it').
-author('fabrizioc1@yahoo.com').
-include_lib("uri/include/uri.hrl").
-compile([export_all]).

%% erl -sname first -pa eunit-2.1.5\ebin uri-0.2.0\ebin
%% httpc:set_options([{proxy, {{"www-proxy.mycompany.com", 8000},["localhost"]}}]).
%% httpc:set_options([{proxy, {{"localhost", 8080}}}]).
%% httpc:set_options([{cookies, enabled}]).
%% UserAgentHeader = {"User-Agent","Mozilla/5.0 (Windows NT 6.0) AppleWebKit/534.24 (KHTML, like Gecko) Chrome/11.0.696.68 Safari/534.24"}.
%% {ok, {{Version, StatusCode, ReasonPhrase}, Headers, Body}} = httpc:request(get,{"http://www.sonystyle.com",[UserAgentHeader]},[{relaxed,true},{autoredirect,false}],[]).

%% Compile Mochiweb: 
%% lists:map(fun compile:file/1,filelib:wildcard("*.erl")).

%% Macros
-define(USER_AGENT_HEADER,{"User-Agent","Mozilla/5.0 (Windows NT 6.0) AppleWebKit/534.24 (KHTML, like Gecko) Chrome/11.0.696.68 Safari/534.24"}).
-define(LOG_FILE,"spider.log").
-define(LINKS_FILE,"spider-links.txt").
-define(MAX_DEPTH,5).
-define(DEBUG_MODE,true).

%%
%% Need to add regexp to get Javascript
%%
parse_links(Body) ->
    Regexp = "href\s*?=\s*?\"([^\"]+?)\"",
    case re:run(Body,Regexp,[{capture,all,list},global]) of
    {match,Matches} -> 
        [Link || [_Match,Link] <- Matches];
    nomatch -> 
        []
    end.    

%%
%% The given url must be normalized
%%
crawl(Url) -> 
    log_start(), 
    Config        = spider_config:config(Url),
    ExcludeFilter = spider_config:exclude_filter(Config),
    InitialLinks  = get_local_links(Url,ExcludeFilter),
    %%InitialLinks  = get_local_links(Url),    
    Links = crawl_links(InitialLinks,[Url],ExcludeFilter),
    log_stop(),
    Links.

%%
%% Merge the current set of links with the number of links found
%% Using the fact that the Set data structure removes duplicates
%%
merge_links(Current,Found) ->
    MergedSet = sets:from_list(Current++Found),
    sets:to_list(MergedSet).

unique_links2(Links) ->
    LinkSet = sets:from_list(Links),
    sets:to_list(LinkSet).

unique_links(List) ->
    Unique = fun(Item,UniqueList) ->
        case lists:member(Item,UniqueList) of
            false -> [Item|UniqueList];
            true -> UniqueList
        end
    end,
    lists:foldr(Unique,[],List).
    
set_difference(A,B) ->
    case sets:size(A) > sets:size(B) of
    true ->
        sets:subtract(A,B);
    false ->
        sets:subtract(B,A)
    end.

list_difference(A,B) ->
    X = sets:from_list(A),
    Y = sets:from_list(B),
    sets:to_list(set_difference(X,Y)).
    
current_links_not_found(Current,Found) ->
    CurrentSet = sets:from_list(Current),
    FoundSet = sets:from_list(Found),
    CurrentSetNotFound = sets:subtract(CurrentSet,sets:intersection(FoundSet,CurrentSet)),
    sets:to_list(CurrentSetNotFound).

is_subset(Current,Found) ->
    CurrentSet = sets:from_list(Current),
    FoundSet = sets:from_list(Found),
    sets:is_subset(CurrentSet,FoundSet).

%%
%% This is the tail recursive way to do it
%%
crawl_links([], LinksFound, _ExcludeFilter) -> LinksFound;
crawl_links([CurrentLink|OtherLinks], LinksFound, ExcludeFilter) ->
    log("[crawl_links] current link: ~p~n",[CurrentLink]),
    NewLinksFound = [CurrentLink]++LinksFound,
    case lists:member(CurrentLink,LinksFound) of
    true ->
        crawl_links(OtherLinks,LinksFound,ExcludeFilter);
    false ->
        CurrentLinks = get_local_links(CurrentLink,ExcludeFilter),
        
        %% Only crawl child links which have not been found
        CurrentLinksNotFound = current_links_not_found(CurrentLinks,NewLinksFound),
        ChildLinks = crawl_links(CurrentLinksNotFound,NewLinksFound,ExcludeFilter),
        
        %% Remove the child links from the other links to crawl
        OtherLinksNotFound = current_links_not_found(OtherLinks,ChildLinks),
        
        %% Append the child links to the links found so far        
        AllLinksFound = unique_links(lists:append(ChildLinks,NewLinksFound)),
        crawl_links(OtherLinksNotFound,AllLinksFound,ExcludeFilter)
    end.

%%
%% Returns raw set of links in a page and filters them with LinkFilter
%% User-Agent header is required in order to follow redirects
%%
get_links(Url,LinkFilter) ->
    Normalized = spider_uri:normalize_link(Url),
    %%log("[get_links] url: ~p~n",[Normalized]),    
    case httpc:request(get,{Normalized,[?USER_AGENT_HEADER]},[{autoredirect,false}],[]) of
    {ok, {{_Version, StatusCode, _ReasonPhrase}, Headers, _Body}} when StatusCode==301; StatusCode==302 -> 
        {_,Location} = lists:keyfind("location",1,Headers), 
        %%get_links(Location,LinkFilter);
        LinkFilter([Location],Url) ++ get_links(Location,LinkFilter);
    {ok, {{_Version, _StatusCode, _ReasonPhrase}, _Headers, Body}} -> 
        LinkFilter(parse_links(Body),Url);
    {error,Reason} ->
        log("[get_links] error: ~p~n",[Reason]),
        []
    end.

raw_links(Url) -> get_links(Url,fun(Links,_)-> Links end).

get_local_links(Url) -> 
    NormalizedAndLocal = fun(Links,Parent) -> local_links(unique_links(normalize_links(Links,Parent)),Parent) end,    
    get_links(Url,NormalizedAndLocal).
    
get_local_links(Url,ExcludeFilter) -> 
    IncludeFilter = fun(Link) -> not ExcludeFilter(Link) end,
    lists:filter(IncludeFilter,get_local_links(Url)).
    
get_local_html_links(Url) -> html_links(get_local_links(Url)).

get_unique_local_links(Url) -> unique_links(get_local_links(Url)).
    
normalize_links(Links,Parent) ->
    ValidLinks = lists:filter(fun spider_uri:is_valid_link/1, Links),
    lists:map(fun(Y) -> spider_uri:normalize_link(Y,Parent) end, ValidLinks).

%% The links must be normalized
local_links(Links,Parent) -> 
    IsLocalLink = fun(X) -> spider_uri:is_local_link(X,Parent) end,
    lists:filter(IsLocalLink,Links).
    
%% Don't get images CSS or JavaScript
html_links(Links) -> 
    IsHtmlLink = fun(X) -> spider_uri:is_html_link(X) end,
    lists:filter(IsHtmlLink,Links).
    
%%
%% Write the links to a file so that they can be loaded using file:consult()
%% Example: {ok,[Links]}=file:consult("raw_links.txt").
%%
write_links(Links) -> 
    write_links(Links,?LINKS_FILE).    
write_links(Links,Filename) ->
    case file:open(Filename, write) of
    {ok, Fd} ->
        %%lists:map(fun(X) -> io:fwrite(Fd,"~p,~n",[X]) end, Links),
        io:fwrite(Fd,"~p.~n",[Links]),
        file:close(Fd);
    {error, Reason} ->
        {error, Reason}
    end.

log(Str) -> log(Str,[]).
log(Str,Args) -> file:write_file(?LOG_FILE,io_lib:fwrite(Str,Args),[append]).

log_if(Assertion,Str) -> log_if(Assertion,Str,[]).
log_if(Assertion,Str,Args) when Assertion -> log(Str,Args);
log_if(_Assertion,_Str,_Args) -> nomatch.

log_start() -> log_mark("started",[]).    
log_stop() -> log_mark("stopped",[append]).

log_mark(Message,Modes) ->
    file:write_file(?LOG_FILE,io_lib:fwrite("spider ~s on ~s~n",[Message,log_timestamp()]),Modes).

log_timestamp() ->
    {{Year,Mon,Day},{Hour,Min,Sec}} = erlang:localtime(),    
    io_lib:fwrite("~w/~2.2.0w/~2.2.0w ~2.2.0w:~2.2.0w:~2.2.0w",[Year,Mon,Day,Hour,Min,Sec]).

    
    
