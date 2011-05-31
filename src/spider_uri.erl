%%%----------------------------------------------------------------------
%%% File    : spider_uri.erl
%%% Author  : Fabrizio Castrotorres <fabrizioc1@yahoo.com>
%%% Requires : uri, mochiweb_util, strre (my custom version)
%%%----------------------------------------------------------------------

-module(spider_uri).
-purpose('URL normalization and parsing for use by spider module').
-author('fabrizioc1@yahoo.com').
-include_lib("uri/include/uri.hrl").
-compile([export_all]).
%% -export([from_string/1,normalize_link/1,is_valid_link/1,is_local_link/1]).

%%
%% This is more robust versino of uri:from_string() and won't throw exceptions on bad URLs
%%
from_string(Url) ->
    {Scheme, Netloc, Path, Query, Fragment} = mochiweb_util:urlsplit(Url),
    {Host,Port} = host_and_port(Netloc),
    #uri{scheme=Scheme,host=Host,port=Port,path=Path,raw_query=Query,frag=Fragment,raw=Url}.

%% Normalize a url using the parent as a template
normalize_link(Link,Parent) -> 
    Uri       = from_string(fix_scheme(Link,Parent)),
    ParentUri = from_string(Parent),
    FullUri   = fix_ssl(normalize_path(add_scheme_and_host(Uri,ParentUri),ParentUri)),
    uri:raw(uri:frag(FullUri,"")).

%% If we don't have a template to use to normalize, then we can only fix the SSL bug and remove the anchor
normalize_link(Link) -> 
    FullUri = fix_ssl(from_string(Link)),
    uri:raw(uri:frag(FullUri,"")).

%% If path is relative, prefix it with the root of the parent
normalize_path(Uri,ParentUri) ->
    Path       = Uri#uri.path,
    ParentPath = ParentUri#uri.path,        
    case strre:starts_with(Path,"/") of
    false ->
        NormalizedPath = string:substr(ParentPath,1,string:rstr(ParentPath,"/"))++Path,
        Uri#uri{path=NormalizedPath};
    true ->
        Uri
    end.
    
%%
%% Different kinds of link tests
%%
is_valid_link(Link) -> (not is_javascript(Link)) andalso (not is_anchor(Link)).

%% TODO: Handle sub-domains
is_local_link(Link,Parent) -> host(Link) == host(Parent).

is_html_link(Link) -> 
    not is_image_link(Link) andalso 
    not strre:ends_with(Link,".css") andalso
    not strre:ends_with(Link,".js").
    
is_image_link(Link) ->
    strre:ends_with(Link,".gif") orelse    
    strre:ends_with(Link,".jpg") orelse    
    strre:ends_with(Link,".png").

is_javascript(Url) -> strre:starts_with(Url,"javascript:").

is_anchor(Url) -> strre:starts_with(Url,"#").

%%
%% Helpers to get URL parts without updating the URI record
%%
host(Url) -> Uri = from_string(Url), Uri#uri.host.

port(":"++Port) -> port(Port);
port(Port) when Port =:= "" -> Port;
port(Port) -> list_to_integer(Port).

host_and_port(Netloc) -> 
    {Host,PortStr} = lists:splitwith(fun(C)-> C =/= $: end,Netloc),    
    {Host,port(PortStr)}.

path(Url) -> Uri = from_string(Url), Uri#uri.path.

raw_query(Url) -> Uri = from_string(Url), Uri#uri.raw_query.

%% Add missing URL parts
add_scheme(Uri,ParentUri) -> 
    case Uri#uri.scheme == "" of
        true -> Uri#uri{scheme=ParentUri#uri.scheme};
        false -> Uri
    end.

add_host(Uri,ParentUri) -> 
    case Uri#uri.host == "" of
        true -> Uri#uri{host=ParentUri#uri.host};
        false -> Uri
    end.
    
add_path(Uri) -> 
    case Uri#uri.path == "" of
        true -> Uri#uri{path="/"};
        false -> Uri
    end.
    
add_scheme_and_host(Uri,ParentUri) -> add_scheme(add_host(Uri,ParentUri),ParentUri).

%% Fix bad SSL redirects
fix_ssl(Uri) when Uri#uri.port == 443 andalso Uri#uri.scheme == "http" -> Uri#uri{scheme="https",port=""};
fix_ssl(Uri) -> Uri.

%% Must be called before normalize_path
fix_scheme("://"++Rest,Parent) -> fix_scheme("//"++Rest,Parent);
fix_scheme("//"++Rest,Parent) -> uri:scheme(from_string(Parent))++"://"++Rest;
fix_scheme(Url,_Parent) -> Url.

fix_spaces(Path) -> strre:sub(Path," ","%20").

fix_query(Uri) when is_record(Uri,uri) ->
    uri:raw_query(Uri,re:replace(uri:raw_query(Uri),"&amp;","&"));    
fix_query(Url) -> 
    fix_query(from_string(Url)).    
    
remove_params(Url,[]) -> Url;
remove_params(Url,Params) ->
    Uri    = from_string(Url),
    Query  = uri:raw_query(Uri),
    Filter = fun(P) -> not lists:member(hd(string:tokens(P,"=")),Params) end,
    NewUri = uri:raw_query(Uri,string:join(lists:filter(Filter,string:tokens(Query,"&")),"&")),
    uri:raw(NewUri).

    
