%%%----------------------------------------------------------------------
%%% File    : spider_config.erl
%%% Author  : Fabrizio Castrotorres <fabrizioc1@yahoo.com>
%%% Requires : uri, mochiweb_util, strre (my custom version)
%%%----------------------------------------------------------------------

-module(spider_config).
-purpose('Spider configuration module').
-author('fabrizioc1@yahoo.com').
-include("../include/spider_config.hrl").
-compile([export_all]).

new() -> #spider_config{max_depth=100,exclude_filters=[],page_handlers=[]}.

read(Filename) -> 
    case file:consult(Filename) of
        {ok,[Config]} -> Config;
        {error,_Reason} -> new()
    end.

max_depth(Config) -> Config#spider_config.max_depth.

filename(Url) -> spider_uri:host(Url)++".config".

config(Url) ->  read(filename(Url)).

exclude_filter(Config) when is_record(Config,spider_config) -> fun(Link) -> is_excluded_link(Link,Config) end;
exclude_filter(Url) -> exclude_filter(config(Url)).

is_excluded_link(Url,Config) when is_tuple(Config) -> is_excluded_link(Url,exclude_regexp(Config));    
is_excluded_link(_,"") -> false;
is_excluded_link(Url,Regexp) ->
    case re:run(spider_uri:path(Url),Regexp) of 
        {match,_} -> true; _ -> false 
    end.

exclude_regexp(Config) -> 
    Filters = Config#spider_config.exclude_filters,
    string:join(Filters,"|").



    