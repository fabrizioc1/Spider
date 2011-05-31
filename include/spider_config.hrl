%%
%% The configuration file must be named ${HOSTNAME}.config 
%% For example if you want to crawl http://www.example.com/stuff the file should be named www.example.com.config
%%
%% It currently has to be in the same directory where the modules
%%
%% Here is a sample configuration file which excludes images as well as JS and CSS files:
%%
%% {spider_config,5,["\\.jpg$","\\.gif$","\\.png$","\\.css$","\\.js$"],[]}.
%%
%%
-record(spider_config, {
    max_depth=0,        %% The max level of recursion. Less than or equal to zero = no limit
    exclude_filters=[], %% Regular expressions to use for excluding links to follow
    page_handlers=[]    %% Functions to call in sequence to process each page
}).