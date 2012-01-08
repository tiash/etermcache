
-module(etermcache).

-define(APP,etermcache).

%% Term cache

%% every VM keeps a local cache
%% get(KEY,fun((KEY)->VAL))->VAL.
%%

-type key() :: term().
-type value() :: term().

%% Public exports
-export([start/0]).

-export([purge/0,purge/1,flush/0]).
-export([get/2]).

-export([status/0,status/1]).
-export([config/0,config/1]).
-export([config/2]).

-compile({no_auto_import,[get/1,put/2]}).

%% Ensure the cache application is started.
-spec start() -> ok.
start() -> application:start(?APP).

%% Removes all enterties from ALL caches
-spec purge() -> ok.
purge() -> etermcache_srv:purge().
%% Removes select enterties from ALL caches
-spec purge(key() | [key()]) -> ok.
purge(KEYS) -> etermcache_srv:purge(KEYS).


%% Lookup a value in the (local) cache,
%% if no value is cached evaluate FUN to get the correct value (which is subsequently cached).
-spec get(KEY,FUN)->VAL
       when KEY :: key()
          , VAL :: value()
          , FUN :: fun((KEY)->VAL) | fun(()->VAL).
get(Key,Fun) -> etermcache_srv:get(Key,Fun).

%% Status of the local cache
-spec status() -> [{atom(),term()}].
status() -> etermcache_srv:status().
-spec status(atom()) -> term().
status(Key) -> etermcache_srv:status(Key).


%% Queries the configuration of the local cache
-spec config() -> [{atom(),term()}].
config() -> application:get_all_env(?APP).
-spec config(atom()) -> term().
config(Key) -> application:get_env(?APP,Key).

%% Updates the configuration of the local cache
%% Simply updates the application options and forces the application to reload the config immediately
-spec config(atom(),term()) -> ok.
config(Key,Value) ->
  application:set_env(?APP,Key,Value).

%% Force the (local) cache to immediately remove stale entries,
-spec flush() -> ok.
flush() -> etermcache_srv:flush().







