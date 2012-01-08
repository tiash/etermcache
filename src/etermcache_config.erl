
-module(etermcache_config).

-compile({parse_transform,properties_pt}).

-source({application,etermcache}).
-source({file,"etermcache.config"}).

%% sizes are in bytes
-define(B(A),A).
-define(K(A),?B(A*1024)).
-define(M(A),?K(A*1024)).

%% times are in microseconds
-define(ms(A),A).
-define(sec(A),?ms(A*1000)).
-define(min(A),?sec(A*60)).

%% Maximum number of bytes in the cache
%%  nb: this is only a guidance, the cache may under heavy load grow beyond this
-property({max_bytes,268435456}). %% 256MB
%% Maximum number of entries in the cache
%%  nb: this is only a guidance, the cache may under heavy load grow beyond this
-property({max_entries,1000000}). %% 1M
%% The maximum age
%% entries older than this will be droped on the the next vaccum
%% even if the entry was recently accessed.
%% The system may use a lower threshold if the system is under heavy load.
-property({max_age,600000}).
%% The maximum time since last access
%% entries that have not been accessed in this time will be droped on the next vaccum.
%% The system may use a lower threshold if the system is under heavy load.
-property({time_to_live,120000}).

%% Time to wait between scans of the cache
-property({flush_delay,1000}).
%% Time to sleep during the table scan
-property({flush_sleep,10}).
%% Number of entries to process during a single step
-property({flush_block,100}).



