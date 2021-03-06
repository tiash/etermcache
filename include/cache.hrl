
-ifndef(NOCACHE).
-define(cache_(K,Value), etermcache:get(K,fun () -> Value end)).
-define(purge_(K), etermcache:purge(K)).
-else.
-define(cache_(K,Value), Value).
-define(purge_(K), ok).
-endif.

-define(cache(K,Value), ?cache_({?MODULE,K},Value)).
-define(purge(K), ?purge({?MODULE,K})).

-define(cache(K1,K2,Value), ?cache_({?MODULE,K1,K2},Value)).
-define(purge(K1,K2), ?purge({?MODULE,K1,K2})).

