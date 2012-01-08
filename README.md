<!-- vim: ft=markdown
-->


Erlang Term Cache
=================

Yet another caching tool for erlang.
Developed because I didn't find one that was easy to use for what I wanted.

Its built around 1 primitive operation:

    etermcache:get(Key,fun(()->Value))->Value

or much simpler
    ?cache(Key,Value).

For completness there's also a
    purge(Key) -> ok.

How It Works
------------

The cache checks if a value is known for the key and returns that, otherwise the function is called and its result is cached for future calls.

Entries are removed if:
1. They get too old;
2. They haven't been accessed in a while.
3. They are manualy purged using purge/1.

Normally each node keeps its own cache and entries are added when accessed and
removed as they expire locally.

Purge however broadcasts to ALL known nodes, and immediately/asynchronously
removes the entry (the local entry is guaranteed to be removed).




