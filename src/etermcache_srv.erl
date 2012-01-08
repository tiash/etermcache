-module(etermcache_srv).

-define(APP,etermcache).

-compile({parse_transform,value_pt}).

-field(max_age).
-field(time_to_live).
-field(sleep).
-field({needs_flush,[{default,false}]}).

-member({loop/0,[accessor]}).
-member({adjust/0,[mutator]}).
-member({check/0,[mutator]}).
-member({run_flush/1,[mutator]}).
-member({run_flush/2,[mutator]}).
-member({run_flush/5,[mutator]}).
-member({receiver/0,[mutator]}).
-member({receiver/1,[mutator]}).
-member({receiver_/1,[mutator]}).
-member({start_flush/0,[mutator]}).

-export([start_link/0]).
-export([flush/0]).
-export([get/2]).
-export([purge/0,purge/1]).
-export([status/0,status/1]).

-export([loop/1]).



-define(DATA,etermcache_data).
-define(LOG,etermcache_log).

init() -> init(etermcache_config:max_age(),etermcache_config:time_to_live(),etermcache_config:flush_sleep()).

ensure_tables() ->
  case ets:info(?DATA,named_table) of
    true -> ok;
    _ ->
      ?DATA = ets:new(?DATA,[set,public,{keypos,1},named_table,
        {read_concurrency,true},{write_concurrency,true}]),
      ok
  end,
  case ets:info(?LOG,named_table) of
    true -> ok;
    _ ->
      ?LOG = ets:new(?LOG,[set,public,{keypos,1},named_table,
          {write_concurrency,true}]),
      ok
  end.

start_link() ->
  {ok,proc_lib:spawn_link(fun () ->
      register(?MODULE,self()),
      ensure_tables(),
      ?MODULE:loop(init())
  end)}.

quota() ->
  case status_bytes_entries() of
    undefined -> undefined;
    {Bytes,Entries} -> quota(Bytes,Entries)
  end.
quota(undefined,_Entries) -> undefined;
quota(_Bytes,undefiend) -> undefined;
quota(Bytes,Entries) ->
  max(Entries*1.0/etermcache_config:max_entries()
     ,Bytes*1.0/etermcache_config:max_bytes()).

start_flush() ->
  sleep(0), needs_flush(true).

adjust() ->
  case quota() of
    1 -> ok;
    Q ->
      if Q>1 -> start_flush();
         true -> ok
      end,
      Q1 = max(0.9,Q),
      Q2 = max(0.8,Q*Q),
      max_age(min(etermcache_config:max_age(),round(max_age()/Q1))),
      time_to_live(min(etermcache_config:time_to_live(),round(time_to_live()/Q2)))
  end.

receiver() -> receiver(sleep()).
receiver(Sleep) -> 
  adjust(), receiver_(Sleep).
receiver_(Sleep) ->
  receive
    {time_to_live,Ref,Pid} -> Pid!{time_to_live,Ref,time_to_live()}, receiver_(0);
    {max_age,Ref,Pid} -> Pid!{max_age,Ref,max_age()}, receiver_(0);
    {purge,Key} -> lpurge(Key), receiver_(0);
    flush -> start_flush();
    _ -> receiver_(0)
  after Sleep -> ok
  end.

loop() ->
    case needs_flush() of
      true ->
        receiver(etermcache_config:flush_delay());
      _ ->
        sleep(etermcache_config:flush_sleep()),
        receiver(etermcache_config:flush_delay())
    end,
    needs_flush(false),
    run_flush(?LOG),
    receiver(sleep()),
    adjust(),
    run_flush(?DATA),
    ?MODULE:loop(THIS).

run_flush(Table) ->
  run_flush(Table,ets:first(Table)).

run_flush(?DATA,Key) ->
  run_flush(etermcache_config:flush_block(),max_age(),tsNow(),?DATA,Key);
run_flush(?LOG,Key) ->
  run_flush(etermcache_config:flush_block(),time_to_live(),tsNow(),?LOG,Key).

run_flush(_Count,_MaxAge,_Now,_Table,'$end_of_table') -> ok;
run_flush(0,_MaxAge,_Now,Table,Key) ->
  receiver(),
  adjust(),
  run_flush(Table,Key);
run_flush(Count,MaxAge,Now,Table,Key) ->
  case try {ok,ets:next(Table,Key)} catch error:badarg -> error end of
    {ok,Next} ->
      try
        TS = tsPlus(ets:lookup_element(Table,Key,2),MaxAge),
        if TS<Now -> lpurge(Key);
           true -> ok
        end
      catch 
        error:badarg -> ok
      end,
      run_flush(Count,MaxAge,Now,Table,Next);
    _ -> needs_flush(true) %% Something went wrong with the table scan so ensure we do another scan ASAP
  end.

flush() ->
  ?MODULE!flush.

tsPlus({A,B,C},{X,Y,Z}) -> tsNorm({A+X,B+Y,C+Z});
tsPlus(A,B) -> tsPlus(tsNorm(A),tsNorm(B)).

tsNorm({A0,B0,C0}) ->
  C1 = C0 rem 1000000,
  B1 = B0 + (C0 div 1000000),
  B2 = B1 rem 1000000,
  A1 = A0 + (B1 div 1000000),
  {A1,B2,C1};
tsNorm(A) when is_integer(A) -> tsNorm({0,A div 1000,(A rem 1000)*1000}).

tsNow() -> tsNorm(now()).
% tsNow(A) -> tsPlus(now(),A).
  
get(Key,Fun) ->
  % ensure(),
  ets:insert(?LOG,{Key,tsNow()}),
  try ets:lookup_element(?DATA,Key,3)
  catch error:badarg ->
          Value = if is_function(Fun,0) -> Fun();
                     is_function(Fun,1) -> Fun(Key)
                  end,
          ets:insert(?DATA,{Key,tsNow(),Value}),
          Value
  end.

purge() -> 
  lists:foreach(fun (N) -> {?MODULE,N} ! purge end, nodes()),
  lpurge().
  
purge(K) ->
  lists:foreach(fun (N) -> {?MODULE,N} !  {purge,K} end, nodes()),
  lpurge(K).

lpurge() ->
  ets:delete_all_objects(?DATA),
  ets:delete_all_objects(?LOG).
lpurge(K) ->
  ets:delete(?DATA,K),
  ets:delete(?LOG,K).

einfo(Info) ->
  case ets:info(?DATA,Info) of
    undefined -> undefined;
    A ->
      case ets:info(?LOG,Info) of
        undefined -> undefiend;
        B -> {A,B}
      end
  end.

status_bytes() ->
  case einfo(memory) of
    undefined -> undefined;
    {A,B} -> A+B
  end.
status_entries() ->
  case einfo(size) of
    undefined -> undefined;
    {A,B} -> max(A,B)
  end.
status_bytes_entries() ->
  case status_bytes() of
    undefined -> undefined;
    A ->
      case status_entries() of
        undefined -> undefined;
        B -> {A,B}
      end
  end.

status() ->
  Ref = make_ref(),
  ?MODULE!{max_age,Ref,self()},
  ?MODULE!{time_to_live,Ref,self()},
  case status_bytes_entries() of
    {Bytes,Entries} ->
      [ {bytes,Bytes}
      , {entries,Entries}
      , {quota,quota(Bytes,Entries)}
      ];
    undefined -> []
  end
  ++
  receive
    {max_age,Ref,MaxAge} -> [{max_age,MaxAge}]
  after 5000 -> [] end
  ++
  receive
    {time_to_live,Ref,TimeToLive} -> [{time_to_live,TimeToLive}]
  after 5000 -> [] end.

status(bytes) -> status_bytes();
status(entries) -> status_entries();
status(time_to_live) ->
  Ref = make_ref(),
  ?MODULE ! {time_to_live,Ref,self()},
  receive
    {time_to_live,Ref,TimeToLive} -> TimeToLive
  after 5000 -> undefined
  end;
status(max_age) ->
  Ref = make_ref(),
  ?MODULE ! {max_age,Ref,self()},
  receive
    {max_age,Ref,MaxAge} -> MaxAge
  after 5000 -> undefined
  end;
status(quota) -> quota().

    
  


  
  





