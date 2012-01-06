-module(etermcache_srv).

-define(APP,etermcache).

-export([start_link/1]).
-export([flush/0]).
-export([get/2]).
-export([purge/0,purge/1]).
-export([status/0,status/1]).

-export([loop/1]).

-define(B(A),A).
-define(K(A),?B(A*1024)).
-define(M(A),?K(A*1024)).
-define(ms(A),A*1000).
-define(sec(A),?ms(A*1000)).
-define(min(A),?sec(A*60)).

-define(DATA,etermcache_data).
-define(LOG,etermcache_log).

-record(config, { max_bytes=?M(64)
                , max_entries=?K(512)
                , hard_max_age=?sec(30)
                , max_age
                , hard_time_to_live=?sec(10)
                , time_to_live
                }).
init() ->
  ?DATA = ets:new(?DATA,[set,public,{keypos,1},named_table,
      {read_concurrency,true},{write_concurrency,true}]),
  ?LOG = ets:new(?LOG,[set,public,{keypos,1},named_table,
      {write_concurrency,true}]),
  ok.

ensure_tables() ->
  case ets:info(?DATA,named_table) of
    true -> 
      case ets:info(?LOG,named_table) of
        true -> ok;
        _ -> init()
      end;
    _ -> init()
  end.

% ensure() ->
  % application:start(?APP),
  % case check() of
    % false -> flush();
    % _ -> ok
  % end,
  % ok.

start_link(_Options) ->
  {ok,erlang:spawn_link(fun () ->
      register(?MODULE,self()),
      ensure_tables(),
      ?MODULE:loop(config())
  end)}.

quota(Config) ->
  case ets:info(?DATA,size) of
    undefined -> 1;
    Size ->
      Bytes = ets:info(?DATA,memory) + ets:info(?LOG,memory),
      max(Size*1.0/Config#config.max_entries
         ,Bytes*1.0/Config#config.max_bytes)
  end.

check() -> check(config()).
check(Config) ->
  Q = quota(Config),
  Q=<1.0.

config() -> 
  Config0=#config{hard_max_age=MaxAge,hard_time_to_live=TimeToLive} = update(#config{}),
  Config0#config{max_age=MaxAge,time_to_live=TimeToLive}.
update(Config) -> update(application:get_all_env(?APP),Config).
update([],Config) -> Config;
update([O|Opts],Config) -> update(Opts,update(O,Config));
update({max_age,A},Config) -> Config#config{hard_max_age=A};
update({time_to_live,A},Config) -> Config#config{hard_time_to_live=A};
update({max_bytes,A},Config) -> Config#config{max_bytes=A};
update({max_entries,A},Config) -> Config#config{max_entries=A};
update(_,Config) -> Config.

adjust(Config=#config{ hard_max_age=HardMaxAge
                     , max_age=MaxAge
                     , hard_time_to_live=HardTimeToLive
                     , time_to_live=TimeToLive
                     }) ->
  case quota(Config) of
    1 -> Config;
    Q -> 
      Q1 = max(0.9,Q),
      Q2 = max(0.8,Q*Q),
      Config#config { max_age=min(HardMaxAge,round(MaxAge/Q1))
                    , time_to_live=min(HardTimeToLive,round(TimeToLive/Q2))
                    }
  end.

loop(Config0) ->
    Config1 = update(Config0),
    Rate0 = case check(Config1) of true -> {100,1000}; _ -> immediate end,
    Config2 = adjust(Config1),
    Rate1 = updateRate(Config2#config.time_to_live,Rate0,?LOG,ets:first(?LOG)),
    _Rate2 = updateRate(Config2#config.max_age,Rate1,?DATA,ets:first(?DATA)),
    receive 
      {time_to_live,Ref,Pid} -> Pid!{time_to_live,Ref,Config2#config.time_to_live};
      {max_age,Ref,Pid} -> Pid!{max_age,Ref,Config2#config.max_age}
    after 0 -> ok
    end,
    ?MODULE:loop(Config2).

flush() ->
  ?MODULE!flush.

flush(_TimeOut,Rate,_Count,_Now,_Table,'$end_of_table') -> Rate;
flush(TimeOut,Rate,0,_Now,Table,Key) -> updateRate(TimeOut,Rate,Table,Key);
flush(TimeOut,Rate,Count,Now,Table,Key) ->
  Next = ets:next(Table,Key),
  try
    TS = tsPlus(ets:lookup_element(Table,Key,2),TimeOut),
    if TS<Now -> lpurge(Key);
       true -> ok
    end
  catch 
    error:badarg -> ok
  end,
  flush(TimeOut,Rate,Count-1,Now,Table,Next).


updateRate(TimeOut,ORate,Table,Key) ->
  updateRate(TimeOut,ORate,sleep(ORate),Table,Key).
updateRate(TimeOut,ORate,Sleep,Table,Key) ->
  receive
    purge -> lpurge(all);
    {purge,Key} -> lpurge(Key);
    {flush,NRate} ->
      updateRate(TimeOut,fastest(ORate,NRate),0,Table,Key);
    flush ->
      updateRate(TimeOut,immediate,0,Table,Key)
  after Sleep ->
    flush(TimeOut,ORate,step(ORate),tsNow(),Table,Key)
  end.

sleep(immediate) -> 0;
sleep({_,Sleep}) -> Sleep.
step(immediate) -> 100;
step({Step,_}) -> Step.
fastest(immediate,_) -> immediate;
fastest(_,immediate) -> immediate;
fastest({A,B},{C,D}) -> 
  if A*D > C*B -> {A,B};
     true -> {C,D}
  end.

tsPlus({A,B,C},{X,Y,Z}) -> tsNorm({A+X,B+Y,C+Z});
tsPlus(A,B) -> tsPlus(tsNorm(A),tsNorm(B)).

tsNorm({A0,B0,C0}) ->
  C1 = C0 rem 1000000,
  B1 = B0 + (C0 div 1000000),
  B2 = B1 rem 1000000,
  A1 = A0 + (B1 div 1000000),
  {A1,B2,C1};
tsNorm(A) when is_integer(A) -> tsNorm({0,0,A}).

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

purge() -> purge(all).
purge(K) -> purge(K,[node()|nodes()]).
purge(_K,[]) -> ok;
purge(K,[N|Ns]) ->
  spawn(N,?MODULE,lpurge,K),
  purge(K,Ns).

lpurge(all) ->
  ets:delete_all_objects(?DATA),
  ets:delete_all_objects(?LOG);
lpurge([]) -> ok;
lpurge([K|Ks]) -> lpurge(K), lpurge(Ks);
lpurge(K) ->
  ets:delete(?DATA,K),
  ets:delete(?LOG,K).


status() ->
  Ref = make_ref(),
  ?MODULE!{max_age,Ref,self()},
  ?MODULE!{time_to_live,Ref,self()},
  receive
    {max_age,Ref,MaxAge} -> ok
  after 5000 -> MaxAge = undefined end,
  receive
    {time_to_live,Ref,TimeToLive} -> ok
  after 5000 -> TimeToLive = undefined end,
  [ {bytes,status(bytes)}
  , {entries,status(entries)}
  , {quota,status(quota)}]
  ++  case TimeToLive of
        undefined -> [];
        _ -> [{time_to_live,TimeToLive}]
      end
  ++  case MaxAge of
        undefined -> [];
        _ -> [{max_age,MaxAge}]
      end.

status(bytes) ->
  case {ets:info(?DATA,memory),ets:info(?LOG,memory)} of
    {undefined,undefined} -> 0;
    {A,undefined} -> A;
    {undefined,A} -> A;
    {A,B} -> A+B
  end;
status(entries) ->
  case {ets:info(?DATA,size),ets:info(?LOG,size)} of
    {undefined,undefined} -> 0;
    {A,undefined} -> A;
    {undefined,A} -> A;
    {A,B} -> max(A,B)
  end;
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
status(quota) -> quota(config()).

    
  


  
  





