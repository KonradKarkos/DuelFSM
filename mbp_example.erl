-module(mbp_example).
-compile(export_all).

%% test a little bit of everything and also deadlocks on ready state
%% -- leftover messages possible on race conditions on ready state
main_ab() ->
    S = self(),
    PidCliA = spawn(fun() -> a(S) end),
    receive PidA -> PidA end,
    spawn(fun() -> b(PidA, PidCliA) end).

a(Parent) ->
    {ok, Pid} = mbp:start_link("Carl"),
    Parent ! Pid,
    io:format("Spawned Carl: ~p~n", [Pid]),
    %sys:trace(Pid,true),
    timer:sleep(800),
    mbp:accept_duel(Pid),
    timer:sleep(400),
    %%io:format("~p~n",[mbp:draw(Pid)]),
    timer:sleep(1000),
    mbp:attack(Pid),
    mbp:attack(Pid),
    timer:sleep(1000),
    io:format("a synchronizing~n"),
    sync2(),
    mbp:draw(Pid),
    timer:sleep(200),
    mbp:draw(Pid),
    timer:sleep(1000).

b(PidA, PidCliA) ->
    {ok, Pid} = mbp:start_link("Jim"),
    io:format("Spawned Jim: ~p~n", [Pid]),
    %sys:trace(Pid,true),
    timer:sleep(500),
    mbp:duel(Pid, PidA),
    mbp:defend(Pid),
    timer:sleep(200),
    mbp:make_hp_potion(Pid),
    timer:sleep(500),
    mbp:take_potion(Pid, "hp_potion"),
	mbp:yield(Pid, 500),
    timer:sleep(1000),
    io:format("b synchronizing~n"),
    sync1(PidCliA),
    mbp:attack(Pid), %% race condition!
    mbp:draw(Pid),
    timer:sleep(200),
    timer:sleep(1000).

main_cd() ->
    S = self(),
    PidCliC = spawn(fun() -> c(S) end),
    receive PidC -> PidC end,
    spawn(fun() -> d(PidC, PidCliC) end).

c(Parent) ->
    {ok, Pid} = mbp:start_link("Carl"),
    Parent ! Pid,
    io:format("Spawned Carl: ~p~n", [Pid]),
    %sys:trace(Pid,true),
    timer:sleep(800),
    mbp:accept_duel(Pid),
    timer:sleep(400),
    %%io:format("~p~n",[mbp:ready(Pid)]),
    %%timer:sleep(1000),
    mbp:attack(Pid),
	timer:sleep(150),
	mbp:defend(Pid),
		timer:sleep(150),
    mbp:make_mp_potion(Pid),
		timer:sleep(150),
	mbp:make_mp_potion(Pid),
		timer:sleep(150),
	mbp:take_potion(Pid,"mp_potion"),
		timer:sleep(150),
	mbp:take_potion(Pid,"mp_potion"),
		timer:sleep(150),
	mbp:attack(Pid),
		timer:sleep(150),
	mbp:cast_spell(Pid, "blind"),
		timer:sleep(150),
	mbp:attack(Pid),
    timer:sleep(500),
    io:format("a synchronizing~n"),
    sync2(),
    mbp:finish_oponent(Pid),
    timer:sleep(200),
    mbp:finish_oponent(Pid),
    timer:sleep(1000).

d(PidC, PidCliC) ->
    {ok, Pid} = mbp:start_link("Jim"),
    io:format("Spawned Jim: ~p~n", [Pid]),
    %sys:trace(Pid,true),
    timer:sleep(500),
    mbp:duel(Pid, PidC),
	mbp:make_hp_potion(Pid),
	timer:sleep(100),
    mbp:attack(Pid),
    timer:sleep(200),
    mbp:attack(Pid),
    timer:sleep(500),
    mbp:attack(Pid),
    timer:sleep(1000),
    io:format("b synchronizing~n"),
    sync1(PidCliC),
    mbp:attack(Pid),
    timer:sleep(1000).
	
%%% Utils
sync1(Pid) ->
    Pid ! self(),
    receive ack -> ok end.

sync2() ->
    receive
        From -> From ! ack
    end.
