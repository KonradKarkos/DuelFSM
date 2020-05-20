-module(mbp).
-behaviour(gen_fsm).

%% public API
-export([start/1, start_link/1, duel/2, accept_duel/1, attack/1, finish_oponent/1, defend/1, take_potion/2,
         make_hp_potion/1, make_mp_potion/1, cast_spell/2, draw/1, yield/2]).
%% gen_fsm callbacks
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3,
         terminate/3, code_change/4,
         % custom state names
         idle/2, idle/3, idle_wait/2, idle_wait/3, neutral_stance/2,
         neutral_stance/3, aggressive_stance/2, aggressive_stance/3, defensive_stance/2, defensive_stance/3, wait/2, wait/3, yielding/2, end_win/2, end_lose/3]).

-record(state, {name="",
                other,
                hp,
                otherhp,
				mp,
				othermp,
				ownitems=[],
				otheritems=[],
                monitor,
                from}).

%%% PUBLIC API
start(Name) ->
    gen_fsm:start(?MODULE, [Name], []).

start_link(Name) ->
    gen_fsm:start_link(?MODULE, [Name], []).

accept_duel(OwnPid) ->
    gen_fsm:sync_send_event(OwnPid, accept_duel).

attack(OwnPid) ->
	gen_fsm:send_event(OwnPid, attack).
	
cast_spell(OwnPid, Spell) ->
	gen_fsm:send_event(OwnPid, {cast_spell, Spell}).

defend(OwnPid) ->
	gen_fsm:send_event(OwnPid, defend).

duel(OwnPid, OtherPid) ->
    gen_fsm:sync_send_event(OwnPid, {neutral_stance, OtherPid}, 30000).
	
draw(OwnPid) ->
    gen_fsm:sync_send_event(OwnPid, draw, infinity).
	
finish_oponent(OwnPid)->
	gen_fsm:send_event(OwnPid, finish_oponent).

make_hp_potion(OwnPid) ->
    gen_fsm:send_event(OwnPid, make_hp_potion).
	
make_mp_potion(OwnPid) ->
    gen_fsm:send_event(OwnPid, make_mp_potion).

take_potion(OwnPid, Potion) ->
    gen_fsm:send_event(OwnPid, {take_potion, Potion}).
	
yield(OwnPid,Timeout) ->
	gen_fsm:sync_send_event(OwnPid,{yield,Timeout},infinity).
	
notify_yield(OtherPid) ->
    gen_fsm:send_all_state_event(OtherPid, yielded).

%%% CLIENT-TO-CLIENT API
%% These calls are only listed for the gen_fsm to call among themselves

ask_for_duel(OtherPid, OwnPid, Hp, Mp) ->
    gen_fsm:send_event(OtherPid, {ask_for_duel, OwnPid, Hp, Mp}).

accept_duel(OtherPid, OwnPid, Hp, Mp) ->
    gen_fsm:send_event(OtherPid, {accept_duel, OwnPid, Hp, Mp}).
	
attacked(OtherPid)->
	gen_fsm:send_event(OtherPid, attacked).
	
casted_spell(OtherPid, Spell)->
	gen_fsm:send_event(OtherPid, {casted_spell, Spell}).

try_finish(OtherPid)->
	gen_fsm:send_event(OtherPid, try_finish).

made_item(OtherPid, Item) ->
    gen_fsm:send_event(OtherPid, {made_item, Item}).

taken_potion(OtherPid, Potion) ->
    gen_fsm:send_event(OtherPid, {taken_potion, Potion}).

suggest_draw(OtherPid) ->
    gen_fsm:send_event(OtherPid, suggest_draw).
	
stats_update(OtherPid, Hp, Mp) ->
    gen_fsm:send_event(OtherPid, {stats_update, Hp, Mp}).
	
yielding_in_time(OtherPid,Timeout) ->
    gen_fsm:send_event(OtherPid, {yielding_in_time,Timeout}).

not_yet(OtherPid) ->
    gen_fsm:send_event(OtherPid, not_yet).

am_ready(OtherPid) ->
    gen_fsm:send_event(OtherPid, 'ready!').

ack_trans(OtherPid) ->
    gen_fsm:send_event(OtherPid, ack).

ask_commit(OtherPid) ->
    gen_fsm:sync_send_event(OtherPid, ask_commit).

do_commit(OtherPid) ->
    gen_fsm:sync_send_event(OtherPid, do_commit).
	
you_won(OtherPid)->
	gen_fsm:send_event(OtherPid, you_won).

%%% GEN_FSM API
init(Name) ->
    {ok, idle, #state{name=Name, hp=100, mp=0}}. 


idle({ask_for_duel, OtherPid, Otherhp, Othermp}, S=#state{}) ->
    Ref = monitor(process, OtherPid),
    notice(S, "~p asked for a duel", [OtherPid]),
    {next_state, idle_wait, S#state{other=OtherPid, monitor=Ref, otherhp=Otherhp, othermp=Othermp}};
idle(Event, Data) ->
    unexpected(Event, idle),
    {next_state, idle, Data}.

idle({neutral_stance, OtherPid}, From, S=#state{hp=Hp, mp=Mp}) ->
    ask_for_duel(OtherPid, self(), Hp, Mp),
    notice(S, "asking player ~p for a duel", [OtherPid]),
    Ref = monitor(process, OtherPid),
    {next_state, idle_wait, S#state{other=OtherPid, monitor=Ref, from=From}};
idle(Event, _From, Data) ->
    unexpected(Event, idle),
    {next_state, idle, Data}.



idle_wait({ask_for_duel, OtherPid, Otherhp, Othermp}, S=#state{other=OtherPid}) ->
    gen_fsm:reply(S#state.from, ok),
    notice(S, "starting duel", []),
    {next_state, neutral_stance, S#state{otherhp=Otherhp, othermp=Othermp}};
idle_wait({accept_duel, OtherPid, Otherhp, Othermp}, S=#state{other=OtherPid}) ->
    gen_fsm:reply(S#state.from, ok),
    notice(S, "starting duel", []),
    {next_state, neutral_stance, S#state{otherhp=Otherhp, othermp=Othermp}};
idle_wait(Event, Data) ->
    unexpected(Event, idle_wait),
    {next_state, idle_wait, Data}.

idle_wait(accept_duel, _From, S=#state{other=OtherPid, hp=Hp, mp=Mp}) ->
    accept_duel(OtherPid, self(), Hp, Mp),
    notice(S, "accepting duel", []),
    {reply, ok, neutral_stance, S};
idle_wait(Event, _From, Data) ->
    unexpected(Event, idle_wait),
    {next_state, idle_wait, Data}.



neutral_stance(make_hp_potion, S=#state{ownitems=OwnItems}) ->
    made_item(S#state.other, "small_hp_potion"),
    notice(S, "made small hp potion", []),
    {next_state, neutral_stance, S#state{ownitems=add("small_hp_potion", OwnItems)}};
neutral_stance(make_mp_potion, S=#state{ownitems=OwnItems}) ->
    made_item(S#state.other, "small_mp_potion"),
    notice(S, "made small mp potion", []),
    {next_state, neutral_stance, S#state{ownitems=add("small_mp_potion", OwnItems)}};
neutral_stance({take_potion, Potion}, S=#state{ownitems=OwnItems, hp=Hp, mp=Mp}) ->
	case Potion of
        "small_hp_potion" -> Resulthp=Hp+20, Resultmp=Mp;
        "small_mp_potion" -> Resultmp=Mp+20, Resulthp=Hp;
        "hp_potion" -> Resulthp=Hp+35, Resultmp=Mp;
		"mp_potion" -> Resultmp=Mp+35, Resulthp=Hp;
		_ -> Resultmp=Mp, Resulthp=Hp
    end,
    taken_potion(S#state.other, Potion),
    notice(S, "taking potion ~p", [Potion]),
    {next_state, neutral_stance, S#state{ownitems=remove(Potion, OwnItems), hp=Resulthp, mp=Resultmp}};
neutral_stance({cast_spell, Spell}, S=#state{other=OtherPid, hp=Hp, mp=Mp}) ->
	case Spell of
        "fireball" -> if Mp>=30 -> Resultmp = Mp-30, casted_spell(OtherPid, Spell), stats_update(OtherPid, Hp, Resultmp) end;
        "blind" -> if Mp>=50 -> Resultmp = Mp-50, casted_spell(OtherPid, Spell), stats_update(OtherPid, Hp, Resultmp) end;
		_ -> Resultmp = Mp
    end,
    notice(S, "trying to cast spell ~p", [Spell]),
    {next_state, neutral_stance, S#state{mp=Resultmp}};
neutral_stance({casted_spell, Spell}, S=#state{other=OtherPid, hp=Hp, mp=Mp}) ->
	case Spell of
        "fireball" -> Resulthp = Hp-30;
        "blind" -> Resulthp = Hp-50;
		_ -> Resulthp = Hp
    end,
	stats_update(OtherPid, Resulthp, Mp),
    notice(S, "got casted ~p spell", [Spell]),
    {next_state, neutral_stance, S#state{hp=Resulthp}};
neutral_stance({made_item, Item}, S=#state{otheritems=OtherItems}) ->
    notice(S, "other player made ~p", [Item]),
    {next_state, neutral_stance, S#state{otheritems=add(Item, OtherItems)}};
neutral_stance({taken_potion, Potion}, S=#state{otheritems=OtherItems, otherhp=Otherhp, othermp=Othermp}) ->
	case Potion of
        "small_hp_potion" -> Resulthp=Otherhp+20, Resultmp=Othermp;
        "small_mp_potion" -> Resultmp=Othermp+20, Resulthp=Otherhp;
        "hp_potion" -> Resulthp=Otherhp+35, Resultmp=Othermp;
		"mp_potion" -> Resultmp=Othermp+35, Resulthp=Otherhp;
		_ ->  Resultmp=Othermp, Resulthp=Otherhp
    end,
    notice(S, "Other player has taken ~p", [Potion]),
    {next_state, neutral_stance, S#state{otheritems=remove(Potion, OtherItems), otherhp=Resulthp, othermp=Resultmp}};
neutral_stance({stats_update, Otherhp, Othermp}, S=#state{}) ->
	notice(S, "other player has noticed about new stats ~p hp and ~p mp", [Otherhp, Othermp]),
    {next_state, neutral_stance, S#state{otherhp=Otherhp, othermp=Othermp}};
neutral_stance(suggest_draw, S=#state{other=OtherPid}) ->
    io:format("Other user is suggesting a draw.~n"),
    notice(S,
           "Compared stats:~n"
           "You have ~p hp and ~p mp, The other palyer has ~p hp and ~p mp",
           [S#state.hp, S#state.mp, S#state.otherhp, S#state.othermp]),
    not_yet(OtherPid),
    {next_state, neutral_stance, S};
neutral_stance({yielding_in_time,Timeout}, S=#state{other=OtherPid}) ->
    io:format("Other user is yielding.~n"),
    notice(S,
           "Compared stats:~n"
           "You have ~p hp and ~p mp, The other palyer has ~p hp and ~p mp "
		   "Got ~p ms to think about it",
           [S#state.hp, S#state.mp, S#state.otherhp, S#state.othermp, Timeout]),
    not_yet(OtherPid),
    {next_state, aggressive_stance, S};
neutral_stance(attack, S=#state{}) ->
    attacked(S#state.other),
    notice(S, "attacked ~p", [S#state.other]),
    {next_state, aggressive_stance, S};
neutral_stance(attacked, S=#state{hp=Hp, mp=Mp}) ->
	Resulthp = Hp-20,
    stats_update(S#state.other, Resulthp, Mp),
    notice(S, "got attacked for 20 hp, current hp is ~p", [Resulthp]),
    {next_state, neutral_stance, S#state{hp=Resulthp, mp=Mp}};
neutral_stance(defend, S=#state{}) ->
    {next_state, defensive_stance, S};
neutral_stance(try_finish, S=#state{other=OtherPid, hp=Hp}) ->
	if Hp=<0 -> yielding_in_time(OtherPid, 100000),
	notice(S, "other player tried to finish me off and succeded", []),
    {next_state, yielding, S};
	true->notice(S, "other player tried to finish me off and failed", []),
    {next_state, neutral_stance, S}
	end;
neutral_stance(Event, Data) ->
    unexpected(Event, neutral_stance),
    {next_state, neutral_stance, Data}.

neutral_stance(draw, From, S = #state{other=OtherPid}) ->
    suggest_draw(OtherPid),
    notice(S, "asking if other player wants a draw", []),
    {next_state, wait, S#state{from=From}};
neutral_stance({yield,Timeout}, From, S = #state{other=OtherPid}) ->
    yielding_in_time(OtherPid,Timeout),
    notice(S, "yielding after time", []),
	gen_fsm:send_event_after(Timeout,final_yield),
    {next_state, yielding, S#state{from=From}};
neutral_stance(Event, _From, S) ->
    unexpected(Event, neutral_stance),
    {next_state, neutral_stance, S}.
	
%%========================================================

aggressive_stance({take_potion, Potion}, S=#state{ownitems=OwnItems, hp=Hp, mp=Mp}) ->
	case Potion of
        "small_hp_potion" -> Resulthp=Hp+20, Resultmp=Mp;
        "small_mp_potion" -> Resultmp=Mp+20, Resulthp=Hp;
        "hp_potion" -> Resulthp=Hp+35, Resultmp=Mp;
		"mp_potion" -> Resultmp=Mp+35, Resulthp=Hp;
		_ -> Resultmp=Mp, Resulthp=Hp
    end,
    taken_potion(S#state.other, Potion),
    notice(S, "taking potion ~p", [Potion]),
    {next_state, aggressive_stance, S#state{ownitems=remove(Potion, OwnItems), hp=Resulthp, mp=Resultmp}};
aggressive_stance({cast_spell, Spell}, S=#state{other=OtherPid,hp=Hp, mp=Mp}) ->
	case Spell of
        "fireball" -> if Mp>=30 -> Resultmp = Mp-30, casted_spell(OtherPid, Spell), stats_update(OtherPid, Hp, Resultmp) end;
        "blind" -> if Mp>=50 -> Resultmp = Mp-50, casted_spell(OtherPid, Spell), stats_update(OtherPid, Hp, Resultmp) end;
		_ -> Resultmp = Mp
    end,
    notice(S, "trying to cast spell ~p", [Spell]),
    {next_state, neutral_stance, S#state{mp=Resultmp}};
aggressive_stance({casted_spell, Spell}, S=#state{other=OtherPid,hp=Hp, mp=Mp}) ->
	case Spell of
        "fireball" -> Resulthp = Hp-30;
        "blind" -> Resulthp = Hp-50;
		_ -> Resulthp = Hp
    end,
	stats_update(OtherPid, Resulthp, Mp),
    notice(S, "got casted ~p spell", [Spell]),
    {next_state, aggressive_stance, S#state{hp=Resulthp}};
aggressive_stance({made_item, Item}, S=#state{otheritems=OtherItems}) ->
    notice(S, "other player made ~p", [Item]),
    {next_state, aggressive_stance, S#state{otheritems=add(Item, OtherItems)}};
aggressive_stance({taken_potion, Potion}, S=#state{otheritems=OtherItems, otherhp=Otherhp, othermp=Othermp}) ->
	case Potion of
        "small_hp_potion" -> Resulthp=Otherhp+20, Resultmp=Othermp;
        "small_mp_potion" -> Resultmp=Othermp+20, Resulthp=Otherhp;
        "hp_potion" -> Resulthp=Otherhp+35, Resultmp=Othermp;
		"mp_potion" -> Resultmp=Othermp+35, Resulthp=Otherhp;
		_ ->  Resultmp=Othermp, Resulthp=Otherhp
    end,
    notice(S, "Other player has taken ~p", [Potion]),
    {next_state, aggressive_stance, S#state{otheritems=remove(Potion, OtherItems), otherhp=Resulthp, othermp=Resultmp}};
aggressive_stance({stats_update, Otherhp, Othermp}, S=#state{}) ->
	notice(S, "other player has noticed about new stats ~p hp and ~p mp", [Otherhp, Othermp]),
    {next_state, aggressive_stance, S#state{otherhp=Otherhp, othermp=Othermp}};
aggressive_stance(suggest_draw, S=#state{other=OtherPid}) ->
    io:format("Other user is suggesting a draw.~n"),
    notice(S,
           "Compared stats:~n"
           "You have ~p hp and ~p mp, The other palyer has ~p hp and ~p mp",
           [S#state.hp, S#state.mp, S#state.otherhp, S#state.othermp]),
    not_yet(OtherPid),
    {next_state, aggressive_stance, S};
aggressive_stance({yielding_in_time,Timeout}, S=#state{other=OtherPid}) ->
    io:format("Other user is yielding.~n"),
    notice(S,
           "Compared stats:~n"
           "You have ~p hp and ~p mp, The other palyer has ~p hp and ~p mp "
		   "Got ~p ms to think about it",
           [S#state.hp, S#state.mp, S#state.otherhp, S#state.othermp, Timeout]),
    not_yet(OtherPid),
    {next_state, aggressive_stance, S};
aggressive_stance(attack, S=#state{}) ->
    attacked(S#state.other),
    notice(S, "attacked ~p", [S#state.other]),
    {next_state, aggressive_stance, S};
aggressive_stance(attacked, S=#state{hp=Hp, mp=Mp}) ->
	Resulthp = Hp-30,
    stats_update(S#state.other, Resulthp, Mp),
    notice(S, "got attacked for 30 hp, current hp is ~p", [Resulthp]),
    {next_state, aggressive_stance, S#state{hp=Resulthp, mp=Mp}};
aggressive_stance(defend, S=#state{}) ->
    {next_state, defensive_stance, S};
aggressive_stance(finish_oponent, S=#state{other=OtherPid}) ->
	try_finish(OtherPid),
	notice(S, "trying to finish off other player", []),
    {next_state, aggressive_stance, S};
aggressive_stance(try_finish, S=#state{other=OtherPid, hp=Hp}) ->
	if Hp=<0 -> yielding_in_time(OtherPid, 100000),
	notice(S, "other player tried to finish me off and succeded", []),
    {next_state, yielding, S};
	true->notice(S, "other player tried to finish me off and failed", []),
    {next_state, aggressive_stance, S}
	end;
aggressive_stance(you_won, S=#state{})->
	notice(S, "I finished other player off, going to take his items", []),
	{next_state, end_win, S};
aggressive_stance(Event, Data) ->
    unexpected(Event, aggressive_stance),
    {next_state, aggressive_stance, Data}.

aggressive_stance(draw, From, S = #state{other=OtherPid}) ->
    suggest_draw(OtherPid),
    notice(S, "asking if other player wants a draw", []),
    {next_state, wait, S#state{from=From}};
aggressive_stance({yield,Timeout}, From, S = #state{other=OtherPid}) ->
    yielding_in_time(OtherPid,Timeout),
    notice(S, "yielding after time", []),
	gen_fsm:send_event_after(Timeout,final_yield),
    {next_state, yielding, S#state{from=From}};
aggressive_stance(Event, _From, S) ->
    unexpected(Event, aggressive_stance),
    {next_state, aggressive_stance, S}.

%%=====================================================================================================

defensive_stance(make_hp_potion, S=#state{ownitems=OwnItems}) ->
    made_item(S#state.other, "hp_potion"),
    notice(S, "made hp potion", []),
    {next_state, defensive_stance, S#state{ownitems=add("hp_potion", OwnItems)}};
defensive_stance(make_mp_potion, S=#state{ownitems=OwnItems}) ->
    made_item(S#state.other, "mp_potion"),
    notice(S, "made mp potion", []),
    {next_state, defensive_stance, S#state{ownitems=add("mp_potion", OwnItems)}};
defensive_stance({take_potion, Potion}, S=#state{ownitems=OwnItems, hp=Hp, mp=Mp}) ->
	case Potion of
        "small_hp_potion" -> Resulthp=Hp+20, Resultmp=Mp;
        "small_mp_potion" -> Resultmp=Mp+20, Resulthp=Hp;
        "hp_potion" -> Resulthp=Hp+35, Resultmp=Mp;
		"mp_potion" -> Resultmp=Mp+35, Resulthp=Hp;
		_ -> Resultmp=Mp, Resulthp=Hp
    end,
    taken_potion(S#state.other, Potion),
    notice(S, "taking potion ~p", [Potion]),
    {next_state, defensive_stance, S#state{ownitems=remove(Potion, OwnItems), hp=Resulthp, mp=Resultmp}};
defensive_stance({stats_update, Otherhp, Othermp}, S=#state{}) ->
	notice(S, "other player has noticed about new stats ~p hp and ~p mp", [Otherhp, Othermp]),
    {next_state, defensive_stance, S#state{otherhp=Otherhp, othermp=Othermp}};
defensive_stance({cast_spell, Spell}, S=#state{other=OtherPid, hp=Hp, mp=Mp}) ->
	case Spell of
        "fireball" -> if Mp>=30 -> Resultmp = Mp-30, casted_spell(OtherPid, Spell), stats_update(OtherPid, Hp, Resultmp) end;
        "blind" -> if Mp>=50 -> Resultmp = Mp-50, casted_spell(OtherPid, Spell), stats_update(OtherPid, Hp, Resultmp) end;
		_ -> Resultmp = Mp
    end,
    notice(S, "trying to cast spell ~p", [Spell]),
    {next_state, neutral_stance, S#state{mp=Resultmp}};
defensive_stance({casted_spell, Spell}, S=#state{other=OtherPid, hp=Hp, mp=Mp}) ->
	case Spell of
        "fireball" -> Resulthp = Hp-30;
        "blind" -> Resulthp = Hp-50;
		_ -> Resulthp = Hp
    end,
	stats_update(OtherPid, Resulthp, Mp),
    notice(S, "got casted ~p spell", [Spell]),
    {next_state, defensive_stance, S#state{hp=Resulthp}};
defensive_stance({made_item, Item}, S=#state{otheritems=OtherItems}) ->
    notice(S, "other player made ~p", [Item]),
    {next_state, defensive_stance, S#state{otheritems=add(Item, OtherItems)}};
defensive_stance({taken_potion, Potion}, S=#state{otheritems=OtherItems, otherhp=Otherhp, othermp=Othermp}) ->
	case Potion of
        "small_hp_potion" -> Resulthp=Otherhp+20, Resultmp=Othermp;
        "small_mp_potion" -> Resultmp=Othermp+20, Resulthp=Otherhp;
        "hp_potion" -> Resulthp=Otherhp+35, Resultmp=Othermp;
		"mp_potion" -> Resultmp=Othermp+35, Resulthp=Otherhp;
		_ ->  Resultmp=Othermp, Resulthp=Otherhp
    end,
    notice(S, "Other player has taken ~p", [Potion]),
    {next_state, defensive_stance, S#state{otheritems=remove(Potion, OtherItems), otherhp=Resulthp, othermp=Resultmp}};
defensive_stance(suggest_draw, S=#state{other=OtherPid}) ->
    io:format("Other user is suggesting a draw.~n"),
    notice(S,
           "Compared stats:~n"
           "You have ~p hp and ~p mp, The other palyer has ~p hp and ~p mp",
           [S#state.hp, S#state.mp, S#state.otherhp, S#state.othermp]),
    not_yet(OtherPid),
    {next_state, defensive_stance, S};
defensive_stance({yielding_in_time,Timeout}, S=#state{other=OtherPid}) ->
    io:format("Other user is yielding.~n"),
    notice(S,
           "Compared stats:~n"
           "You have ~p hp and ~p mp, The other palyer has ~p hp and ~p mp "
		   "Got ~p ms to think about it",
           [S#state.hp, S#state.mp, S#state.otherhp, S#state.othermp, Timeout]),
    not_yet(OtherPid),
    {next_state, aggressive_stance, S};
defensive_stance(attack, S=#state{}) ->
    attacked(S#state.other),
    notice(S, "attacked ~p", [S#state.other]),
    {next_state, aggressive_stance, S};
defensive_stance(attacked, S=#state{hp=Hp, mp=Mp}) ->
	Resulthp = Hp-10,
    stats_update(S#state.other, Resulthp, Mp),
    notice(S, "got attacked for 10 hp, current hp is ~p", [Resulthp]),
    {next_state, defensive_stance, S#state{hp=Resulthp, mp=Mp}};
defensive_stance(defend, S=#state{}) ->
    {next_state, defensive_stance, S};
defensive_stance(try_finish, S=#state{other=OtherPid, hp=Hp}) ->
	if Hp=<0 -> yielding_in_time(OtherPid, 100000),
	notice(S, "other player tried to finish me off and succeded", []),
    {next_state, yielding, S};
	true->notice(S, "other player tried to finish me off and failed", []),
    {next_state, defensive_stance, S}
	end;
defensive_stance(Event, Data) ->
    unexpected(Event, defensive_stance),
    {next_state, defensive_stance, Data}.

defensive_stance(draw, From, S = #state{other=OtherPid}) ->
    suggest_draw(OtherPid),
    notice(S, "asking if other player wants a draw", []),
    {next_state, wait, S#state{from=From}};
defensive_stance({yield,Timeout}, From, S = #state{other=OtherPid}) ->
    yielding_in_time(OtherPid,Timeout),
    notice(S, "yielding after ~p ms", [Timeout]),
	gen_fsm:send_event_after(Timeout,final_yield),
    {next_state, yielding, S#state{from=From}};
defensive_stance(Event, _From, S) ->
    unexpected(Event, defensive_stance),
    {next_state, defensive_stance, S}.


wait(draw, From, S = #state{other=OtherPid}) ->
    suggest_draw(OtherPid),
    notice(S, "asking if other player wants a draw", []),
    {next_state, wait, S#state{from=From}};
wait(Event, _From, S) ->
    unexpected(Event, neutral_stance),
    {next_state, wait, S}.
wait(attacked, S=#state{hp=Hp, mp=Mp}) ->
	Resulthp = Hp-15,
    stats_update(S#state.other, Resulthp, Mp),
    notice(S, "got attacked for 15 hp, current hp is ~p", [Resulthp]),
    {next_state, defensive_stance, S#state{hp=Resulthp, mp=Mp}};
wait({casted_spell, Spell}, S=#state{other=OtherPid, hp=Hp, mp=Mp}) ->
	case Spell of
        "fireball" -> Resulthp = Hp-30;
        "blind" -> Resulthp = Hp-50;
		_ -> Resulthp = Hp
    end,
	stats_update(OtherPid, Resulthp, Mp),
    notice(S, "got casted ~p spell", [Spell]),
    {next_state, defensive_stance, S#state{hp=Resulthp}};
wait({taken_potion, Potion}, S=#state{otheritems=OtherItems, otherhp=Otherhp, othermp=Othermp}) ->
	case Potion of
        "small_hp_potion" -> Resulthp=Otherhp+20, Resultmp=Othermp;
        "small_mp_potion" -> Resultmp=Othermp+20, Resulthp=Otherhp;
        "hp_potion" -> Resulthp=Otherhp+35, Resultmp=Othermp;
		"mp_potion" -> Resultmp=Othermp+35, Resulthp=Otherhp;
		_ ->  Resultmp=Othermp, Resulthp=Otherhp
    end,
    notice(S, "Other player has taken ~p", [Potion]),
    {next_state, wait, S#state{otheritems=remove(Potion, OtherItems), otherhp=Resulthp, othermp=Resultmp}};
wait({stats_update, Otherhp, Othermp}, S=#state{}) ->
	notice(S, "other player has noticed about new stats ~p hp and ~p mp", [Otherhp, Othermp]),
    {next_state, wait, S#state{otherhp=Otherhp, othermp=Othermp}};
wait(suggest_draw, S=#state{}) ->
    am_ready(S#state.other),
    notice(S, "asked if I want a draw, and I do. Waiting for confirmation", []),
    {next_state, wait, S};
wait({yield,Timeout}, S=#state{}) ->
    am_ready(S#state.other),
    notice(S, "yielding after ~p ms. Waiting for reply", [Timeout]),
    {next_state, wait, S};
wait({yielding_in_time,Timeout}, S=#state{}) ->
    am_ready(S#state.other),
    notice(S, "other player yielding for ~p ms. Waiting for reply", [Timeout]),
    {next_state, wait, S};
wait(not_yet, S = #state{}) ->
    notice(S, "Other not ready yet", []),
    {next_state, wait, S};
wait('ready!', S=#state{}) ->
    am_ready(S#state.other),
	timer:sleep(100),
    ack_trans(S#state.other),
    gen_fsm:reply(S#state.from, ok),
    notice(S, "other side is ready. Ending duel with a draw", []),
    {stop, normal, S};
wait(Event, Data) ->
    unexpected(Event, wait),
    {next_state, wait, Data}.

yielding(suggest_draw, S=#state{}) ->
    am_ready(S#state.other),
    notice(S, "asked if I want a draw, and I do. Waiting for confirmation", []),
    {next_state, yielding, S};
yielding({yielding_in_time,Timeout}, S=#state{}) ->
    am_ready(S#state.other),
    notice(S, "other player yielding for ~p ms. Waiting for reply", [Timeout]),
    {next_state, yielding, S};
yielding({stats_update, Otherhp, Othermp}, S=#state{}) ->
	notice(S, "other player has noticed about new stats ~p hp and ~p mp", [Otherhp, Othermp]),
    {next_state, yielding, S#state{otherhp=Otherhp, othermp=Othermp}};
yielding(not_yet, S = #state{}) ->
    notice(S, "Other not ready yet", []),
    {next_state, yielding, S};
yielding(try_finish, S = #state{}) ->
    notice(S, "Other player is cutting my throat", []),
	you_won(S#state.other),
	ack_trans(S#state.other),
    {next_state, end_lose, S};
yielding(final_yield, S=#state{}) ->
	notify_yield(S#state.other),
    notice(S, "Yielded and got spared", []),
    {stop, normal, S};
yielding('ready!', S=#state{}) ->
    am_ready(S#state.other),
    ack_trans(S#state.other),
    gen_fsm:reply(S#state.from, ok),
    notice(S, "other side is ready. Ending duel with a draw", []),
    {stop, normal, S};
yielding(Event, Data) ->
    unexpected(Event, yielding),
    {next_state, yielding, Data}.


end_win(ack, S=#state{}) ->
            try 
                notice(S, "asking for commit", []),
                ready_commit = ask_commit(S#state.other),
                notice(S, "ordering commit", []),
                ok = do_commit(S#state.other),
                notice(S, "committing...", []),
                commit(S),
                {stop, normal, S}
            catch Class:Reason -> 
                %% abort! Either ready_commit or do_commit failed
                notice(S, "commit failed", []),
                {stop, {Class, Reason}, S}
            end;
end_win(Event, Data) ->
    unexpected(Event, end_win),
    {next_state, end_win, Data}.

end_lose(ask_commit, _From, S) ->
    notice(S, "replying to ask_commit", []),
    {reply, ready_commit, end_lose, S};
end_lose(do_commit, _From, S) ->
    notice(S, "committing...", []),
    {stop, normal, ok, S};
end_lose(Event, _From, Data) ->
    unexpected(Event, end_lose),
    {next_state, end_lose, Data}.

handle_event(yielded, _StateName, S=#state{}) ->
    notice(S, "received yielded event", []),
    {stop, normal, S};
handle_event(Event, StateName, Data) ->
    unexpected(Event, StateName),
    {next_state, StateName, Data}.

handle_sync_event(yielded, _From, _StateName, S = #state{}) ->
    notify_yield(S#state.other),
    notice(S, "player yielded", []),
    {stop, normal, ok, S};
handle_sync_event(Event, _From, StateName, Data) ->
    unexpected(Event, StateName),
    {next_state, StateName, Data}.

%% The other player's FSM has gone down.
handle_info({'DOWN', Ref, process, Pid, Reason}, _, S=#state{other=Pid, monitor=Ref}) ->
    notice(S, "Other side dead", []),
    {stop, {other_down, Reason}, S};
handle_info(Info, StateName, Data) ->
    unexpected(Info, StateName),
    {next_state, StateName, Data}.

code_change(_OldVsn, StateName, Data, _Extra) ->
 {ok, StateName, Data}.

%% Transaction completed.
terminate(normal, ready, S=#state{}) ->
    notice(S, "FSM leaving.", []);
terminate(_Reason, _StateName, _StateData) ->
    ok.

%%% PRIVATE FUNCTIONS

%% adds an item to an item list
add(Item, Items) ->
    [Item | Items].

%% remove an item from an item list
remove(Item, Items) ->
    Items -- [Item].

%% Send players a notice.
notice(#state{name=N}, Str, Args) ->
    io:format("~s: "++Str++"~n", [N|Args]).

%% Unexpected allows to log unexpected messages
unexpected(Msg, State) ->
    io:format("~p received unknown event ~p while in state ~p~n",
              [self(), Msg, State]).


commit(S = #state{}) ->
    io:format("Transaction completed for ~s. "
              "Items left are:~n~p,~n items gained are:~n~p.~n",
              [S#state.name, S#state.ownitems, S#state.otheritems]).

