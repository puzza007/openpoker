%%% Copyright (C) 2005 Wager Labs, SA
%%%
%%% This file is part of OpenPoker.
%%%
%%% OpenPoker is free software; you can redistribute it and/or
%%% modify it under the terms of the GNU General Public
%%% License as published by the Free Software Foundation; either
%%% version 2 of the License, or (at your option) any later version.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%%% General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public
%%% License along with this library; if not, write to the Free Software
%%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
%%%
%%% Please visit http://wagerlabs.com or contact Joel Reymont 
%%% at joelr@well.com for more information.

-module(pot).
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, 
	 handle_info/2, terminate/2, code_change/3]).

-export([start/0, start_link/0, stop/1, test/0]).

-include("test.hrl").

-record(side_pot, {
	  members,
	  all_in
	 }).

-record(pot, {
	  active = [],
	  inactive = [],
	  current = new_side_pot()
	 }).

new_side_pot(AllInAmt, Members) ->
    SidePot = #side_pot{ 
      all_in = AllInAmt, 
      members = Members 
     },
    SidePot.
    
new_side_pot(AllInAmt) when number(AllInAmt) ->
    new_side_pot(AllInAmt, gb_trees:empty());

new_side_pot(Pot) when record(Pot, side_pot) ->
    new_side_pot(Pot#side_pot.all_in, Pot#side_pot.members).

new_side_pot() ->
    new_side_pot(0, gb_trees:empty()).

new_pot() ->
    #pot {}.

start() ->
    gen_server:start(pot, [], []).

start_link() ->
    gen_server:start_link(pot, [], []).

init(_) ->
    process_flag(trap_exit, true),
    {ok, new_pot()}.

stop(PotRef) ->
    gen_server:cast(PotRef, stop).

terminate(normal, _Pot) ->
    ok.

handle_cast('RESET', _Pot) ->
    {noreply, new_pot()};

handle_cast('NEW STAGE', Pot) ->
    Inactive = Pot#pot.inactive 
	++ Pot#pot.active
	++ [Pot#pot.current],
    NewPot = Pot#pot { 
	       active = [], 
	       inactive = Inactive, 
	       current = new_side_pot()
	      },
    {noreply, NewPot};

handle_cast({'SPLIT', Player, Amount}, Pot) ->
    {noreply, split(Pot, Player, Amount)};

handle_cast(stop, Pot) ->
    {stop, normal, Pot};

handle_cast({'ADD BET', Player, Amount, IsAllIn}, Pot) ->
    {NewPot, 0} = add_bet(Pot, Player, Amount, IsAllIn),
    {noreply, NewPot};

handle_cast(Event, Pot) ->
    error_logger:info_report([{module, ?MODULE}, 
			      {line, ?LINE},
			      {pot, self()}, 
			      {message, Event}]),
    {noreply, Pot}.

handle_call('SIDE POTS', _From, Pot) ->
    Pots = [{total(P), P#side_pot.members} || P <- side_pots(Pot)],
    {reply, Pots, Pot};

handle_call('TOTAL', _From, Pot) ->
    {reply, total(Pot), Pot};

handle_call(Event, From, Pot) ->
    error_logger:info_report([{module, ?MODULE}, 
			      {line, ?LINE},
			      {pot, self()}, 
			      {message, Event}, 
			      {from, From}]),
    {noreply, Pot}.

handle_info({'EXIT', _Pid, _Reason}, Pot) ->
    %% child exit?
    {noreply, Pot};

handle_info(Info, Pot) ->
    error_logger:info_report([{module, ?MODULE}, 
			      {line, ?LINE},
			      {pot, self()}, 
			      {message, Info}]),
    {noreply, Pot}.

code_change(_OldVsn, Pot, _Extra) ->
    {ok, Pot}.

%%
%% Utility
%%

%% Ensure that player belongs to the pot

make_member(Pot, Player) ->
    case gb_trees:lookup(Player, Pot#side_pot.members) of
	{value, Bet } ->
	    {Pot, Bet};
	_ -> 
	    Members = gb_trees:insert(Player, 0, Pot#side_pot.members),
	    NewPot = Pot#side_pot { 
		       members = Members 
		      },
	    {NewPot, 0}
    end.

%% Add up to all-in amount if pot is split
%% and simply assign the amount if not

add_bet(Pot, Player, Amount) when record(Pot, side_pot) ->
    {NewPot, Bet} = make_member(Pot, Player),
    AllIn = NewPot#side_pot.all_in,
    {Unallocated, Members} = 
	if
	    AllIn > 0 ->
		%% Pot is split, figure out
		%% the difference between amount bet
		%% so far and the all-in amount
		Delta = AllIn - Bet,
		if 
		    Delta > 0 ->
			%% Post all-in
			U = Amount - Delta,
			M = gb_trees:enter(Player, AllIn, Pot#side_pot.members),
			{U, M};
		    true ->
			%% Posted enough already
			{Amount, Pot#side_pot.members}
		end;
	    true ->
		%% Pot is not split, post Amount
		M = update_counter(Player, Amount, Pot#side_pot.members),
		{0, M}
	end,
    NewPot1 = NewPot#side_pot{ 
		members = Members 
	       }, 
    {NewPot1, Unallocated};

add_bet(Pot, Player, Amount) when record(Pot, pot) ->
    add_bet(Pot, Player, Amount, false).

add_bet(Pot, Player, Amount, IsAllIn) when record(Pot, pot) ->
    %% add to prior pots as needed
    {Active, Unallocated} = allocate_bet(Pot#pot.active, Player, Amount),
    Pot1 = Pot#pot {
	     active = Active
	    },
    if
 	IsAllIn ->
	    %% split the pot
 	    Pot2 = split(Pot1, Player, Unallocated),
	    Rest = 0;
 	true ->
	    {Current, Rest} = add_bet(Pot1#pot.current, Player, Unallocated),
	    Pot2 = Pot1#pot {
		     current = Current
		    }
    end,
    {Pot2, Rest}.

allocate_bet(SidePots, Player, Amount) when list(SidePots) ->
    lists:mapfoldl(fun(Pot, Unallocated) ->
			   add_bet(Pot, Player, Unallocated)
		   end, 
		   Amount, SidePots).
    
side_pots(Pot) ->
    Temp = lists:append(Pot#pot.active, Pot#pot.inactive),
    Current = Pot#pot.current,
    lists:filter(fun(P) ->
			 gb_trees:size(P#side_pot.members) > 0
			     andalso total(P) > 0
		 end, [Current|Temp]).

total(Pot) when record(Pot, side_pot) ->
    F = fun(X, Acc) -> X + Acc end,
    lists:foldl(F, 0, gb_trees:values(Pot#side_pot.members));
		 
total(Pot) when record(Pot, pot) ->
    F = fun(X, Acc) -> 
		Acc + total(X)
	end,
    lists:foldl(F, 0, side_pots(Pot)).

%% Split the pot. Last bet for this player plus
%% the current bet becomes the all-in amount.
%% Bets in excess of the all-in amount are moved 
%% to a new side pot.

split(Pot, Player, Amount) when record(Pot, pot) ->
    {OldPot, NewPot} = split(Pot#pot.current, Player, Amount),
    Active = lists:append(Pot#pot.active, [OldPot]),
    Pot#pot { 
      current = NewPot, 
      active = Active 
     };

split(SidePot, Player, Amount) ->
    M = update_counter(Player, Amount, SidePot#side_pot.members),
    SidePot1 = SidePot#side_pot { 
		 members = M 
		},
    Members1 = SidePot1#side_pot.members,
    Bet = gb_trees:get(Player, Members1),
    List = gb_trees:to_list(Members1),
    List1 = lists:filter(fun({Key, Value}) ->
				 (Key /= Player) and (Value > Bet)
			 end, List),
    List2 = lists:map(fun({Key, Value}) ->
			      {Key, Value - Bet}
		      end, List1),
    NewPot = #side_pot {
      all_in = 0,
      members = gb_trees:from_orddict(List2)
     },
    Members2 = lists:map(fun({Key, Value}) -> 
				 if 
				     Value > Bet -> {Key, Bet};
				     true -> {Key, Value}
				 end
			 end, List),
    OldPot = SidePot1#side_pot { 
	       all_in = Bet, 
	       members = gb_trees:from_orddict(Members2)
	      },
    {OldPot, NewPot}.

update_counter(Key, Amount, Tree) ->
    case gb_trees:lookup(Key, Tree) of
	{value, Old} ->
	    Old = gb_trees:get(Key, Tree),
	    gb_trees:update(Key, Old + Amount, Tree);
	none ->
	    gb_trees:insert(Key, Amount, Tree)
    end.

%%%
%%% Test suite
%%%

is_member(Pot, Player) when record(Pot, side_pot) ->
    gb_trees:is_defined(Player, Pot#side_pot.members).

test() ->
    test1(),
    test2(),
    test3(),
    test4(),
    test5(),
    test6(),
    test7(),
    test8(),
    test9(),
    test10(),
    test11(),
    test12(),
    test13().

%% Pot is split, Delta > 0

test1() ->
    Pot = new_side_pot(100),
    {NewPot, Amount} = add_bet(Pot, 'P', 120),
    ?match(20, Amount),
    ?match(true, is_member(NewPot, 'P')),
    ?match(100, total(NewPot)).
    
%% Pot is split, Delta <= 0

test2() ->
    Pot = new_side_pot(100),
    {NewPot, Amount} = add_bet(Pot, 'P', 100),
    ?match(0, Amount),
    ?match(true, is_member(NewPot, 'P')),
    ?match(100, total(NewPot)).
    
%% Pot is not split

test3() ->
    Pot = new_side_pot(),
    {NewPot, Amount} = add_bet(Pot, 'P', 100),
    ?match(0, Amount),
    ?match(true, is_member(NewPot, 'P')),
    ?match(100, total(NewPot)),
    {NewPot1, Amount1} = add_bet(NewPot, 'P', 100),
    ?match(0, Amount1),
    ?match(200, total(NewPot1)).
    
%% Split pot

test4() ->
    Pot = new_side_pot(),
    Pot1 = Pot#side_pot { 
	     members = gb_trees:insert('A', 10, Pot#side_pot.members) 
	    },
    Pot2 = Pot1#side_pot { 
	     members = gb_trees:insert('B', 30, Pot1#side_pot.members) 
	    },
    Pot3 = Pot2#side_pot { 
	     members = gb_trees:insert('C', 40, Pot2#side_pot.members) 
	    },
    {NewPot, SidePot} = split(Pot3, 'A', 10),
    ?match(20, NewPot#side_pot.all_in),
    ?match(20, gb_trees:get('A', NewPot#side_pot.members)),
    ?match(20, gb_trees:get('B', NewPot#side_pot.members)),
    ?match(20, gb_trees:get('C', NewPot#side_pot.members)),
    ?match(0, SidePot#side_pot.all_in),
    ?match(10, gb_trees:get('B', SidePot#side_pot.members)),
    ?match(20, gb_trees:get('C', SidePot#side_pot.members)),
    ?match(false, is_member(SidePot, 'A')).
    
%% % ;;; http://www.homepokertourney.com/allin_examples.htm

test5() ->
    Pot = new_pot(),
    { Pot1, Amt1 } = add_bet(Pot, 'A', 100),
    ?match(0, Amt1),
    { Pot2, Amt2 } = add_bet(Pot1, 'B', 60, true),
    ?match(0, Amt2),
    ?match(40, total(Pot2#pot.current)),
    ?match(true, is_member(Pot2#pot.current, 'A')),
    ?match(false, is_member(Pot2#pot.current, 'B')),
    ?match(120, total(hd(Pot2#pot.active))),
    ?match(true, is_member(hd(Pot2#pot.active), 'A')),
    ?match(true, is_member(hd(Pot2#pot.active), 'B')).

test6() ->
    Pot = new_pot(),
    { Pot1, 0 } = add_bet(Pot, 'A', 100),
    { Pot2, 0 } = add_bet(Pot1, 'B', 100),
    { Pot3, 0 } = add_bet(Pot2, 'C', 60, true),
    ?match(80, total(Pot3#pot.current)),
    ?match(true, is_member(Pot3#pot.current, 'A')),
    ?match(true, is_member(Pot3#pot.current, 'B')),
    ?match(false, is_member(Pot3#pot.current, 'C')),
    ?match(180, total(hd(Pot3#pot.active))),
    ?match(true, is_member(hd(Pot3#pot.active), 'A')),
    ?match(true, is_member(hd(Pot3#pot.active), 'B')),
    ?match(true, is_member(hd(Pot3#pot.active), 'C')).
    
test7() ->
    Pot = new_pot(),
    { Pot1, 0 } = add_bet(Pot, 'A', 100),
    { Pot2, 0 } = add_bet(Pot1, 'B', 60, true),
    { Pot3, 0 } = add_bet(Pot2, 'C', 100),
    ?match(80, total(Pot3#pot.current)),
    ?match(true, is_member(Pot3#pot.current, 'A')),
    ?match(true, is_member(Pot3#pot.current, 'C')),
    ?match(false, is_member(Pot3#pot.current, 'B')),
    ?match(180, total(hd(Pot3#pot.active))),
    ?match(true, is_member(hd(Pot3#pot.active), 'A')),
    ?match(true, is_member(hd(Pot3#pot.active), 'B')),
    ?match(true, is_member(hd(Pot3#pot.active), 'C')).
    
test8() ->
    Pot = new_pot(),
    { Pot1, 0 } = add_bet(Pot, 'A', 100),
    { Pot2, 0 } = add_bet(Pot1, 'B', 60, true),
    { Pot3, 0 } = add_bet(Pot2, 'C', 100),
    { Pot4, 0 } = add_bet(Pot3, 'D', 500),
    { Pot5, 0 } = add_bet(Pot4, 'A', 250, true),
    { Pot6, 0 } = add_bet(Pot5, 'C', 400),
    %% there's a main pot between all 4 players
    Side1 = lists:nth(1, Pot6#pot.active),
    ?match(240, total(Side1)),
    ?match(true, is_member(Side1, 'A')),
    ?match(true, is_member(Side1, 'B')),
    ?match(true, is_member(Side1, 'C')),
    ?match(true, is_member(Side1, 'D')),
    %% there's a side pot between a, c and d
    Side2 = lists:nth(2, Pot6#pot.active),
    ?match(870, total(Side2)),
    ?match(true, is_member(Side2, 'A')),
    ?match(true, is_member(Side2, 'C')),
    ?match(true, is_member(Side2, 'D')),
    ?match(false, is_member(Side2, 'B')),
    %% there's another side pot between c and d
    Side3 = Pot6#pot.current,
    ?match(300, total(Side3)),
    ?match(true, is_member(Side3, 'C')),
    ?match(true, is_member(Side3, 'D')),
    ?match(false, is_member(Side3, 'A')),
    ?match(false, is_member(Side3, 'B')).

test9() ->
    Pot = new_pot(),
    { Pot1, 0 } = add_bet(Pot, 'A', 10),
    { Pot2, 0 } = add_bet(Pot1, 'B', 10),
    { Pot3, 0 } = add_bet(Pot2, 'C', 7, true),
    { Pot4, 0 } = add_bet(Pot3, 'D', 20),
    { Pot5, 0 } = add_bet(Pot4, 'A', 10),
    { Pot6, 0 } = add_bet(Pot5, 'B', 20),
    { Pot7, 0 } = add_bet(Pot6, 'D', 10),
    %% player-a folds but is still
    %% member of the last side pot
    Side = lists:last(Pot7#pot.active),
    ?match(28, total(Side)),
    ?match(true, is_member(Side, 'A')),
    ?match(true, is_member(Side, 'B')),
    ?match(true, is_member(Side, 'C')),
    ?match(true, is_member(Side, 'D')),
    Side1 = Pot7#pot.current,
    ?match(59, total(Side1)),
    ?match(true, is_member(Side1, 'A')),
    ?match(true, is_member(Side1, 'B')),
    ?match(true, is_member(Side1, 'D')),
    ?match(false, is_member(Side1, 'C')).
    
test10() ->
    Pot = new_pot(),
    { Pot1, 0 } = add_bet(Pot, 'A', 10),
    { Pot2, 0 } = add_bet(Pot1, 'B', 10),
    { Pot3, 0 } = add_bet(Pot2, 'C', 7, true),
    { Pot4, 0 } = add_bet(Pot3, 'D', 20),
    { Pot5, 0 } = add_bet(Pot4, 'A', 2, true),
    { Pot6, 0 } = add_bet(Pot5, 'B', 20),
    { Pot7, 0 } = add_bet(Pot6, 'D', 10),
    Side = lists:nth(1, Pot7#pot.active),
    ?match(28, total(Side)),
    ?match(true, is_member(Side, 'A')),
    ?match(true, is_member(Side, 'B')),
    ?match(true, is_member(Side, 'C')),
    ?match(true, is_member(Side, 'D')),
    Side1 = lists:nth(2, Pot7#pot.active),
    ?match(15, total(Side1)),
    ?match(true, is_member(Side1, 'A')),
    ?match(true, is_member(Side1, 'B')),
    ?match(true, is_member(Side1, 'D')),
    ?match(false, is_member(Side1, 'C')),
    Side2 = Pot7#pot.current,
    ?match(36, total(Side2)),
    ?match(true, is_member(Side2, 'B')),
    ?match(true, is_member(Side2, 'D')),
    ?match(false, is_member(Side2, 'A')),
    ?match(false, is_member(Side2, 'C')).

test11() ->
    Pot = new_pot(),
    { Pot1, 0 } = add_bet(Pot, 'A', 5, true),
    { Pot2, 0 } = add_bet(Pot1, 'B', 10),
    { Pot3, 0 } = add_bet(Pot2, 'C', 8, true),
    { Pot4, 0 } = add_bet(Pot3, 'D', 10),
    Side = lists:nth(1, Pot4#pot.active),
    ?match(20, total(Side)),
    ?match(true, is_member(Side, 'A')),
    ?match(true, is_member(Side, 'B')),
    ?match(true, is_member(Side, 'C')),
    ?match(true, is_member(Side, 'D')),
    Side1 = lists:nth(2, Pot4#pot.active),
    ?match(9, total(Side1)),
    ?match(true, is_member(Side1, 'B')),
    ?match(true, is_member(Side1, 'C')),
    ?match(true, is_member(Side1, 'D')),
    ?match(false, is_member(Side1, 'A')),
    Side2 = Pot4#pot.current,
    ?match(4, total(Side2)),
    ?match(true, is_member(Side2, 'B')),
    ?match(true, is_member(Side2, 'D')),
    ?match(false, is_member(Side2, 'A')),
    ?match(false, is_member(Side2, 'C')).
    
test12() ->
    Pot = new_pot(),
    { Pot1, 0 } = add_bet(Pot, 'A', 10),
    { Pot2, 0 } = add_bet(Pot1, 'B', 10),
    { Pot3, 0 } = add_bet(Pot2, 'C', 7, true),
    { Pot4, 0 } = add_bet(Pot3, 'D', 10),
    Side = lists:last(Pot4#pot.active),
    ?match(28, total(Side)),
    ?match(true, is_member(Side, 'A')),
    ?match(true, is_member(Side, 'B')),
    ?match(true, is_member(Side, 'C')),
    ?match(true, is_member(Side, 'D')),
    Side2 = Pot4#pot.current,
    ?match(9, total(Side2)),
    ?match(true, is_member(Side2, 'A')),
    ?match(true, is_member(Side2, 'B')),
    ?match(true, is_member(Side2, 'D')),
    ?match(false, is_member(Side2, 'C')).

test13() ->
    Pot = new_pot(),
    { Pot1, 0 } = add_bet(Pot, 'A', 20),
    { Pot2, 0 } = add_bet(Pot1, 'B', 10),
    ?match(30, total(Pot2)).

