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

-module(blinds).
-behaviour(cardgame).

-export([stop/1, test/0]).

-export([init/1, terminate/3]).
-export([handle_event/3, handle_info/3, 
	 handle_sync_event/4, code_change/4]).

-export([small_blind/2, big_blind/2]).

-include("common.hrl").
-include("test.hrl").
-include("texas.hrl").
-include("proto.hrl").

-record(data, {
	  game,
	  context,
	  small_blind_seat,
	  big_blind_seat,
	  button_seat,
	  no_small_blind,
	  small_blind_amount,
	  small_blind_bet,
	  big_blind_amount,
	  timer,
	  expected, % {Player, Seat, Amount}
	  type
	 }).

init([Game]) ->
    init([Game, normal]);

init([Game, Type]) ->
    {Small, Big} = gen_server:call(Game, 'BLINDS'),
    Data = #data {
      game = Game,
      small_blind_amount = Small,
      big_blind_amount = Big,
      small_blind_bet = 0,
      no_small_blind = false,
      timer = none,
      expected = {none, 0},
      type = Type
     },
    {ok, small_blind, Data}.

stop(Ref) ->
    cardgame:send_all_state_event(Ref, stop).

%% Theory

%% Heads-up play. The small blind is the button and acts first 
%% before the flop and last after the flop. The player 
%% who does not have the button is dealt the first card.

%% There are three players remaining and one is eliminated.
%% Determine which player would have been the next big blind 
%% ... that player becomes the big blind and the other player 
%% is the small blind (and button).

%% Small blind is eliminated. The player who was the big blind 
%% now posts the small blind and the player to his left 
%% posts the big blind. The button does not move and the player 
%% who was the button, will be the button once again.

%% Big blind is eliminated. The player to the left of the eliminated 
%% big blind now posts the big blind and there is no small blind 
%% for that hand. The button moves to the player who was the small blind. 
%% On the following hand, the button does not move and the two blinds 
%% are posted normally.

small_blind({'START', Context}, Data) ->
    if 
	Data#data.type /= irc ->
	    Data1 = Data#data {
		      context = Context,
		      small_blind_seat = Context#texas.small_blind_seat,
		      big_blind_seat = Context#texas.big_blind_seat,
		      button_seat = Context#texas.button_seat
		     };
	true ->
	    Data1 = Data#data {
		      context = Context,
		      small_blind_seat = none,
		      big_blind_seat = none,
		      button_seat = none
		     }
    end,
    Game = Data1#data.game,
    %% advance button and broadcast position
    {Button1, Bust} = advance_button(Data1),
    gen_server:cast(Game, {'BROADCAST', {?PP_NOTIFY_BUTTON, Button1}}), 
    %% collect blinds
    SBPlayers = gen_server:call(Game, {'SEATS', Button1, ?PS_ACTIVE}),
    BBPlayers = gen_server:call(Game, {'SEATS', Button1, ?PS_BB_ACTIVE}),
    L1 = length(SBPlayers),
    L2 = length(BBPlayers),
    HeadsUp = ((L1 == 2) and (L2 == 2)) % two active, 0 waiting for bb
	or ((L1 == 1) and (L2 == 2)), % one active, one waiting for bb
    BB_N = length(BBPlayers),
    if
	BB_N < 2 ->
	    {stop, {normal, restart}, Data1};
	Bust and not HeadsUp ->
	    %% there's no small blind so the first player
	    %% after the button is the big blind
	    Data2 = Data1#data {
		      button_seat = Button1,
		      no_small_blind = true,
		      small_blind_seat = Data1#data.big_blind_seat
		     },
	    Amount = Data2#data.big_blind_amount,
	    %% ask for big blind
	    Data3 = ask_for_blind(Data2, hd(BBPlayers), Amount), 
	    {next_state, big_blind, Data3};
	Bust and HeadsUp ->
	    %% the first player after the button 
	    %% is the big blind and the other player
	    %% is the small blind and button
	    Data2 = Data1#data {
		      button_seat = Button1
		     },
	    Amount = Data2#data.small_blind_amount,
	    Data3 = ask_for_blind(Data2, lists:last(SBPlayers), Amount), 
	    {next_state, small_blind, Data3};
	true ->
	    Data2 = Data1#data {
		      button_seat = Button1
		     },
	    Amount = Data2#data.small_blind_amount,
	    Data3 = ask_for_blind(Data2, hd(SBPlayers), Amount),
	    {next_state, small_blind, Data3}
    end;
	
small_blind({?PP_CALL, Player, Amount}, Data) ->
    Game = Data#data.game,
    {ExpPlayer, Seat, ExpAmount} = Data#data.expected,
    if
	ExpPlayer /= Player ->
	    {next_state, small_blind, Data};
	true ->
	    %% it's us
	    cancel_timer(Data),
	    InPlay = gen_server:call(Player, 'INPLAY'),
	    if 
		(ExpAmount /= Amount) and
		(InPlay /= Amount) ->
		    timeout(Data, Player, small_blind);
		true ->
		    %% small blind posted
		    Data1 = Data#data {
			      small_blind_seat = Seat,
			      small_blind_bet = Amount
			     },
		    BBPlayers = gen_server:call(Game, 
						{'SEATS', Seat, ?PS_BB_ACTIVE}),
		    Data2 = ask_for_blind(Data1, 
					  hd(BBPlayers), 
					  Data1#data.big_blind_amount),
		    {next_state, big_blind, Data2}
	    end
    end;

small_blind({?PP_FOLD, Player}, Data) ->
    {ExpPlayer, _Seat, _ExpAmount} = Data#data.expected,
    if
	ExpPlayer /= Player ->
	    {next_state, small_blind, Data};
	true ->
	    timeout(Data, Player, small_blind)
    end;

small_blind({timeout, _Timer, Player}, Data) ->
    cancel_timer(Data),
    Game = Data#data.game,
    GID = gen_server:call(Game, 'ID'),
    Seat = gen_server:call(Game, {'WHAT SEAT', Player}),
    error_logger:warning_report(
      [{message, "Player timeout!"},
       {module, ?MODULE}, 
       {state, small_blind},
       {player, Player},
       {game, GID},
       {seat, Seat},
       {now, now()}]),
    timeout(Data, Player, small_blind);

small_blind({?PP_JOIN, Player, SeatNum, BuyIn}, Data) ->
    join(Data, Player, SeatNum, BuyIn, small_blind);

small_blind({?PP_LEAVE, Player}, Data) ->
    leave(Data, Player, small_blind);

small_blind({?PP_SIT_OUT, Player}, Data) ->
    sit_out(Data, Player, small_blind);

small_blind({?PP_COME_BACK, Player}, Data) ->
    come_back(Data, Player, small_blind);

small_blind(Event, Data) ->
    handle_event(Event, small_blind, Data).

big_blind({?PP_CALL, Player, Amount}, Data) ->
    Game = Data#data.game,
    {ExpPlayer, Seat, ExpAmount} = Data#data.expected,
    if
	ExpPlayer /= Player ->
	    {next_state, big_blind, Data};
	true ->
	    %% it's us
	    cancel_timer(Data),
	    InPlay = gen_server:call(Player, 'INPLAY'),
	    if 
		(ExpAmount /= Amount) and
		(InPlay /= Amount) ->
		    timeout(Data, Player, big_blind);
		true ->
		    %% big blind posted
		    SB = Data#data.small_blind_seat,
		    BB = Seat,
		    SBPlayer = gen_server:call(Game, {'PLAYER AT', SB}),
		    BBPlayer = Player,
		    gen_server:cast(Game, {'SET STATE', SBPlayer, ?PS_PLAY}),
		    gen_server:cast(Game, {'SET STATE', BBPlayer, ?PS_PLAY}),
		    %% record blind bets
		    Small = Data#data.small_blind_bet,
		    Big = Amount,
		    if
			Data#data.no_small_blind ->
			    ok;
			true ->
			    gen_server:cast(Game, {'ADD BET', SBPlayer, Small})
		    end,
		    gen_server:cast(Game, {'ADD BET', BBPlayer, Big}),
		    %% adjust button if a heads-up game
		    Seats = gen_server:call(Game, {'SEATS', ?PS_ACTIVE}),
		    if
			(length(Seats) == 2) and (Data#data.type /= irc) ->
			    Button = SB;
			true ->
			    Button = Data#data.button_seat
		    end,
		    Data1 = Data#data {
			      big_blind_seat = BB,
			      button_seat = Button,
			      expected = {none, none, 0}
			     },
		    %% notify players
		    gen_server:cast(Game, {'BROADCAST', {?PP_NOTIFY_SB, SB}}),
		    gen_server:cast(Game, {'BROADCAST', {?PP_NOTIFY_BB, BB}}),
		    gen_server:cast(Game, {'BROADCAST', 
		    			   {?PP_NOTIFY_BET, SBPlayer, Small}}),
		    gen_server:cast(Game, {'BROADCAST', 
		    			   {?PP_NOTIFY_BET, BBPlayer, Big}}),
		    Ctx = Data#data.context,
		    Ctx1 = Ctx#texas {
			     call = Amount,
			     small_blind_seat = SB,
			     big_blind_seat = BB,
			     button_seat = Button
			    },
		    {stop, {normal, Ctx1}, Data1}
	    end
    end;

big_blind({?PP_FOLD, Player}, Data) ->
    {ExpPlayer, _Seat, _ExpAmount} = Data#data.expected,
    if
	ExpPlayer /= Player ->
	    {next_state, big_blind, Data};
	true ->
	    timeout(Data, Player, big_blind)
    end;

big_blind({timeout, _Timer, Player}, Data) ->
    cancel_timer(Data),
    Game = Data#data.game,
    GID = gen_server:call(Game, 'ID'),
    Seat = gen_server:call(Game, {'WHAT SEAT', Player}),
    error_logger:warning_report(
      [{message, "Player timeout!"},
       {module, ?MODULE}, 
       {state, big_blind},
       {player, Player},
       {game, GID},
       {seat, Seat},
       {now, now()}]),
    timeout(Data, Player, big_blind);

big_blind({?PP_JOIN, Player, SeatNum, BuyIn}, Data) ->
    join(Data, Player, SeatNum, BuyIn, big_blind);

big_blind({?PP_LEAVE, Player}, Data) ->
    leave(Data, Player, big_blind);

big_blind({?PP_SIT_OUT, Player}, Data) ->
    sit_out(Data, Player, big_blind);

big_blind({?PP_COME_BACK, Player}, Data) ->
    come_back(Data, Player, big_blind);

big_blind(Event, Data) ->
    handle_event(Event, big_blind, Data).

handle_event(stop, _State, Data) ->
    {stop, normal, Data};

handle_event(Event, State, Data) ->
    error_logger:error_report([{module, ?MODULE}, 
			       {line, ?LINE},
			       {where, handle_event},
			       {message, Event}, 
			       {self, self()},
			       {game, Data#data.game},
			       {expected, Data#data.expected},
			       {sb, Data#data.small_blind_seat},
			       {bb, Data#data.big_blind_seat},
			       {b, Data#data.button_seat}]),
    {next_state, State, Data}.
        
handle_sync_event(Event, From, State, Data) ->
    error_logger:error_report([{module, ?MODULE}, 
			       {line, ?LINE},
			       {where, handle_sync_event},
			       {message, Event}, 
			       {from, From},
			       {self, self()},
			       {game, Data#data.game},
			       {expected, Data#data.expected},
			       {sb, Data#data.small_blind_seat},
			       {bb, Data#data.big_blind_seat},
			       {b, Data#data.button_seat}]),
    {next_state, State, Data}.
        
handle_info(Info, State, Data) ->
    error_logger:error_report([{module, ?MODULE}, 
			       {line, ?LINE},
			       {where, handle_info},
			       {message, Info}, 
			       {self, self()},
			       {game, Data#data.game},
			       {expected, Data#data.expected},
			       {sb, Data#data.small_blind_seat},
			       {bb, Data#data.big_blind_seat},
			       {b, Data#data.button_seat}]),
    {next_state, State, Data}.

terminate(_Reason, _State, _Data) -> 
    ok.

code_change(_OldVsn, State, Data, _Extra) ->
    {ok, State, Data}.

%%
%% Utility
%%

timeout(Data, Player, State) ->
    cancel_timer(Data),
    Game = Data#data.game,
    Seat = gen_server:call(Game, {'WHAT SEAT', Player}),
    case State of
	small_blind ->
	    Players = gen_server:call(Game, {'SEATS', Seat, ?PS_ACTIVE}),
	    Amount = Data#data.small_blind_amount,
	    Expected = 2;
	_ ->
	    Temp = gen_server:call(Game, {'SEATS', Seat, ?PS_BB_ACTIVE}),
	    %% remove small blind
	    Players = lists:delete(Data#data.small_blind_seat, Temp),
	    Amount = Data#data.big_blind_amount,
	    Expected = 1
    end,
    Players1 = lists:delete(Seat, Players),
    %%gen_server:cast(Game, {?PP_LEAVE, Player}), % kick player
    gen_server:cast(Game, {'SET STATE', Player, ?PS_SIT_OUT}),
    if
	length(Players1) < Expected ->
	    {stop, {normal, restart}, Data};
	true ->
	    Data1 = ask_for_blind(Data, hd(Players1), Amount),
	    {next_state, State, Data1}
    end.


join(Data, Player, SeatNum, BuyIn, State) ->
    Game = Data#data.game,
    gen_server:cast(Game, {?PP_JOIN, Player, SeatNum, BuyIn, ?PS_MAKEUP_BB}),
    {next_state, State, Data}.

leave(Data, Player, State) ->
    Game = Data#data.game,
    Seat = gen_server:call(Game, {'WHAT SEAT', Player}),
    if
	%% small blind can't leave
	%% while we are collecting
	%% the big blind
	(State == big_blind) and 
	(Seat == Data#data.small_blind_seat) ->
	    oops;
	true ->
	    gen_server:cast(Game, {?PP_LEAVE, Player})
    end,
    {next_state, State, Data}.

sit_out(Data, Player, State) ->
    gen_server:cast(Data#data.game, {'SET STATE', Player, ?PS_SIT_OUT}),
    {next_state, State, Data}.

come_back(Data, Player, State) ->
    gen_server:cast(Data#data.game, {'SET STATE', Player, ?PS_PLAY}),
    {next_state, State, Data}.

advance_button(Data) ->
    Game = Data#data.game,
    B = Data#data.button_seat,
    if
	B == none ->
	    %% first hand of the game
	    %% start with the first player
	    Players = gen_server:call(Game, {'SEATS', ?PS_ANY}),
	    Button = lists:last(Players),
	    Bust = false;
	true ->
	    %% start with the first 
	    %% player after the button
	    Players = gen_server:call(Game, {'SEATS', B, ?PS_ANY}),
	    Button = hd(Players),
	    %% big blind is bust
	    BB = Data#data.big_blind_seat,
	    BBPlayer = gen_server:call(Game, {'PLAYER AT', BB}),
	    State = gen_server:call(Game, {'STATE', BBPlayer}),
	    Bust = ?PS_FOLD == State
    end,
    {Button, Bust}.

ask_for_blind(Data, Seat, Amount) ->
    Game = Data#data.game,
    FSM = gen_server:call(Game, 'FSM'),
    Player = gen_server:call(Game, {'PLAYER AT', Seat}),
    gen_server:cast(Player, {?PP_BET_REQ, FSM, Amount, 0, 0}),
    Data1 = restart_timer(Data, Player),
    Data1#data {
      expected = {Player, Seat, Amount}
     }.

cancel_timer(Data) ->
    catch cardgame:cancel_timer(Data#data.timer).

restart_timer(Data, Msg) ->
    Timeout = gen_server:call(Data#data.game, 'TIMEOUT'),
    Data#data {
      timer = cardgame:start_timer(Timeout, Msg)
     }.

%%%
%%% Test suite
%%% 

modules() -> 
    %%[{delayed_start, [0]}, 
    %% {blinds, []}].
    [{blinds, []}].

make_game_heads_up() ->
    Players = test:make_players(2),
    Ctx = #texas {
      small_blind_seat = none,
      big_blind_seat = none,
      button_seat = none
     },
    Game = test:make_test_game(Players, Ctx, modules()),
    {Game, Players}.

make_game_3_bust() ->
    Players = test:make_players(3),
    Ctx = #texas {
      small_blind_seat = element(2, lists:nth(2, Players)),
      big_blind_seat = element(2, lists:nth(3, Players)),
      button_seat = element(2, lists:nth(1, Players))
     },
    Game = test:make_test_game(Players, Ctx, modules()),
    {Game, Players}.

make_game_5_bust() ->
    make_game_5_bust(1, 2, 3).

make_game_5_bust(Button_N, SB_N, BB_N) ->
    A = test:make_player('A'),
    B = test:make_player('B'),
    C = test:make_player('C'),
    D = test:make_player('D'),
    E = test:make_player('E'),
    Players = [{A, 2}, {B, 4}, {C, 6}, {D, 8}, {E, 9}],
    Ctx = #texas {
      small_blind_seat = element(2, lists:nth(SB_N, Players)),
      big_blind_seat = element(2, lists:nth(BB_N, Players)),
      button_seat = element(2, lists:nth(Button_N, Players))
     },
    Game = test:make_test_game(10, Players, Ctx, modules()),
    {Game, Players}.

test() ->
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
    ok.

%% Both blinds are posted

post_blinds_trigger(Game, Event, Pid) ->
    case Event of 
	{in, {'$gen_cast', {?PP_BET_REQ, Game, Amount, 0, 0}}} ->
	    %% post the blind
	    cardgame:send_event(Game, {?PP_CALL, Pid, Amount});
	_ ->
	    ok
    end,
    Game.

test3() ->
    {Game, Players} = make_game_heads_up(),
    [A, B] = Players,
    test:install_trigger(fun post_blinds_trigger/3, Game, [A, B]),
    Ctx = #texas {
      button_seat = element(2, A),
      small_blind_seat = element(2, A), 
      big_blind_seat = element(2, B),
      call = 10
     },
    ?match(success, ?waitmsg({'CARDGAME EXIT', Game, Ctx}, 1000)),
    cardgame:stop(Game),
    test:kill_players(Players).

%%% http://www.homepokertourney.com/button.htm

%%% 3 players, button is bust

test4() ->
    {Game, Players} = make_game_3_bust(),
    [A, B, C] = Players,
    cardgame:cast(Game, {'SET STATE', element(1, A), ?PS_FOLD}),
    test:install_trigger(fun post_blinds_trigger/3, Game, [A, B, C]),
    Ctx = #texas {
      button_seat = element(2, C),
      small_blind_seat = element(2, C),
      big_blind_seat = element(2, B),
      call = 10
     },
    ?match(success, ?waitmsg({'CARDGAME EXIT', Game, Ctx}, 1000)),
    cardgame:stop(Game),
    test:kill_players(Players).

%%% 3 players, small blind is bust

test5() ->
    {Game, Players} = make_game_3_bust(),
    [A, B, C] = Players,
    cardgame:cast(Game, {'SET STATE', element(1, B), ?PS_FOLD}),
    test:install_trigger(fun post_blinds_trigger/3, Game, [A, B, C]),
    Ctx = #texas {
      button_seat = element(2, C),
      small_blind_seat = element(2, C),
      big_blind_seat = element(2, A),
      call = 10
     },
    ?match(success, ?waitmsg({'CARDGAME EXIT', Game, Ctx}, 1000)),
    cardgame:stop(Game),
    test:kill_players(Players).

%%% 3 players, big blind is bust

test6() ->
    {Game, Players} = make_game_3_bust(),
    [A, B, C] = Players,
    cardgame:cast(Game, {'SET STATE', element(1, C), ?PS_FOLD}),
    test:install_trigger(fun post_blinds_trigger/3, Game, [A, B, C]),
    Ctx = #texas {
      button_seat = element(2, B),
      small_blind_seat = element(2, B),
      big_blind_seat = element(2, A),
      call = 10
     },
    ?match(success, ?waitmsg({'CARDGAME EXIT', Game, Ctx}, 1000)),
    cardgame:stop(Game),
    test:kill_players(Players).

%%% 5 players, small blind is bust

test7() ->
    {Game, Players} = make_game_5_bust(),
    [_, B, C, D, E] = Players,
    cardgame:cast(Game, {'SET STATE', element(1, B), ?PS_FOLD}),
    test:install_trigger(fun post_blinds_trigger/3, Game, [B, C, D, E]),
    Ctx = #texas {
      button_seat = element(2, B),
      small_blind_seat = element(2, C),
      big_blind_seat = element(2, D),
      call = 10
     },
    ?match(success, ?waitmsg({'CARDGAME EXIT', Game, Ctx}, 1000)),
    cardgame:stop(Game),
    test:kill_players(Players).

test8() ->
    {Game, Players} = make_game_5_bust(2, 3, 4),
    [_, B, C, D, E] = Players,
    cardgame:cast(Game, {'SET STATE', element(1, B), ?PS_FOLD}),
    test:install_trigger(fun post_blinds_trigger/3, Game, [B, C, D, E]),
    Ctx = #texas {
      button_seat = element(2, C),
      small_blind_seat = element(2, D),
      big_blind_seat = element(2, E),
      call = 10
     },
    ?match(success, ?waitmsg({'CARDGAME EXIT', Game, Ctx}, 1000)),
    cardgame:stop(Game),
    test:kill_players(Players).

%%% 5 players, big blind is bust

test9() ->
    {Game, Players} = make_game_5_bust(),
    [_, B, C, D, E] = Players,
    cardgame:cast(Game, {'SET STATE', element(1, C), ?PS_FOLD}),
    test:install_trigger(fun post_blinds_trigger/3, Game, [B, C, D, E]),
    Ctx = #texas {
      button_seat = element(2, B),
      small_blind_seat = element(2, C),
      big_blind_seat = element(2, D),
      call = 10
     },
    ?match(success, ?waitmsg({'CARDGAME EXIT', Game, Ctx}, 1000)),
    cardgame:stop(Game),
    test:kill_players(Players).

test10() ->
    {Game, Players} = make_game_5_bust(2, 3, 4),
    [_, B, C, D, E] = Players,
    cardgame:cast(Game, {'SET STATE', element(1, C), ?PS_FOLD}),
    test:install_trigger(fun post_blinds_trigger/3, Game, [B, C, D, E]),
    Ctx = #texas {
      button_seat = element(2, C),
      small_blind_seat = element(2, D),
      big_blind_seat = element(2, E),
      call = 10
     },
    ?match(success, ?waitmsg({'CARDGAME EXIT', Game, Ctx}, 1000)),
    cardgame:stop(Game),
    test:kill_players(Players).

%%% 5 players, both blinds are bust

test11() ->
    {Game, Players} = make_game_5_bust(),
    [_, B, C, D, E] = Players,
    cardgame:cast(Game, {'SET STATE', element(1, B), ?PS_FOLD}),
    cardgame:cast(Game, {'SET STATE', element(1, C), ?PS_FOLD}),
    test:install_trigger(fun post_blinds_trigger/3, Game, [B, C, D, E]),
    Ctx = #texas {
      button_seat = element(2, B),
      small_blind_seat = element(2, C),
      big_blind_seat = element(2, D),
      call = 10
     },
    ?match(success, ?waitmsg({'CARDGAME EXIT', Game, Ctx}, 1000)),
    cardgame:stop(Game),
    test:kill_players(Players).

test12() ->
    {Game, Players} = make_game_5_bust(2, 3, 4),
    [_, B, C, D, E] = Players,
    cardgame:cast(Game, {'SET STATE', element(1, B), ?PS_FOLD}),
    cardgame:cast(Game, {'SET STATE', element(1, C), ?PS_FOLD}),
    test:install_trigger(fun post_blinds_trigger/3, Game, [B, C, D, E]),
    Ctx = #texas {
      button_seat = element(2, C),
      small_blind_seat = element(2, D),
      big_blind_seat = element(2, E),
      call = 10
     },
    ?match(success, ?waitmsg({'CARDGAME EXIT', Game, Ctx}, 1000)),
    cardgame:stop(Game),
    test:kill_players(Players).

