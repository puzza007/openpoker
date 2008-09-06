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

-module(betting).
-behaviour(cardgame).

-export([stop/1, test/0]).

-export([init/1, terminate/3]).
-export([handle_event/3, handle_info/3, 
	 handle_sync_event/4, code_change/4]).

-export([betting/2]).

-include("common.hrl").
-include("texas.hrl").
-include("test.hrl").
-include("proto.hrl").

-record(data, {
	  game,
	  context,
	  have_blinds,
	  max_raises,
	  stage,
	  expected, % {Player, Min, Max}
	  call,
	  raise_count,
	  timer
	 }).

init([Game, MaxRaises, Stage]) ->
    init([Game, MaxRaises, Stage, false]);

init([Game, MaxRaises, Stage, HaveBlinds]) ->
    Data = #data {
      game = Game,
      have_blinds = HaveBlinds,
      max_raises = MaxRaises,
      stage = Stage
     },
    {ok, betting, Data}.

stop(Ref) ->
    cardgame:send_all_state_event(Ref, stop).

betting({'START', Context}, Data) ->
    Game = Data#data.game,
    %% assume that we are given a record
    Button = element(2, Context),
    Call = element(3, Context),
    %%io:format("betting/start: call = ~.2. f~n", [Call * 1.0]),
    Active = gen_server:call(Game, {'SEATS', Button, ?PS_PLAY}),
    PlayerCount = length(Active),
    if
	PlayerCount < 2 ->
	    {stop, {normal, Context}, Data};
	true ->
	    _Total = gen_server:call(Game, 'POT TOTAL'),
	    gen_server:cast(Game, {'BROADCAST', 
				   {?PP_GAME_STAGE, Data#data.stage}}), 
	    if 
		Data#data.have_blinds ->
		    %% start with the player after the big blind
		    BB = element(6, Context),
		    Temp = gen_server:call(Game, {'SEATS', BB, ?PS_PLAY}),
		    Player = hd(Temp);
		true ->
		    %% start with the first player after the button
		    Player = hd(Active)
	    end,
	    Data1 = Data#data {
		      context = Context,
		      call = Call,
		      raise_count = 0
		     },
	    Data2 = ask_for_bet(Data1, Player),
	    {next_state, betting, Data2}
    end;

betting({?PP_CALL, Player, Amount}, Data) ->
    Game = Data#data.game,
    {Expected, Call, _Min, _Max} = Data#data.expected,
    if 
	Expected /= Player ->
	    {next_state, betting, Data};
	true ->
	    %% it's us
	    cancel_timer(Data),
	    InPlay = gen_server:call(Player, 'INPLAY'),
	    if 
		Amount > InPlay ->
		    betting({?PP_FOLD, Player}, Data);
		Amount > Call ->
		    betting({?PP_FOLD, Player}, Data);
		Amount == InPlay ->
		    %% all-in
		    gen_server:cast(Game, {'ADD BET', Player, Amount}),
		    next_turn(Data, Player);
		true ->
		    %% proper bet
		    gen_server:cast(Game, {'SET STATE', Player, ?PS_BET}),
		    gen_server:cast(Game, {'ADD BET', Player, Amount}),
		    gen_server:cast(Game, {'BROADCAST', {?PP_NOTIFY_CALL, 
							 Player, Call}}),
		    next_turn(Data, Player)
	    end
    end;

betting({?PP_RAISE, Player, Amount}, Data) ->
    Game = Data#data.game,
    RaiseCount = Data#data.raise_count,
    {Expected, Call, Min, Max} = Data#data.expected,
    if
	Expected /= Player ->
	    {next_state, betting, Data};
	true ->
	    %% it's us
	    cancel_timer(Data),
	    InPlay = gen_server:call(Player, 'INPLAY'),
	    if 
		(Amount > InPlay) or 
		(Amount > Max) or
		(Max == 0) or % should have sent CALL
		((Amount < Min) and ((Amount + Call) /= InPlay)) ->
		    betting({?PP_FOLD, Player}, Data);
		true ->
		    %% proper raise
		    RaiseCount1 = if 
				      Call /= 0 ->
					  RaiseCount + 1;
				      true ->
					  RaiseCount
				  end,
		    gen_server:cast(Game, {'ADD BET', Player, Amount + Call}),
		    gen_server:cast(Game, {'RESET STATE', ?PS_BET, ?PS_PLAY}),
		    if
			Amount + Call == InPlay ->
			    ok;
			true ->
			    gen_server:cast(Game, 
					    {'SET STATE', Player, ?PS_BET})
		    end,
		    gen_server:cast(Game, {'BROADCAST', {?PP_NOTIFY_RAISE, 
							 Player, Amount}}),
		    Data1 = Data#data {
			      call = Data#data.call + Amount,
			      raise_count = RaiseCount1
			     },
		    next_turn(Data1, Player)
	    end
    end;

betting({?PP_FOLD, Player}, Data) ->
    {Expected, _Call, _Min, _Max} = Data#data.expected,
    if
	Expected /= Player ->
	    {next_state, betting, Data};
	true ->
	    cancel_timer(Data),
	    gen_server:cast(Data#data.game, {'SET STATE', Player, ?PS_FOLD}),
	    gen_server:cast(Data#data.game, {'BROADCAST', 
					     {?PP_PLAYER_STATE, 
					      Player, 
					      ?PS_FOLD}}),
	    next_turn(Data, Player)
    end;

betting({timeout, _Timer, Player}, Data) ->
    cancel_timer(Data),
    Game = Data#data.game,
    GID = gen_server:call(Game, 'ID'),
    Seat = gen_server:call(Game, {'WHAT SEAT', Player}),
    error_logger:warning_report([{message, "Player timeout!"},
				 {module, ?MODULE}, 
				 {player, Player},
				 {game, GID},
				 {seat, Seat}]),
    %%
    %%io:format("~w timed out, folding~n", [Player]),
    betting({?PP_FOLD, Player}, Data);

betting(Event, Data) ->
    handle_event(Event, betting, Data).

handle_event(stop, _State, Data) ->
    {stop, normal, Data};

handle_event(Event, State, Data) ->
    error_logger:error_report([{module, ?MODULE}, 
			       {line, ?LINE},
			       {message, Event}, 
			       {self, self()},
			       {game, Data#data.game},
			       {expected, Data#data.expected}]),
    {next_state, State, Data}.
        
handle_sync_event(Event, From, State, Data) ->
    error_logger:error_report([{module, ?MODULE}, 
			       {line, ?LINE},
			       {message, Event}, 
			       {from, From},
			       {self, self()},
			       {game, Data#data.game},
			       {expected, Data#data.expected}]),
    {next_state, State, Data}.
        
handle_info(Info, State, Data) ->
    error_logger:error_report([{module, ?MODULE}, 
			       {line, ?LINE},
			       {message, Info}, 
			       {self, self()},
			       {game, Data#data.game},
			       {expected, Data#data.expected}]),
    {next_state, State, Data}.

terminate(_Reason, _State, _Data) -> 
    ok.

code_change(_OldVsn, State, Data, _Extra) ->
    {ok, State, Data}.

%%
%% Utility
%%

next_turn(Data, Player) ->
    Game = Data#data.game,
    Seat = gen_server:call(Game, {'WHAT SEAT', Player}),
    Active = gen_server:call(Game, {'SEATS', Seat, ?PS_PLAY}),
    Standing = gen_server:call(Game, {'SEATS', Seat, ?PS_STANDING}),
    ActiveCount = length(Active),
    StandingCount = length(Standing),
    if 
	StandingCount < 2 ->
	    %% last man standing wins
	    {stop, {endgame, Data#data.context}, Data};
 	ActiveCount == 0 ->
 	    %% we are done with this stage
 	    gen_server:cast(Game, {'RESET STATE', ?PS_BET, ?PS_PLAY}),
 	    Ctx = setelement(3, Data#data.context, 0), % call = 0
	    gen_server:cast(Game, 'NEW STAGE'),
	    {stop, {normal, Ctx}, Data};
 	true ->
 	    %% next player
 	    Data1 = ask_for_bet(Data, hd(Active)),
 	    {next_state, betting, Data1}
    end.

ask_for_bet(Data, Seat) ->
    Game = Data#data.game,
    Stage = Data#data.stage,
    Player = gen_server:call(Game, {'PLAYER AT', Seat}),
    Bet = gen_server:call(Game, {'BET TOTAL', Player}),
    Call = Data#data.call - Bet,
    {Min, Max} = gen_server:call(Game, {'RAISE SIZE', Player, Stage}),
    Parent = gen_server:call(Data#data.game, 'FSM'),
    gen_server:cast(Player, {?PP_BET_REQ, Parent, Call, Min, Max}),
    Data1 = restart_timer(Data, Player),
    Data1#data {
      expected = {Player, Call, Min, Max}
     }.

cancel_timer(Data) ->
    catch cardgame:cancel_timer(Data#data.timer).

restart_timer(Data, Msg) ->
    Timeout = gen_server:call(Data#data.game, 'TIMEOUT'),
    Data#data {
      timer = cardgame:start_timer(Timeout, Msg)
     }.
    
%%
%% Test suite
%% 

test() ->
    ok.
