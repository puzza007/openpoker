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

-module(deal_cards).
-behaviour(cardgame).

-export([stop/1, test/0]).

-export([init/1, terminate/3]).
-export([handle_event/3, handle_info/3, 
	 handle_sync_event/4, code_change/4]).

-export([deal_cards/2]).

-include("common.hrl").
-include("proto.hrl").
-include("texas.hrl").
-include("test.hrl").

-record(data, {
	  game,
	  n,
	  type
	 }).

init([Game, N, Type]) ->
    Data = #data {
      game = Game,
      n = N,
      type = Type
     },
    {ok, deal_cards, Data}.

stop(Ref) ->
    cardgame:send_all_state_event(Ref, stop).

deal_cards({'START', Context}, Data) ->
    Game = Data#data.game,
    Deck = gen_server:call(Game, 'DECK'),
    case Data#data.type of
	private ->
	    B = element(2, Context),
	    Seats = gen_server:call(Game, {'SEATS', B, ?PS_STANDING}),
	    deal_private(Game, Deck, Seats, Data#data.n);
	shared ->
	    deal_shared(Game, Deck, Data#data.n)
    end,
    {stop, {normal, Context}, Data};

deal_cards({timeout, _Timer, _Player}, Data) ->
    {next_state, deal_cards, Data};

deal_cards(Event, Data) ->
    handle_event(Event, deal_cards, Data).

handle_event(stop, _State, Data) ->
    {stop, normal, Data};

handle_event(Event, State, Data) ->
    error_logger:error_report([{module, ?MODULE}, 
			       {line, ?LINE},
			       {message, Event}, 
			       {self, self()},
			       {game, Data#data.game}]),
    {next_state, State, Data}.
        
handle_sync_event(Event, From, State, Data) ->
    error_logger:error_report([{module, ?MODULE}, 
			       {line, ?LINE},
			       {message, Event}, 
			       {from, From},
			       {self, self()},
			       {game, Data#data.game}]),
    {next_state, State, Data}.
        
handle_info(Info, State, Data) ->
    error_logger:error_report([{module, ?MODULE}, 
			       {line, ?LINE},
			       {message, Info}, 
			       {self, self()},
			       {game, Data#data.game}]),
    {next_state, State, Data}.

terminate(_Reason, _State, _Data) -> 
    ok.

code_change(_OldVsn, State, Data, _Extra) ->
    {ok, State, Data}.

%%
%% Utility
%%

deal_shared(_Game, _Deck, 0) ->
    ok;

deal_shared(Game, Deck, N) ->
    Card = gen_server:call(Deck, 'DRAW'),
    gen_server:cast(Game, {'DRAW SHARED', Card}),
    deal_shared(Game, Deck, N - 1).

deal_private(_Game, _Deck, _Seats, 0) ->
    ok;

deal_private(Game, Deck, Seats, N) ->
    F = fun(Seat) ->
		Card = gen_server:call(Deck, 'DRAW'),
		Player = gen_server:call(Game, {'PLAYER AT', Seat}),
		%%
		%%PID = gen_server:call(Player, 'ID'),
		%%io:format("Dealing ~w to ~w/~w~n", 
		%%	  [Card, PID, Seat]),
		%%
		gen_server:cast(Game, {'DRAW', Player, Card})
	end,
    lists:foreach(F, Seats),
    deal_private(Game, Deck, Seats, N - 1).

%%
%% Test suite
%% 

test() ->
    ok.
