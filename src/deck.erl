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

-module(deck).
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, 
	 handle_info/2, terminate/2, code_change/3]).

-export([start/0, start_link/0, stop/1, test/0]).

-include("test.hrl").
-include("common.hrl").

-record(data, {
	  rigged,
	  cards
	 }).

new() ->
    #data {
     rigged = [],
     cards = shuffle(make_deck())
    }.

start() ->
    gen_server:start(deck, [], []).

start_link() ->
    gen_server:start_link(deck, [], []).

init(_) ->
    process_flag(trap_exit, true),
    {ok, new()}.

stop(DeckRef) ->
    gen_server:cast(DeckRef, stop).

terminate(normal, _Data) ->
    ok.

handle_cast(stop, Data) ->
    {stop, normal, Data};

handle_cast('RESET', Data) ->
    Data1 = case Data#data.rigged of
		[] ->
		    %%io:format("Deck is not rigged~n"),
		    new();
		Cards ->
		    %%io:format("Deck is rigged with ~w~n", [Cards]),
		    Data#data {
		      cards = Cards
		     }
	    end,
    {noreply, Data1};

handle_cast({'RIG', Cards}, Data) ->
    Data1 = Data#data {
	      rigged = Cards,
	      cards = Cards
	     },
    {noreply, Data1};

handle_cast(Event, Data) ->
    error_logger:info_report([{module, ?MODULE}, 
			      {line, ?LINE},
			      {deck, self()}, 
			      {message, Event}]),
    {noreply, Data}.

handle_call('DRAW', _From, Data) ->
    if
	length(Data#data.cards) > 0 ->
	    [Card|Rest] = Data#data.cards,
	    Data1 = Data#data {
		      cards = Rest
		     },
	    {reply, Card, Data1};
	true ->
	    {reply, none, Data}
    end;

handle_call(Event, From, Data) ->
    error_logger:info_report([{module, ?MODULE}, 
			      {line, ?LINE},
			      {deck, self()}, 
			      {message, Event}, 
			      {from, From}]),
    {noreply, Data}.

handle_info({'EXIT', _Pid, _Reason}, Data) ->
    %% child exit?
    {noreply, Data};

handle_info(Info, Data) ->
    error_logger:info_report([{module, ?MODULE}, 
			      {line, ?LINE},
			      {deck, self()}, 
			      {message, Info}]),
    {noreply, Data}.

code_change(_OldVsn, Deck, _Extra) ->
    {ok, Deck}.

make_deck() ->
    Face = [ two, 
	     three, 
	     four,
	     five,
	     six,
	     seven,
	     eight,
	     nine,
	     ten,
	     jack,
	     queen,
	     king,
	     ace ],
    Suit = [ clubs, 
	     diamonds, 
	     hearts,
	     spades ],
    make_deck(Face, Suit, []).

make_deck(Face, [Suit|Rest], Acc) when atom(Face) ->
    make_deck(Face, Rest, [{ Face, Suit }|Acc]);

make_deck(_Face, [], Acc) ->
    Acc;

make_deck([Face|Rest], Suit, Acc) ->
    Acc1 = make_deck(Face, Suit, Acc),
    make_deck(Rest, Suit, Acc1);

make_deck([], _Suit, Acc) ->
    Acc.

shuffle(Cards) ->
    Temp = lists:map(fun(X) ->
			     {random:uniform(1 bsl 64), X}
		     end,
		     Cards),
    Temp1 = lists:keysort(1, Temp),
    lists:map(fun(X) ->
		      element(2, X)
	      end,
	      Temp1).

test() ->
    ok.
