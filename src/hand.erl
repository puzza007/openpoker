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

-module(hand).
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, 
	 handle_info/2, terminate/2, code_change/3]).

-export([start/0, start_link/0, stop/1, test/0, describe/1]).

-export([make_card/1, face/1, suit/1, print_bin/1, print_rep/1]).

-include("test.hrl").

-record(hand, {
	  id, 
	  cards, 
	  rank,
	  score,
	  high
	 }).

new() ->
    new(0, []).

new(Id, Cards) ->
    #hand { 
     id = Id, 
     cards = Cards 
    }.

start() ->
    gen_server:start(hand, [], []).

start_link() ->
    gen_server:start_link(hand, [], []).

init(_) ->
    process_flag(trap_exit, true),
    {ok, new()}.

stop(HandRef) ->
    gen_server:cast(HandRef, stop).

terminate(normal, _Hand) ->
    ok.

handle_cast({'ADD CARD', Card}, Hand) ->
    NewHand = add(Hand, Card), 
    {noreply, NewHand};

handle_cast({'RESET', Id}, Hand) ->
    NewHand = Hand#hand {
		id = Id,
		cards = [],
		rank = none,
		score = 0,
		high = none
	       },
    {noreply, NewHand};

handle_cast(stop, Hand) ->
    {stop, normal, Hand};

handle_cast(Event, Hand) ->
    error_logger:info_report([{module, ?MODULE}, 
			      {line, ?LINE},
			      {hand, self()}, 
			      {message, Event}]),
    {noreply, Hand}.

handle_call('CARDS', _From, Hand) ->
    {reply, {Hand#hand.id, Hand#hand.cards}, Hand};

handle_call('RANK', _From, Hand) ->
    NewHand = rank(Hand),
    Id = NewHand#hand.id,
    Value = rank_value(NewHand#hand.rank),
    High = NewHand#hand.high,
    Score = NewHand#hand.score,
    {reply, {Id, Value, High, Score}, NewHand};

handle_call(Event, From, Hand) ->
    error_logger:info_report([{module, ?MODULE}, 
			      {line, ?LINE},
			      {hand, self()}, 
			      {message, Event}, 
			      {from, From}]),
    {noreply, Hand}.

handle_info({'EXIT', _Pid, _Reason}, Hand) ->
    %% child exit?
    {noreply, Hand};

handle_info(Info, Hand) ->
    error_logger:info_report([{module, ?MODULE}, 
			      {line, ?LINE},
			      {hand, self()}, 
			      {message, Info}]),
    {noreply, Hand}.

code_change(_OldVsn, Hand, _Extra) ->
    {ok, Hand}.

%%
%% Utility
%%

describe({8, High, _Score}) ->
    "straight flush high " 
	++ atom_to_list(face(High))
	++ "s";

describe({7, High, _Score}) ->
    "four of a kind " 
	++ atom_to_list(face(High))
	++ "s";
	
describe({6, High, _Score}) ->
    Bin = <<High:32>>,
    <<High3:16, High2:16>> = Bin,
    "house of " 
	++ atom_to_list(face(High3)) 
	++ "s full of " 
	++ atom_to_list(face(High2))
	++ "s";

describe({5, High, _Score}) ->
    "flush high "
	++ atom_to_list(face(High))
	++ "s";
	
describe({4, High, _Score}) ->
    "straight high "
	++ atom_to_list(face(High))
	++ "s";
	
describe({3, High, _Score}) ->
    "three of a kind "
	++ atom_to_list(face(High))
	++ "s";
	
describe({2, High, _Score}) ->
    High1 = face(High),
    HighVal2 = High band (bnot face(High1)),
    High2 = face(HighVal2),
    "two pairs of "
	++ atom_to_list(High1)
	++ "s and "
	++ atom_to_list(High2)
	++ "s";
	
describe({1, High, _Score}) ->
    "pair of "
	++ atom_to_list(face(High))
	++ "s";
	
describe({0, High, _Score}) ->
    "high card "
	++ atom_to_list(face(High)).

add(Hand, Card) ->
    Hand#hand{ 
      cards = [Card|Hand#hand.cards]
     }.

rank(Hand) ->
    Rep = make_rep(Hand),
    {Rank, High, Score} = score(Rep),
    Hand#hand {
      rank = Rank, 
      high = High, 
      score = Score
     }.

score(Rep) ->
    score([fun is_straight_flush/1,
	   fun is_four_kind/1,
	   fun is_full_house/1,
	   fun is_flush/1,
	   fun is_straight/1,
	   fun is_three_kind/1,
	   fun is_two_pair/1,
	   fun is_pair/1
	  ], Rep).

score([H|T], Rep) ->
    case Score = H(Rep) of
	junk ->
	    score(T, Rep);
	_ ->
	    Score
    end;

score([], Rep) ->
    Mask = make_mask(Rep),
    High = bits:clear_extra_bits(Mask, 5),
    {junk, High, 0}.

make_rep(Hand) when record(Hand, hand) ->
    make_rep(Hand#hand.cards);

make_rep(Cards) when list(Cards) ->
    make_rep(Cards, {0, 0, 0, 0}).

make_rep([{Face, Suit}|T], Rep) -> 
    Suit1 = suit(Suit),
    Old = element(Suit1, Rep),
    Face1 = face(Face),
    make_rep(T, setelement(Suit1, Rep, Old bor Face1));

make_rep([], Rep) ->
    tuple_to_list(Rep).

make_mask([C, D, H, S]) ->
    C bor D bor H bor S.

high_bit(Mask) ->
    1 bsl bits:log2(Mask).

clear_high_bit([C, D, H, S], High) ->
    [C band (bnot High),
     D band (bnot High),
     H band (bnot High),
     S band (bnot High)].

score(Rep, High, Bits) ->
    Mask = make_mask(Rep),
    Mask1 = Mask band (bnot High),
    bits:clear_extra_bits(Mask1, Bits).

is_straight_flush(Rep) ->
    Mask = make_mask(Rep),
    case is_flush(Mask, Rep) of
	{_, High, _} ->
	    case is_straight([High, High, High, High]) of
		{_, High1, _} ->
		    {straight_flush, High1, 0};
		_ ->
		    junk
	    end;
	_ ->
	    junk
    end.

is_flush(Rep) ->
    Mask = make_mask(Rep),
    is_flush(Mask, Rep).

is_flush(Mask, [H|T]) ->
    Score = Mask band H,
    Count = bits:bits1(Score),
    if 
	Count < 5 ->
	    is_flush(Mask, T);
	true ->
	    {flush, bits:clear_extra_bits(Score, 5), 0}
    end;

is_flush(_, []) ->
    junk.

is_straight(Rep) ->
    Temp = make_mask(Rep),
    if             %AKQJT98765432A
	Temp band 2#10000000000000 > 0 ->
	    Value = Temp bor 1;
	true ->
	    Value = Temp
    end,                %AKQJT98765432A
    is_straight(Value, 2#11111000000000).

is_straight(_, Mask) when Mask < 2#11111 ->
    junk;

is_straight(Value, Mask) when Mask >= 2#11111 ->
    if 
	Value band Mask =:= Mask ->
	    {straight, Mask, 0};
	true ->
	    is_straight(Value, Mask bsr 1)
    end.
	
is_four_kind([C, D, H, S]) ->
    Value = C band D band H band S,
    if
	Value > 0 ->
	    {four_kind, Value, score([C, D, H, S], Value, 1)};
	true ->
	    junk
    end.

is_full_house(Rep) ->
    case is_three_kind(Rep) of
	{_, High3, _} ->
	    case is_pair(clear_high_bit(Rep, High3)) of
		{_, High2, _} ->
		    Score = (High3 bsl 16) bor High2,
		    {full_house, Score, 0};
		_ -> 
		    junk
	    end;
	_ ->
	    junk
    end.

is_three_kind([C, D, H, S]) ->
    L = lists:sort(fun(A, B) ->
			   A > B
		   end, [C band D band H,
			 D band H band S,
			 H band S band C,
			 S band C band D]),
    is_three_kind(L, [C, D, H, S]).

is_three_kind([H|T], Rep) ->
    if 
	H > 0 ->
	    {three_kind, high_bit(H), score(Rep, H, 2)};
	true ->
	    is_three_kind(T, Rep)
    end;

is_three_kind([], _Rep) ->
    junk.

is_two_pair(Rep) ->
    case is_pair(Rep) of
	{pair, High1, _} ->
	    Rep1 = clear_high_bit(Rep, High1),
	    case is_pair(Rep1) of
		{pair, High2, _} ->
		    High = High1 bor High2, 
		    {two_pair, High1 bor High2, score(Rep, High, 1)};
		_ ->
		    junk
	    end;
	_ ->
	    junk
    end.

is_pair([C, D, H, S]) ->
    L = lists:sort(fun(A, B) ->
			   A > B
		   end, [C band D,
			 D band H,
			 H band S,
			 S band C,
			 C band H,
			 D band S]),
    is_pair(L, [C, D, H, S]).

is_pair([H|T], Rep) ->
    if 
	H > 0 ->
	    {pair, high_bit(H), score(Rep, H, 3)};
	true ->
	    is_pair(T, Rep)
    end;

is_pair([], _Rep) ->
    junk.

rank_value(Rank) when atom(Rank) ->
    case Rank of
	straight_flush -> 8;
	four_kind -> 7;
	full_house -> 6;
	flush -> 5;
	straight -> 4;
	three_kind -> 3;
	two_pair -> 2;
	pair -> 1;
	_ -> 0
    end.

%% Make a list of {face, suit} tuples 
%% from a space-delimited string 
%% such as "AD JC 5S"

make_cards(S)
  when is_list(S) ->
    lists:map(fun make_card/1, 
	      string:tokens(S, " ")).

%% Make a single card tuple

make_card([H, T]) ->
    Rank = case H of 
	       $2 -> two;
	       $3 -> three;
	       $4 -> four;
	       $5 -> five;
	       $6 -> six;
	       $7 -> seven;
	       $8 -> eight;
	       $9 -> nine;
	       $T -> ten;
	       $J -> jack;
	       $Q -> queen;
	       $K -> king;
	       $A -> ace
	   end,
    Suit = case T of 
	       $C -> clubs;
	       $D -> diamonds;
	       $H -> hearts;
	       $S -> spades
	   end,
    {Rank, Suit}.

face(Face) when atom(Face)->
    1 bsl case Face of
	      ace -> 13;
	      king -> 12;
	      queen -> 11;
	      jack -> 10;
	      ten -> 9;
	      nine -> 8;
	      eight -> 7;
	      seven -> 6;
	      six -> 5;
	      five -> 4;
	      four -> 3;
	      three -> 2;
	      two -> 1
	  end;

face(X) when is_number(X) ->
    face(X, [ace, king, queen, jack, ten, nine,
	     eight, seven, six, five, four, three, two]).

face(_X, []) ->
    none;

face(X, [Face|Rest]) ->
    Match = (X band face(Face)) > 0,
    if 
	Match ->
	    Face;
	true ->
	    face(X, Rest)
    end.

suit(Suit) when is_atom(Suit) ->
    case Suit of 
	clubs -> 1;
	diamonds -> 2;
	hearts -> 3;
	spades -> 4
    end;

suit(Suit) when is_number(Suit) ->
    case Suit of 
	1 -> clubs;
	2 -> diamonds;
	3 -> hearts;
	4 -> spades
    end.
    
%%%
%%% Test suite
%%%

test() ->
    test_make_rep(),
    test_rank_1(),
    test_rank_2(),
    test_rank_3(),
    test_rank_4(),
    test_rank_5(),
    test_rank_6(),
    test_rank_7(),
    test_rank_8(),
    test_rank_9(),
    test_winner_1(),
    test_winner_2(),
    test_winner_3(),
    test_winner_4(),
    test_winner_5(),
    test_winner_6(),
    test_winner_7(),
    test_winner_8(),
    test_winner_9(),
    test_winner_10(),
    test_winner_11(),
    test_winner_12(),
    ok.

test_make_rep() ->
    %%  AKQJT98765432A
    [2#00000010000000,
     2#00101000011000,
     2#00010001000000,
     2#00000000000000]
	= make_rep(make_cards("4D JH 5D 8C QD TD 7H")).

-define(score(Cards),
	score(make_rep(make_cards(Cards)))).

test_rank_1() ->
    ?match({junk, 2#00111011000000, 0},
	   ?score("4D JH 5D 8C QD TD 7H")),
    ?match({junk, 2#11000110010000, 0},
	   ?score("8C AD 5H 3S KD 9D 4D")),
    ?match({junk, 2#00110010011000, 0},
	   ?score("4C JH 5C 8D QC 2C 3D")).
    
test_rank_2() ->
    ?match({pair, 2#00000000000100, 2#01100100000000},
	   ?score("KD 3S 5H 3D 6C QH 9S")),
    ?match({pair, 2#10000000000000, 2#01000100010000},
	   ?score("AC 2D 5D AS 4H 9D KD")),
    ?match({pair, 2#00000000000100, 2#01011000000000},
	   ?score("9S JH 5D TS 3C KC 3H")).

test_rank_3() ->
    ?match({two_pair, 2#01100000000000, 2#00010000000000},
	   ?score("QC KD JD QD JC 5C KC")),
    ?match({two_pair, 2#00000001100000, 2#00010000000000},
	   ?score("7H 3H 6C TD 7C JH 6H")),
    ?match({two_pair, 2#00010000010000, 2#00100000000000},
	   ?score("4D 3S 5H JD JC QH 5S")),
    ?match({two_pair, 2#10000000010000, 2#00000100000000},
	   ?score("AC 2D 5D AS 5H 9D 4D")),
    ?match({two_pair, 2#00010000010000, 2#01000000000000},
	   ?score("9S JH 5D JS 5C KC 3D")).

test_rank_4() ->
    ?match({three_kind, 2#00100000000000, 2#01000100000000},
	   ?score("KH 9S 5H QD QC QH 3S")),
    ?match({three_kind, 2#01000000000000, 2#10000100000000},
	   ?score("AC KC KD KS 7H 9D 4D")),
    ?match({three_kind, 2#00100000000000, 2#01001000000000},
	   ?score("KS TS QD QS QH 4C 5D")).

test_rank_5() ->
    ?match({straight, 2#01111100000000, 0},
	   ?score("KC QS JH TC 9C 4D 3S")),
    ?match({straight, 2#11111000000000, 0},
	   ?score("AC KS QH JC TC 9D 4D")),
    ?match({straight, 2#01111100000000, 0},
	   ?score("KS QD JS TC 9S 2D 7S")),
    ?match({straight, 2#00000000011111, 0},
	   ?score("5C 4D 3H 2C AD 7H 9S")),
    ?match({straight, 2#00000011111000, 0},
	   ?score("5H 4S JC 8S 7D 6C 3C")).

test_rank_6() ->
    ?match({flush, 2#00110000011010, 0},
	   ?score("4D JD 5D JC QD 2D 7H")),
    ?match({flush, 2#11000100011000, 0},
	   ?score("8C AD 5D AS KD 9D 4D")),
    ?match({flush, 2#00110000011100, 0},
	   ?score("4C JC 5C 8D QC 3C 7S")).

test_rank_7() ->
    ?match({full_house, (2#00010000000000 bsl 16) bor 2#00100000000000, 0},
 	   ?score("4D JS 5H JD JC QH QS")),
    ?match({full_house, (2#10000000000000 bsl 16) bor 2#01000000000000, 0},
 	   ?score("AC AD KD AS KH 9D 4D")),
    ?match({full_house, (2#00010000000000 bsl 16) bor 2#01000000000000, 0},
 	   ?score("3S JH JD JS KH KC 5D")),
    ?match({full_house, (2#00100000000000 bsl 16) bor 2#00001000000000, 0},
	   ?score("TD QH TH TC 6C QD QC")).

test_rank_8() ->
    ?match({four_kind, 2#00100000000000, 2#10000000000000},
	   ?score("4D AS 5H QD QC QH QS")),
    ?match({four_kind, 2#01000000000000, 2#10000000000000},
	   ?score("AC KC KD KS KH 9D 4D")),
    ?match({four_kind, 2#00100000000000, 2#01000000000000},
	   ?score("KS TS QD QS QH QC 5D")).

test_rank_9() ->
    ?match({straight_flush, 2#01111100000000, 0},
	   ?score("KC QC JC TC 9C 4D AS")),
    ?match({straight_flush, 2#11111000000000, 0},
	   ?score("AC KC QC JC TC 9D 4D")),
    ?match({straight_flush, 2#01111100000000, 0},
	   ?score("KS QS JS TS 9S AD 7S")).

test_winner_1() ->
    S1 = ?score("4D JH 5D 8C QD TD 7H"),
    S2 = ?score("8C AD 5H 3S KD 9D 4D"),
    S3 = ?score("4C JH 5C 8D QC 2C 3D"),
    ?match(junk, element(1, S1)),
    ?match(junk, element(1, S2)),
    ?match(junk, element(1, S3)),
    ?match(true, S2 > S1),
    ?match(true, S2 > S3),
    ?match(true, S1 > S3).

test_winner_2() ->
    S1 = ?score("KD 3S 5H 3D 6C QH 9S"),
    S2 = ?score("AC 2D 5D AS 4H 9D KD"),
    S3 = ?score("9S JH 5D TS 3C KC 3H"),
    ?match(pair, element(1, S1)),
    ?match(pair, element(1, S2)),
    ?match(pair, element(1, S3)),
    ?match(true, S2 > S1),
    ?match(true, S2 > S3),
    ?match(true, S1 > S3).

test_winner_3() ->
    S1 = ?score("4D 3S 5H JD JC QH 5S"),
    S2 = ?score("AC 2D 5D AS 5H 9D 4D"),
    S3 = ?score("9S JH 5D JS 5C KC 3D"),
    ?match(two_pair, element(1, S1)),
    ?match(two_pair, element(1, S2)),
    ?match(two_pair, element(1, S3)),
    ?match(true, S2 > S1),
    ?match(true, S2 > S3),
    ?match(true, S3 > S1).
    
test_winner_4() ->
    S1 = ?score("KH 9S 5H QD QC QH 3S"),
    S2 = ?score("AC KC KD KS 7H 9D 4D"),
    S3 = ?score("KS TS QD QS QH 4C 5D"),
    ?match(three_kind, element(1, S1)),
    ?match(three_kind, element(1, S2)),
    ?match(three_kind, element(1, S3)),
    ?match(true, S2 > S1),
    ?match(true, S2 > S3),
    ?match(true, S3 > S1).

test_winner_5() ->
    S1 = ?score("KC QS JH TC 9C 4D 3S"),
    S2 = ?score("AC KS QH JC TC 9D 4D"),
    S3 = ?score("KS QD JS TC 9S 2D 7S"),
    ?match(straight, element(1, S1)),
    ?match(straight, element(1, S2)),
    ?match(straight, element(1, S3)),
    ?match(true, S2 > S1),
    ?match(true, S2 > S3),
    ?match(true, S1 == S3).

test_winner_6() ->
    S1 = ?score("4D JD 5D JC QD 2D 7H"),
    S2 = ?score("8C AD 5D AS KD 9D 4D"),
    S3 = ?score("4C JC 5C 8D QC 3C 7S"),
    S4 = ?score("4C JC 7C 8D QC 5C 7S"),
    ?match(flush, element(1, S1)),
    ?match(flush, element(1, S2)),
    ?match(flush, element(1, S3)),
    ?match(flush, element(1, S4)),
    ?match(true, S2 > S1),
    ?match(true, S2 > S3),
    ?match(true, S3 > S1),
    ?match(true, S4 > S1).

test_winner_7() ->
    S1 = ?score("4D AS 5H QD QC QH QS"),
    S2 = ?score("AC KC KD KS KH 9D 4D"),
    S3 = ?score("KS TS QD QS QH QC 5D"),
    ?match(four_kind, element(1, S1)),
    ?match(four_kind, element(1, S2)),
    ?match(four_kind, element(1, S3)),
    ?match(true, S2 > S1),
    ?match(true, S2 > S3),
    ?match(true, S1 > S3).

test_winner_8() ->
    S1 = ?score("KC QC JC TC 9C 4D AS"),
    S2 = ?score("AC KC QC JC TC 9D 4D"),
    S3 = ?score("KS QS JS TS 9S AD 7S"),
    ?match(straight_flush, element(1, S1)),
    ?match(straight_flush, element(1, S2)),
    ?match(straight_flush, element(1, S3)),
    ?match(true, S2 > S1),
    ?match(true, S2 > S3),
    ?match(true, S1 == S3).

test_winner_9() ->
    S1 = ?score("4D JS 5H JD JC QH QS"),
    S2 = ?score("AC AD KD AS KH 9D 4D"),
    S3 = ?score("3S JH JD JS KH KC 5D"),
    ?match(full_house, element(1, S1)),
    ?match(full_house, element(1, S2)),
    ?match(full_house, element(1, S3)),
    ?match(true, S2 > S1),
    ?match(true, S2 > S3),
    ?match(true, S3 > S1).

test_winner_10() ->
    S1 = ?score("5C TC 7H KH 5S TS KS"),
    S2 = ?score("5C TC 7H KH 5S KC TH"),
    ?match(two_pair, element(1, S1)),
    ?match(two_pair, element(1, S2)),
    ?match(true, S1 == S2).

test_winner_11() ->    
    S1 = ?score("KH TC 9H 7D 6H 5D 2S"),
    S2 = ?score("KH TC 9H 7H 6H 3D 2S"),
    ?match(junk, element(1, S1)),
    ?match(junk, element(1, S2)),
    ?match(true, S1 == S2).

test_winner_12() ->    
    S1 = ?score("2H 2C 5H 5S 5C 7C 4D"),
    S2 = ?score("2H 2C 5H 5S 5D 4D 2D"),
    ?match(full_house, element(1, S1)),
    ?match(full_house, element(1, S2)),
    ?match(true, S1 == S2).

print_bin(X) ->
    io:format("AKQJT98765432A~n"),
    io:format("~14.2.0B~n", [X]).

print_rep({C, D, H, S}) ->
    io:format("   AKQJT98765432A~n"),
    io:format("C: ~14.2.0B~n", [C]),
    io:format("D: ~14.2.0B~n", [D]),
    io:format("H: ~14.2.0B~n", [H]),
    io:format("S: ~14.2.0B~n", [S]).
    
