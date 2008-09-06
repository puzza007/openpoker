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

-module(ircdb).
-export([convert/1]).

-include("ircdb.hrl").

convert(Dir) ->
    HandFile = Dir ++ "/hdb",
    PlayerFile = Dir ++ "/pdb",
    Ets = ets:new(ircdb, [{keypos, 2}]),
    read_hand_file(Ets, HandFile),
    read_player_file(Ets, PlayerFile),
    {ok, Dets} = dets:open_file(ircdb, [{file, "ircdb.dat"},
					{keypos, 2},
					{ram_file, true}]),   
    ets:to_dets(Ets, Dets),
    dets:close(Dets).

read_hand_file(Db, File) ->
    {ok, B} = file:read_file(File),
    read_hand_data(Db, B).

read_hand_data(_Db, <<>>) ->
    ok;

read_hand_data(Db, B) ->
    {Line, Rest} = read_line(B),
    parse_hand(Db, Line),
    read_hand_data(Db, Rest).

parse_hand(Db, B) ->
    {Id, B1} = read_int(B), % game id
    {_, B2} = read_word(B1), % set#
    {_, B3} = read_word(B2), % game#
    {PlayerCount, B4} = read_int(B3), % #irc_players
    {FC, FA, B5} = read_split_int(B4), % flop 
    {TC, TA, B6} = read_split_int(B5), % turn
    {RC, RA, B7} = read_split_int(B6), % river
    {SC, SA, B8} = read_split_int(B7), % showdown
    Cards = read_cards(B8),
    Game = #irc_game {
      id = Id,
      player_count = PlayerCount,
      stages = [{FC, FA}, {TC, TA}, {RC, RA}, {SC, SA}],
      board = Cards,
      players = erlang:make_tuple(PlayerCount, none)
     },
    ets:insert(Db, Game).

read_player_file(Db, File) ->
    {ok, B} = file:read_file(File),
    read_player_data(Db, B, 0).

read_player_data(_Db, <<>>, _) ->
    ok;

read_player_data(Db, B, N) ->
    {Line, Rest} = read_line(B),
    parse_player(Db, Line),
    read_player_data(Db, Rest, N + 1).

parse_player(Db, B) ->
    {Nick, B1} = read_string(B), % nick
    {Id, B2} = read_int(B1), % game id
    {_PlayerCount, B3} = read_int(B2), % #irc_players
    {SeatNum, B4} = read_int(B3), % seat#
    {PA, B5} = read_actions(B4), % preflop actions
    {FA, B6} = read_actions(B5), % flop actions
    {TA, B7} = read_actions(B6), % turn actions
    {RA, B8} = read_actions(B7), % river actions
    {Balance, B9} = read_int(B8), % balance
    {TotalAction, B10} = read_int(B9), % total action
    {Win, B11} = read_int(B10), % amount won
    Cards = read_cards(B11), % pocket cards
    Player = #irc_player {
      nick = Nick,
      actions = PA ++ FA ++ TA ++ RA,
      cards = Cards,
      balance = Balance,
      total_action = TotalAction,
      win = Win
     },
    %% lookup database record
    [Game] = ets:lookup(Db, Id),
    NewGame = Game#irc_game {
		players = setelement(SeatNum,
				     Game#irc_game.players,
				     Player)
	       },
    ets:insert(Db, NewGame).

%%
%% Utility
%%

slurp(B, X) ->
    slurp(B, X, 0).

slurp(B, X, N) ->
    case B of
	<<X, B2/binary>> ->
	    slurp(B2, X, N);
	<<B1:N/binary, X, B2/binary>> ->
	    {B1, B2};
	<<_:N/binary>> = B ->
	    {B, <<>>};
	<<_:N/binary, _/binary>> = B ->
	    slurp(B, X, N + 1)
    end.

read_line(B) ->
    slurp(B, $\n).

read_word(B) ->
    slurp(B, $\s).

read_int(B) ->
    {Temp, B1} = read_word(B),
    List = binary_to_list(Temp),
    Int = list_to_integer(List),
    {Int, B1}.

read_split_int(B) ->    
    {Temp, B1} = read_word(B),
    {T1, T2} = slurp(Temp, $\/),
    {I1, _} = read_int(T1),
    {I2, _} = read_int(T2),
    {I1, I2, B1}.

read_string(B) ->
    {Temp, B1} = read_word(B),
    Str = binary_to_list(Temp),
    {Str, B1}.

make_card([R, S]) ->
    Rank = case R of 
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
    Suit = case S of 
	       $c -> clubs;
	       $d -> diamonds;
	       $h -> hearts;
	       $s -> spades
	   end,
    {Rank, Suit}.

read_cards(<<>>) ->
    [];

read_cards(B) when is_binary(B) ->
    read_cards(B, []).

read_cards(<<>>, Acc) ->
    lists:reverse(Acc);

read_cards(B, Acc) when is_binary(B) ->
    case read_word(B) of
	{<<>>, _} ->
	    Acc;
	{Temp, B1} -> 
	    Temp1 = binary_to_list(Temp),
	    Card = make_card(Temp1),
	    read_cards(B1, [Card|Acc])
    end.

read_actions(B) when is_binary(B) ->
    {Temp, B1} = read_word(B),
    List = binary_to_list(Temp),
    Actions = read_actions(List, []),
    {Actions, B1}.

action(X) ->
    case X of
	$B ->
	    'BLIND';
	$f ->
	    'FOLD';
	$k ->
	    'CHECK';
	$b ->
	    'BET';
	$c ->
	    'CALL';
	$r ->
	    'RAISE';
	$K ->
	    'FOLD';
	$Q ->
	    'FOLD';
	$- ->
	    none;
	X ->
	    io:format("Unknown action: ~c~n", [X]),
	    X
    end.
	   
read_actions([], Acc) ->
    lists:reverse(Acc);

read_actions([A, $\A|Rest], Acc) ->
    read_actions(Rest, [{action(A), allin}|Acc]);

read_actions([A|Rest], Acc) ->
    read_actions(Rest, [action(A)|Acc]).
