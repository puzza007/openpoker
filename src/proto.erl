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

-module(proto).

-export([read/1, write/1, test/0]).

-include("test.hrl").
-include("common.hrl").
-include("proto.hrl").
-include("schema.hrl").

%%% Client -> Server

read(<<?PP_GOOD, Cmd, Extra:32>>) ->
    {?PP_GOOD, Cmd, Extra};

read(<<?PP_BAD, Cmd, Error>>) ->
    {?PP_BAD, Cmd, Error};

read(<<?PP_LOGIN, Bin/binary>>) ->
    {Nick, Bin1} = read_string(Bin),
    {Pass, _} = read_string(Bin1),
    {?PP_LOGIN, Nick, Pass};

read(<<Cmd>>) 
  when Cmd == ?PP_PING;
       Cmd == ?PP_PONG;
       Cmd == ?PP_LOGOUT;
       Cmd == ?PP_BALANCE_REQ ->
    Cmd;

read(<<?PP_HANDOFF, Port:16, Bin/binary>>) ->
    {Host, _} = read_string(Bin),
    {?PP_HANDOFF, Port, Host};

read(<<?PP_PID, PID:32>>) ->
    {?PP_PID, PID};

read(<<?PP_JOIN, GID:32, SeatNum, BuyIn:32>>) ->
    case find_game(GID) of
	Pid when is_pid(Pid) ->
	    {?PP_JOIN, Pid, SeatNum, BuyIn / 100};
	Any ->
	    io:format("JOIN ~w: ~w~n", [GID, Any]),
	    Any
    end;

read(<<Cmd, GID:32>>) 
  when Cmd == ?PP_WATCH; 
       Cmd == ?PP_UNWATCH;
       Cmd == ?PP_SIT_OUT;
       Cmd == ?PP_COME_BACK;
       Cmd == ?PP_FOLD;
       Cmd == ?PP_LEAVE ->
    case find_game(GID) of
	Pid when is_pid(Pid) ->
	    {Cmd, Pid};
	Any ->
	    Any
    end;

read(<<Cmd, GID:32, Amount:32>>) 
  when Cmd == ?PP_CALL;
       Cmd == ?PP_RAISE ->
    case find_game(GID) of
	Pid when is_pid(Pid) ->
	    {Cmd, Pid, Amount / 100};
	Any ->
	    Any
    end;

read(<<?PP_CHAT, GID:32, Bin/binary>>) ->
    {Message, _} = read_string(Bin),
    case find_game(GID) of
	Pid when is_pid(Pid) ->
	    {?PP_CHAT, Pid, Message};
	Any ->
	    Any
    end;

read(<<?PP_GAME_QUERY, 
      GameType, LimitType,
      ExpOp, Expected, 
      JoinOp, Joined,
      WaitOp, Waiting>>) ->
    {?PP_GAME_QUERY, 
     GameType, LimitType,
     ExpOp, Expected, 
     JoinOp, Joined,
     WaitOp, Waiting};

read(<<?PP_SEAT_QUERY, GID:32>>) ->
    case find_game(GID) of
	Pid when is_pid(Pid) ->
	    {?PP_SEAT_QUERY, Pid};
	Any ->
	    io:format("PLAYER_QUERY ~w: ~w~n", [GID, Any]),
	    Any
    end;

read(<<?PP_PLAYER_INFO_REQ, PID:32>>) ->
    {?PP_PLAYER_INFO_REQ, PID};

read(<<?PP_SEAT_STATE, GID:32, SeatNum, State, PID:32>>) ->
    {?PP_SEAT_STATE, GID, SeatNum, State, PID};

%%% Server -> Client

read(<<?PP_GAME_INFO, GID:32, GameType,
      Expected, Joined, Waiting,
      LimitType, Bin/binary>>) ->
    case LimitType of 
	?LT_FIXED_LIMIT ->
	    <<Low:32, High:32>> = Bin,
	    Limit = {?LT_FIXED_LIMIT, Low / 100, High / 100}
    end,
    {?PP_GAME_INFO, GID, GameType, Expected, Joined, Waiting, Limit};
	    
read(<<?PP_PLAYER_INFO, PID:32, InPlay:32, Bin/binary>>) ->
    {Nick, Bin1} = read_string(Bin),
    {Location, _} = read_string(Bin1),
    {?PP_PLAYER_INFO, PID, InPlay / 100, Nick, Location};

read(<<?PP_BET_REQ, GID:32, Call:32, Min:32, Max:32>>) ->
    {?PP_BET_REQ, GID, Call / 100, Min / 100, Max / 100};

read(<<Cmd, GID:32, Face, Suit, Seq:16>>)
  when Cmd == ?PP_NOTIFY_DRAW;
       Cmd == ?PP_NOTIFY_SHARED ->
    {Cmd, GID, {hand:face(1 bsl Face), hand:suit(Suit)}, Seq}; 

read(<<?PP_NOTIFY_JOIN, GID:32, PID:32, SeatNum, Seq:16>>) -> 
    {?PP_NOTIFY_JOIN, GID, PID, SeatNum, Seq};

read(<<Cmd, GID:32, PID:32, Seq:16>>)
  when Cmd == ?PP_NOTIFY_PRIVATE;
       Cmd == ?PP_NOTIFY_LEAVE ->
    {Cmd, GID, PID, Seq};

read(<<?PP_NOTIFY_CHAT, GID:32, PID:32, Seq:16, Bin/binary>>) ->
    {Message, _} = read_string(Bin),
    {?PP_NOTIFY_CHAT, GID, PID, Seq, Message};
    
read(<<Cmd, GID:32, Seq:16>>)
  when Cmd == ?PP_NOTIFY_START_GAME;
       Cmd == ?PP_NOTIFY_CANCEL_GAME;
       Cmd == ?PP_NOTIFY_END_GAME ->
    {Cmd, GID, Seq};

read(<<Cmd, GID:32, PID:32, Amount:32, Seq:16>>)
  when Cmd == ?PP_NOTIFY_WIN;
       Cmd == ?PP_NOTIFY_CALL;
       Cmd == ?PP_NOTIFY_RAISE;
       Cmd == ?PP_NOTIFY_BET ->
    {Cmd, GID, PID, Amount / 100, Seq};

%% Player state change

read(<<?PP_PLAYER_STATE, GID:32, PID:32, State, Seq:16>>) ->
    {?PP_PLAYER_STATE, GID, PID, State, Seq};
    
%% Game stage

read(<<?PP_GAME_STAGE, GID:32, Stage, Seq:16>>) ->
    {?PP_GAME_STAGE, GID, Stage, Seq};

read(<<?PP_NEW_GAME_REQ, GameType, Expected, LimitType, Bin/binary>>) ->
    case LimitType of 
	?LT_FIXED_LIMIT ->
	    <<Low:32, High:32>> = Bin,
	    Limit = {?LT_FIXED_LIMIT, Low / 100, High / 100}
    end,
    {?PP_NEW_GAME_REQ, GameType, Expected, Limit};
	    
read(<<?PP_BALANCE_INFO, Balance:32, Inplay:32>>) ->
    {?PP_BALANCE_INFO, Balance / 100, Inplay / 100};

read(<<Cmd, GID:32, SeatNum, Seq:16>>) 
  when Cmd == ?PP_NOTIFY_BUTTON;
       Cmd == ?PP_NOTIFY_SB;
       Cmd == ?PP_NOTIFY_BB ->
    {Cmd, GID, SeatNum, Seq};

read(<<?PP_MAKE_TEST_GAME, Bin/binary>>) ->
    {?PP_MAKE_TEST_GAME, Bin};
	    
%% Catch-all

read(Bin) when is_binary(Bin) ->
    none.

%%% Client -> Server

write(Cmd) 
  when Cmd == ?PP_LOGOUT;
       Cmd == ?PP_PING;
       Cmd == ?PP_PONG;
       Cmd == ?PP_BALANCE_REQ ->
    <<Cmd>>;

write({?PP_GOOD, Cmd, Extra}) ->
    <<?PP_GOOD, Cmd, Extra:32>>;

write({?PP_BAD, Cmd, Error}) ->
    <<?PP_BAD, Cmd, Error>>;

write({?PP_HANDOFF, Port, Host}) 
  when is_number(Port),
       is_list(Host) ->
    L = [?PP_HANDOFF, <<Port:16>>, length(Host)|Host],
    list_to_binary(L);

write({?PP_LOGIN, Nick, Pass})
  when is_list(Nick), 
       is_list(Pass) ->
    L1 = [length(Pass)|Pass],
    L2 = [?PP_LOGIN, length(Nick), Nick|L1],
    list_to_binary(L2);

write({?PP_PID, PID}) 
  when is_number(PID) ->
    <<?PP_PID, PID:32>>;

write({?PP_JOIN, GID, SeatNum, BuyIn})
  when is_number(GID), 
       is_number(SeatNum) ->
    <<?PP_JOIN, GID:32, SeatNum, (trunc(BuyIn * 100)):32>>;

write({Cmd, GID}) 
  when Cmd == ?PP_WATCH, is_number(GID);
       Cmd == ?PP_UNWATCH, is_number(GID);
       Cmd == ?PP_SIT_OUT, is_number(GID);
       Cmd == ?PP_COME_BACK, is_number(GID);
       Cmd == ?PP_JOIN, is_number(GID);
       Cmd == ?PP_FOLD, is_number(GID);
       Cmd == ?PP_LEAVE, is_number(GID) ->
    <<Cmd, GID:32>>;

write({Cmd, GID, Amount}) 
  when Cmd == ?PP_CALL, is_number(GID);
       Cmd == ?PP_RAISE, is_number(GID) ->
    <<Cmd, GID:32, (trunc(Amount * 100)):32>>;

write({?PP_CHAT, GID, Msg})
  when is_number(GID), 
       is_list(Msg) ->
    list_to_binary([?PP_CHAT, <<GID:32>>, length(Msg)|Msg]);
    
write({?PP_GAME_QUERY, 
       GameType, LimitType,
       ExpOp, Expected, 
       JoinOp, Joined,
       WaitOp, Waiting}) ->
    <<?PP_GAME_QUERY, 
     GameType, LimitType,
     ExpOp, Expected, 
     JoinOp, Joined,
     WaitOp, Waiting>>;

write({?PP_SEAT_QUERY, GID}) ->
    <<?PP_SEAT_QUERY, GID:32>>;

write({?PP_PLAYER_INFO_REQ, PID}) ->
    <<?PP_PLAYER_INFO_REQ, PID:32>>;

write({?PP_SEAT_STATE, GID, SeatNum, State, PID}) 
  when State == ?SS_EMPTY;
       State == ?SS_RESERVED;
       State == ?SS_TAKEN ->
    <<?PP_SEAT_STATE, GID:32, SeatNum, State, PID:32>>;

write({?PP_NEW_GAME_REQ, GameType, Expected, LimitType}) ->
    case LimitType of 
	{?LT_FIXED_LIMIT, Low, High} ->
	    <<?PP_NEW_GAME_REQ, 
	     GameType, 
	     Expected,
	     ?LT_FIXED_LIMIT, 
	     (trunc(Low * 100)):32,
	     (trunc(High * 100)):32>>;
	_ ->
	    none
    end;

%%% Server -> Client

write({?PP_GAME_INFO, GID, GameType, 
       Expected, Joined, Waiting,
       {?LT_FIXED_LIMIT, Low, High}})
  when is_number(GID),
       is_number(GameType),
       is_number(Expected), 
       is_number(Joined),
       is_number(Waiting),
       is_number(Low),
       is_number(High) ->
    <<?PP_GAME_INFO, GID:32, 
     GameType, Expected, Joined, Waiting,
     ?LT_FIXED_LIMIT, 
     (trunc(Low * 100)):32, 
     (trunc(High * 100)):32>>;

write({?PP_PLAYER_INFO, Player, InPlay, Nick, Location})
  when is_pid(Player),
       is_number(InPlay),
       is_list(Nick),
       is_list(Location) ->     
    PID = gen_server:call(Player, 'ID'),
    L1 = [length(Location)|Location],
    L2 = [?PP_PLAYER_INFO, <<PID:32>>, 
	  <<(trunc(InPlay * 100)):32>>, 
	  length(Nick), Nick|L1],
    list_to_binary(L2);

write({?PP_BET_REQ, Game, Call, Min, Max})
  when is_pid(Game),
       is_number(Call),
       is_number(Min),
       is_number(Max) ->
    GID = cardgame:call(Game, 'ID'),
    <<?PP_BET_REQ, GID:32, 
     (trunc(Call * 100)):32, 
     (trunc(Min * 100)):32, 
     (trunc(Max * 100)):32>>;

write({Cmd, GID, {Face, Suit}, Seq})
  when Cmd == ?PP_NOTIFY_DRAW, 
       is_number(GID), 
       is_atom(Face), 
       is_atom(Suit),
       is_number(Seq);
       Cmd == ?PP_NOTIFY_SHARED, 
       is_number(GID), 
       is_atom(Face), 
       is_atom(Suit),
       is_number(Seq) ->
    <<Cmd, GID:32, 
     (bits:log2(hand:face(Face))), 
     (hand:suit(Suit)), Seq:16>>;

write({?PP_NOTIFY_JOIN, GID, Player, SeatNum, Seq})
when is_number(GID),
     is_pid(Player),
     is_number(SeatNum),
     is_number(Seq) ->
    PID = gen_server:call(Player, 'ID'),
    <<?PP_NOTIFY_JOIN, GID:32, PID:32, SeatNum, Seq:16>>;

write({Cmd, GID, Player, Seq}) 
  when Cmd == ?PP_NOTIFY_PRIVATE,
       is_number(GID),
       is_pid(Player),
       is_number(Seq);
       Cmd == ?PP_NOTIFY_LEAVE,
       is_number(GID),
       is_pid(Player),
       is_number(Seq) ->
    PID = gen_server:call(Player, 'ID'),
    <<Cmd, GID:32, PID:32, Seq:16>>;

write({?PP_NOTIFY_CHAT, GID, Player, Seq, Msg})
  when is_number(GID),
       is_pid(Player),
       is_number(Seq),
       is_list(Msg) ->
    PID = gen_server:call(Player, 'ID'),
    L1 = [length(Msg)|Msg],
    L2 = [?PP_NOTIFY_CHAT, <<GID:32>>, <<PID:32>>, <<Seq:16>>|L1],
    list_to_binary(L2);
    
write({?PP_NOTIFY_CHAT, GID, 0, Seq, Msg})
  when is_number(GID),
       is_number(Seq),
       is_list(Msg) ->
    PID = 0,
    L1 = [length(Msg)|Msg],
    L2 = [?PP_NOTIFY_CHAT, <<GID:32>>, <<PID:32>>, <<Seq:16>>|L1],
    list_to_binary(L2);
    
write({Cmd, GID, Seq}) 
  when Cmd == ?PP_NOTIFY_START_GAME, 
       is_number(GID),
       is_number(Seq);
       Cmd == ?PP_NOTIFY_CANCEL_GAME, 
       is_number(GID),
       is_number(Seq);       
       Cmd == ?PP_NOTIFY_END_GAME,
       is_number(GID),
       is_number(Seq) ->
    <<Cmd, GID:32, Seq:16>>;

write({Cmd, GID, Player, Amount, Seq})
  when Cmd == ?PP_NOTIFY_WIN, 
       is_number(GID),
       is_pid(Player),
       is_number(Amount),
       is_number(Seq);
       Cmd == ?PP_NOTIFY_CALL, 
       is_number(GID),
       is_pid(Player),
       is_number(Amount),
       is_number(Seq);
       Cmd == ?PP_NOTIFY_RAISE, 
       is_number(GID),
       is_pid(Player),
       is_number(Amount),
       is_number(Seq);
       Cmd == ?PP_NOTIFY_BET, 
       is_number(GID),
       is_pid(Player),
       is_number(Amount),
       is_number(Seq) ->
    PID = gen_server:call(Player, 'ID'),
    <<Cmd, GID:32, PID:32, (trunc(Amount * 100)):32, Seq:16>>;

%% Player state change

write({?PP_PLAYER_STATE, GID, Player, State, Seq})
  when is_number(State),
       is_number(GID),
       is_pid(Player),
       is_number(Seq) ->
    PID = gen_server:call(Player, 'ID'),
    <<?PP_PLAYER_STATE, GID:32, PID:32, State, Seq:16>>;
    
%% Game stage

write({?PP_GAME_STAGE, GID, Stage, Seq})
  when is_number(GID),
       is_number(Stage),
       is_number(Seq) ->
    <<?PP_GAME_STAGE, GID:32, Stage, Seq:16>>;

%% Player balance

write({?PP_BALANCE_INFO, Balance, Inplay}) ->
    <<?PP_BALANCE_INFO, 
     (trunc(Balance * 100)):32,
     (trunc(Inplay * 100)):32>>;

write({Cmd, GID, SeatNum, Seq}) 
  when Cmd == ?PP_NOTIFY_BUTTON;
       Cmd == ?PP_NOTIFY_SB;
       Cmd == ?PP_NOTIFY_BB ->
    <<Cmd, GID:32, SeatNum, Seq:16>>;

write(Tuple) 
  when is_tuple(Tuple) ->
    none.

read_string(Bin) ->
    <<Len, Bin1/binary>> = Bin,
    <<Str:Len/binary-unit:8, Rest/binary>> = Bin1,
    {binary_to_list(Str), Rest}.

find_game(GID) ->
    case db:find(game_xref, GID) of
	{atomic, [XRef]} ->
	    XRef#game_xref.pid;
	Any ->
	    io:format("find_game(~w): ~w~n", [GID, Any]),
	    none
    end.

%%%
%%% Test suite
%%%

test() ->
    ok.
