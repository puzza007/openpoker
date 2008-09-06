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

-module(game).
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, 
	 handle_info/2, terminate/2, code_change/3]).
-export([start/4, stop/1, test/0]).
-export([find/8, setup/6]).

-include_lib("stdlib/include/qlc.hrl").

-include("common.hrl").
-include("test.hrl").
-include("proto.hrl").
-include("schema.hrl").

-record(seat, {
	  %% player process
	  player, 
	  %% total bet amount
	  bet,
	  %% cards
	  hand,
	  %% player state
	  state,
	  %% sequence number. tracks the last 
	  %% game update packet accepted by this player.
	  %% meant to track the last packet sent 
	  %% over the network connection.
	  seqnum = 0
	 }).
	
-record(game, {
	  oid, 
	  %% game type
	  type,
	  %% cardgame state machine process
	  fsm, 
	  %% player to seat cross-reference
	  xref = gb_trees:empty(), 
	  %% seats tuple
	  seats,
	  %% fixed, pot, no limit, etc. process
	  limit, 
	  %% card deck process
	  deck, 
	  %% shared cards list
	  board = [], 
	  %% pot process
	  pot,
	  %% game observers
	  observers = [], 
	  %% time given to players 
	  %% to make a move
	  timeout = ?PLAYER_TIMEOUT,
	  %% amount to call
	  call = 0, 
	  %% number of raises so far
	  raise_count = 0,
	  %% current sequence number
	  seqnum = 0,
	  %% players required to start a game
	  required_player_count = 2,
	  %% event history
	  event_history = []
	 }).

new(OID, FSM, GameType, SeatCount, LimitType) ->
    {ok, Deck} = deck:start_link(),
    {ok, Pot} = pot:start_link(),
    {ok, Limit} = case LimitType of
		      {?LT_FIXED_LIMIT, Low, High} ->
			  fixed_limit:start_link(Low, High)
		  end,
    _ = #game {
      oid = OID,
      fsm = FSM,
      type = GameType,
      deck = Deck,
      pot = Pot,
      seats = create_seats(SeatCount),
      limit = Limit
     }.

start(FSM, GameType, SeatCount, LimitType) ->
    gen_server:start(game, [FSM, GameType, SeatCount, LimitType], []).

init([FSM, GameType, SeatCount, LimitType])
  when is_pid(FSM),
       is_number(GameType), 
       is_number(SeatCount),
       is_tuple(LimitType) ->
    process_flag(trap_exit, true),
    OID = counter:bump(game),
    Data = new(OID, FSM, GameType, SeatCount, LimitType),
    %% store game info
    Game = #game_xref {
      oid = OID,
      pid = FSM,
      type = GameType, 
      limit = LimitType
     },
    case mnesia:transaction(fun() ->
				    mnesia:write(Game)
			    end) of
	{atomic, ok} ->
	    {ok, Data};
	Any ->
	    {stop, Any}
    end.

stop(Game)
  when is_pid(Game) ->
    gen_server:cast(Game, stop).

terminate(_Reason, Game) ->
    dispose_seats(Game),
    %% limit can be any of fixed, pot or no limit.
    %% since we don't know the module we send
    %% the stop message directly to the process.
    gen_server:cast(Game#game.limit, stop),
    deck:stop(Game#game.deck),
    pot:stop(Game#game.pot),
    %% remove ourselves from the db
    db:delete(game_xref, Game#game.oid),
    ok.

%%% Reset is used each time the game is restarted

handle_cast('RESET', Game) ->
    gen_server:cast(Game#game.deck, 'RESET'),
    gen_server:cast(Game#game.pot, 'RESET'),
    Game1 = Game#game {
	      board = [],
	      call = 0,
	      raise_count = 0,
	      seqnum = 0,
	      event_history = []
	     },
    Game2 = reset_bets(Game1),
    Game3 = reset_state(Game2),
    {noreply, Game3};
    
%%% Player timeout 

handle_cast({'TIMEOUT', Timeout}, Game) ->
    Game1 = Game#game { 
	      timeout = Timeout
	     },
    {noreply, Game1};

%%% Number of players required to start the game
    
handle_cast({'REQUIRED', N}, Game) ->
    N1 = if
	     N < 2 ->
		 2;
	     true ->
		 N
	 end,
    Game1 = Game#game {
	      required_player_count = N1
	     },
    {noreply, Game1};

%%% Broadcast event to players and observers

handle_cast({'BROADCAST', Event}, Game) ->
    Game1 = broadcast(Game, Event),
    {noreply, Game1};

%%% Only used for testing to rig the deck 

handle_cast({'RIG', Deck}, Game) ->
    gen_server:cast(Game#game.deck, {'RIG', Deck}),
    {noreply, Game};
    
%%% Watch the game without joining

handle_cast({?PP_WATCH, Player}, Game) ->
    Game1 = Game#game { 
	      observers = [Player|Game#game.observers]
	     },
    {noreply, Game1};

handle_cast({?PP_UNWATCH, Player}, Game) ->
    Game1 = Game#game { 
	      observers = lists:delete(Player, Game#game.observers)
	     },
    {noreply, Game1};
    
%%% Need to be watching the game or playing
%%% to be able to send chat messages.
    
handle_cast({?PP_CHAT, Player, Message}, Game) ->
    XRef = Game#game.xref,
    OurPlayer = gb_trees:is_defined(Player, XRef) 
	or lists:member(Player, Game#game.observers),
    Game1 = if 
		OurPlayer ->
		    broadcast(Game, {?PP_NOTIFY_CHAT, Player, Message});
		true ->
		    Game
	    end,
    {noreply, Game1};

handle_cast({?PP_JOIN, Player, SeatNum, BuyIn}, Game) ->
    handle_cast({?PP_JOIN, Player, SeatNum, BuyIn, ?PS_PLAY}, Game);

%%% You need to have enough money to buy in 
%%% and the seat has to be empty.

handle_cast({?PP_JOIN, Player, SeatNum, BuyIn, State}, Game) ->
    Seats = Game#game.seats,
    XRef = Game#game.xref,
    Seat = element(SeatNum, Seats),
    OurPlayer = gb_trees:is_defined(Player, XRef),
    if
	%% seat is taken
	Seat#seat.state /= ?PS_EMPTY ->
	    {noreply, Game}; 
	%% already sitting at this table
	OurPlayer ->
	    {noreply, Game};
	true ->
	    %% try to move the buy-in amount 
	    %% from balance to inplay
	    ID = gen_server:call(Player, 'ID'),
	    case db:move_amt(player, ID, {balance, inplay, BuyIn}) of
		{atomic, ok} ->
		    %% update player
		    db:set(player, ID, {game, Game#game.fsm}),
		    %% notify player
		    gen_server:cast(Player, {'INPLAY+', BuyIn}),
		    %% take seat and broadcast the fact
		    Game1 = join_player(Game, Player, SeatNum, State),
		    Game2 = broadcast(Game1, {?PP_NOTIFY_JOIN, 
					      Player, 
					      SeatNum}),
		    {noreply, Game2};
		Any ->
		    %% not enough money
		    %% or another error
		    io:format("Move amt error: ~w for player ~w~n", 
			      [Any, ID]),
		    B1 = db:get(player, ID, balance),
		    I1 = db:get(player, ID, inplay),
		    io:format("Balance: ~w, Inplay: ~w, BuyIn: ~w~n",
			      [B1, I1, BuyIn]),
		    {noreply, Game}
	    end
    end;

handle_cast({?PP_LEAVE, Player}, Game) ->
    XRef = Game#game.xref,
    Seats = Game#game.seats,
    OurPlayer = gb_trees:is_defined(Player, XRef),
    if
	OurPlayer ->
	    SeatNum = gb_trees:get(Player, XRef),
	    Seat = element(SeatNum, Seats),
	    XRef1 = gb_trees:delete(Player, XRef),
	    Game1 = Game#game {
		      xref = XRef1,
		      seats = setelement(SeatNum,
					 Seats,
					 Seat#seat {
					   player = none,
					   state = ?PS_EMPTY
					  })
		     },
	    Game2 = broadcast(Game1, {?PP_NOTIFY_LEAVE, Player}),
	    {noreply, Game2};
	%% not playing here
	true ->
	    {noreply, Game}
    end;

handle_cast({'DRAW', Player, Card}, Game) ->
    SeatNum = gb_trees:get(Player, Game#game.xref),
    Seat = element(SeatNum, Game#game.seats),
    gen_server:cast(Seat#seat.hand, {'ADD CARD', Card}),
    GID = Game#game.oid,
    gen_server:cast(Player, {?PP_NOTIFY_DRAW, GID, Card, Game#game.seqnum}),
    Game1 = broadcast(Game, {?PP_NOTIFY_PRIVATE, Player}),
    {noreply, Game1};

handle_cast({'DRAW SHARED', Card}, Game) ->
    Game1 = Game#game {
	      board = [Card|Game#game.board]
	     },
    Game2 = broadcast(Game1, {?PP_NOTIFY_SHARED, Card}),
    {noreply, Game2};

handle_cast({'SET STATE', Player, State}, Game) ->
    SeatNum = gb_trees:get(Player, Game#game.xref),
    Seat = element(SeatNum, Game#game.seats),
    Game1 = Game#game {
	      seats = setelement(SeatNum,
				 Game#game.seats,
				 Seat#seat {
				   state = State
				  })
	     },
    {noreply, Game1};

%% Reset ?PS_BET to ?PS_PLAY

handle_cast({'RESET STATE', Source, Target}, Game) ->
    Game1 = reset_state(Game, Source, Target),
    {noreply, Game1};

%% Reset bets to 0

handle_cast('NEW STAGE', Game) ->
    gen_server:cast(Game#game.pot, 'NEW STAGE'),
    Game1 = reset_bets(Game),
    {noreply, Game1};

handle_cast({'ADD BET', Player, Amount}, Game) when Amount >= 0 ->
    SeatNum = gb_trees:get(Player, Game#game.xref),
    Seat = element(SeatNum, Game#game.seats),
    InPlay = gen_server:call(Player, 'INPLAY'),
    if
	Amount > InPlay ->
	    {noreply, Game};
	true ->
	    if
		Amount == InPlay ->
		    State = ?PS_ALL_IN,
		    AllIn = true,
		    Game1 = broadcast(Game, {?PP_PLAYER_STATE, 
					     Player, 
					     ?PS_ALL_IN});
		true ->
		    State = Seat#seat.state,
		    AllIn = false,
		    Game1 = Game
	    end,
	    gen_server:cast(Game1#game.pot, {'ADD BET', Player, Amount, AllIn}),
	    gen_server:cast(Player, {'INPLAY-', Amount}),
	    NewBet = Seat#seat.bet + Amount,
	    Game2 = Game1#game {
		      seats = setelement(SeatNum,
					 Game1#game.seats,
					 Seat#seat {
					   bet = NewBet,
					   state = State
					  })
		     },
	    {noreply, Game2}
    end;

handle_cast({'RESEND UPDATES', Player}, Game) ->
    resend_updates(Game, Player),
    {noreply, Game};

handle_cast(stop, Game) ->
    {stop, normal, Game};

handle_cast(Event, Game) ->
    error_logger:info_report([{module, ?MODULE}, 
			      {line, ?LINE},
			      {self, self()}, 
			      {message, Event}]),
    {noreply, Game}.

handle_call('ID', _From, Game) ->
    {reply, Game#game.oid, Game};

handle_call('FSM', _From, Game) ->
    {reply, Game#game.fsm, Game};

handle_call('DECK', _From, Game) ->
    {reply, Game#game.deck, Game};

handle_call('TIMEOUT', _From, Game) ->
    {reply, Game#game.timeout, Game};

handle_call('REQUIRED', _From, Game) ->
    {reply, Game#game.required_player_count, Game};

handle_call('JOINED', _From, Game) ->
    {reply, length(get_seats(Game, ?PS_ANY)), Game};

handle_call('WAITING', _From, Game) ->
    {reply, 0, Game};

handle_call('BLINDS', _From, Game) ->
    {reply, gen_server:call(Game#game.limit,'BLINDS'), Game};

handle_call({'RAISE SIZE', Player, Stage}, _From, Game) ->
    Reply = gen_server:call(Game#game.limit,
			    {'RAISE SIZE', self(), Player, Stage}),
    {reply, Reply, Game};

handle_call({'STATE', Player}, _From, Game) ->
    case gb_trees:lookup(Player, Game#game.xref) of
	none ->
	    {reply, none, Game};
	{value, SeatNum} ->
	    Seat = element(SeatNum, Game#game.seats),
	    {reply, Seat#seat.state, Game}
    end;

handle_call({'WHAT SEAT', Player}, _From, Game) ->
    case gb_trees:lookup(Player, Game#game.xref) of
	none ->
	    {reply, none, Game};
	{value, SeatNum} ->
	    {reply, SeatNum, Game}
    end;

handle_call({'SEAT TAKEN', SeatNum}, _From, Game) ->
    Seat = element(SeatNum, Game#game.seats),
    {reply, Seat#seat.state /= ?PS_EMPTY, Game};

handle_call({'BET TOTAL', Player}, _From, Game) ->
    SeatNum = gb_trees:get(Player, Game#game.xref),
    Seat = element(SeatNum, Game#game.seats),
    {reply, Seat#seat.bet, Game};

handle_call({'PLAYER AT', SeatNum}, _From, Game) ->
    Player = if 
		 SeatNum > size(Game#game.seats) ->
		     none;
		 true ->
		     Seat = element(SeatNum, Game#game.seats),
		     Seat#seat.player
	     end,
    {reply, Player, Game};

handle_call('IS EMPTY', _From, Game) ->
    Seats = get_seats(Game, ?PS_ANY),
    Empty = (Game#game.observers == []) and (Seats == []),
    {reply, Empty, Game};

handle_call('SEAT COUNT', _From, Game) ->
    {reply, size(Game#game.seats), Game};

handle_call('RANK HANDS', _From, Game) ->
    Seats = get_seats(Game, ?PS_SHOWDOWN),
    {reply, rank_hands(Game, Seats), Game};

handle_call('POTS', _From, Game) ->
    Pots = gen_server:call(Game#game.pot, 'SIDE POTS'),
    {reply, Pots, Game};

handle_call('POT TOTAL', _From, Game) ->
    Total = gen_server:call(Game#game.pot, 'TOTAL'),
    {reply, Total, Game};

handle_call({'SEATS', StartFrom, Mask}, _From, Game) ->
    {reply, get_seats(Game, StartFrom, Mask), Game};

handle_call({'SEATS', Mask}, _From, Game) ->
    {reply, get_seats(Game, Mask), Game};

handle_call('SEAT QUERY', _From, Game) ->
    {reply, seat_query(Game), Game};

handle_call(Event, From, Game) ->
    error_logger:info_report([{module, ?MODULE}, 
			      {line, ?LINE},
			      {id, Game#game.oid},
			      {self, self()}, 
			      {message, Event}, 
			      {from, From}]),
    {noreply, Game}.

handle_info({'EXIT', _Pid, _Reason}, Game) ->
    %% child exit?
    {noreply, Game};

handle_info(Info, Game) ->
    error_logger:info_report([{module, ?MODULE}, 
			      {line, ?LINE},
			      {id, Game#game.oid},
			      {self, self()}, 
			      {message, Info}]),
    {noreply, Game}.

code_change(_OldVsn, Game, _Extra) ->
    {ok, Game}.

%%
%% Utility
%% 

rank_hands(Game, Seats) ->
    F = fun(SeatNum) ->
		Seat = element(SeatNum, Game#game.seats),
		Seat#seat.hand
	end,
    Hands = lists:map(F, Seats),
    Cards = Game#game.board,
    F1 = fun(Card) ->
		 F2 = fun(Hand) ->
			      gen_server:cast(Hand, {'ADD CARD', Card})
		      end,
		 lists:foreach(F2, Hands)
	 end,
    lists:foreach(F1, Cards),
    lists:map(fun(Hand) ->
		      gen_server:call(Hand, 'RANK')
	      end, Hands).

%% Initialize seats

create_seats(SeatCount) ->
    Seats = erlang:make_tuple(SeatCount, none),
    create_seats(Seats, SeatCount).

create_seats(Seats, I) when I =:= 0 ->
    Seats;

create_seats(Seats, I) ->
    {ok, Hand} = hand:start_link(),
    Seat = #seat {
      player = none,
      bet = 0,
      hand = Hand,
      state = ?PS_EMPTY
     },
    NewSeats = setelement(I, Seats, Seat),
    create_seats(NewSeats, I - 1).

%% Cleanup seats

dispose_seats(Game) ->
    Seats = Game#game.seats,
    if 
	Seats =/= none ->
	    dispose_seats(Seats, size(Seats));
	true ->
	    bad
    end.

dispose_seats(_Seats, 0) ->
    ok;

dispose_seats(Seats, Count) ->
    Seat = element(Count, Seats),
    hand:stop(Seat#seat.hand),
    dispose_seats(Seats, Count - 1).

%% Reset state

reset_state(Game, From, To) ->
    reset_state(Game, From, To, size(Game#game.seats)).

reset_state(Game, _From, _To, 0) ->
    Game;

reset_state(Game, From, To, Count) ->
    Seat = element(Count, Game#game.seats),
    if
	(Seat#seat.state band From) > 0 ->
	    NewGame = Game#game {
			seats = setelement(Count,
					   Game#game.seats,
					   Seat#seat {
					     state = To
					    })
		       };
	true ->
	    NewGame = Game
    end,
    reset_state(NewGame, From, To, Count - 1).
	    
%% Reset bets

reset_bets(Game) ->
    reset_bets(Game, size(Game#game.seats)).

reset_bets(Game, 0) ->
    Game;

reset_bets(Game, Count) ->
    Seat = element(Count, Game#game.seats),
    NewGame = Game#game {
		seats = setelement(Count,
				   Game#game.seats,
				   Seat#seat {
				     bet = 0
				    })
	       },
    reset_bets(NewGame, Count - 1).
	    
%% Reset player state

reset_state(Game) ->
    reset_state(Game, size(Game#game.seats)).

reset_state(Game, 0) ->
    Game;

reset_state(Game, Count) ->
    Seat = element(Count, Game#game.seats),
    NewGame = if 
		  Seat#seat.state == ?PS_FOLD ->
		      Game#game {
			seats = setelement(Count,
					   Game#game.seats,
					   Seat#seat {
					     state = ?PS_PLAY
					    })
		       };
		  true ->
		      Game
	      end,
    reset_state(NewGame, Count - 1).
	    
%% Create a list of seats matching a certain state

get_seats(Game, From, Mask) ->
    Size = size(Game#game.seats),
    get_seats(Game#game.seats, Size, From, Size, Mask, []).

get_seats(Game, Mask) ->
    Size = size(Game#game.seats),
    get_seats(Game#game.seats, Size, Size, Size, Mask, []).

get_seats(_Seats, _Size, _At, 0, _Mask, Acc) ->
    lists:reverse(Acc);

get_seats(Seats, Size, At, Counter, Mask, Acc) ->
    SeatNum = (At rem Size) + 1,
    Seat = element(SeatNum, Seats),
    IsMember = (Seat#seat.state band Mask) > 0,
    List = if
	       IsMember ->
		   [SeatNum|Acc];
	       true ->
		   Acc
	   end,
    get_seats(Seats, Size, At + 1, Counter - 1, Mask, List).

%% Broadcast event

add_seqnum(Game, Event) 
  when is_tuple(Event) ->
    add_seqnum(Game, tuple_to_list(Event));

add_seqnum(Game, [?PP_NOTIFY_CHAT, Player, Msg]) ->
    [?PP_NOTIFY_CHAT, Game#game.oid, Player, Game#game.seqnum, Msg];

add_seqnum(Game, List) 
  when is_list(List) ->
    [Type|Rest] = List,
    [Type, Game#game.oid|Rest] ++ [Game#game.seqnum].

make_players(Game, Seats) ->
    make_players(Game, Seats, []).

make_players(_Game, [], Acc) ->
    lists:reverse(Acc);

make_players(Game, [SeatNum|Rest], Acc) ->
    Seat = element(SeatNum, Game#game.seats),
    Player = Seat#seat.player,
    make_players(Game, Rest, [Player|Acc]).

broadcast(Game, Event) 
  when is_record(Game, game) ->
    Event1 = add_seqnum(Game, Event),
    Event2 = list_to_tuple(Event1),
    %% notify players
    Seats = get_seats(Game, ?PS_ANY),
    Players = make_players(Game, Seats),
    broadcast(Game, Players, Event2), 
    broadcast(Game, Game#game.observers, Event2).

broadcast(Game, [Player|Rest], Event) ->
    gen_server:cast(Player, Event),
    broadcast(Game, Rest, Event);
    
broadcast(Game, [], Event) ->
    Game#game {
      seqnum = Game#game.seqnum + 1,
      event_history = [Event|Game#game.event_history]
     }.

resend_updates(Game, Player)
  when is_record(Game, game),
       is_pid(Player) ->
    resend_updates(Player, lists:reverse(Game#game.event_history));

resend_updates(Player, [Event|Rest]) ->
    gen_server:cast(Player, Event),
    resend_updates(Player, Rest);

resend_updates(_Player, []) ->
    ok.

%% Seat query

seat_query(Game) ->
    Size = size(Game#game.seats),
    seat_query(Game, Size, []).

seat_query(_Game, 0, Acc) ->
    Acc;

seat_query(Game, SeatNum, Acc) ->
    Seat = element(SeatNum, Game#game.seats),
    Player = Seat#seat.player,
    State = case Seat#seat.state of
		?PS_EMPTY ->
		    ?SS_EMPTY;
		?PS_RESERVED ->
		    ?SS_RESERVED;
		_ ->
		    ?SS_TAKEN
	    end,
    Acc1 = [{SeatNum, State, Player}|Acc],
    seat_query(Game, SeatNum - 1, Acc1).

join_player(Game, Player, SeatNum, State) ->
    Seats = Game#game.seats,
    Seat = element(SeatNum, Seats),
    XRef = Game#game.xref,
    XRef1 = gb_trees:insert(Player, SeatNum, XRef),
    %% assign hand owner
    gen_server:cast(Seat#seat.hand, {'RESET', Player}),
    %% remove from the list of observers
    Observers = lists:delete(Player, Game#game.observers),
    Game#game {
      xref = XRef1,
      seats = setelement(SeatNum,
			 Seats,
			 Seat#seat {
			   player = Player,
			   state = State
			  }),
      observers = Observers
     }.

query_op(Arg, Op, Value) 
  when is_number(Arg),
       is_number(Value) ->
    case Op of
	?OP_IGNORE ->
	    true;
	?OP_EQUAL ->
	    Arg == Value;
	?OP_LESS ->
	    Arg < Value;
	?OP_GREATER ->
	    Arg > Value;
	_ ->
	    false
    end.

find(GameType, LimitType,
     ExpOp, Expected, 
     JoinOp, Joined,
     WaitOp, Waiting) ->
    F = fun() -> find_1(GameType, LimitType) end,
    {atomic, L} = mnesia:transaction(F),
    F1 = fun(Packet) ->
		 {_, _, _, Expected1, Joined1, Waiting1, _}
		     = Packet,
		 query_op(Expected1, ExpOp, Expected) 
		     and query_op(Joined1, JoinOp, Joined) 
		     and query_op(Waiting1, WaitOp, Waiting)
	 end,
    {atomic, lists:filter(F1, L)}.
		 
find_1(GameType, LimitType) ->
    Q = qlc:q([{G#game_xref.oid,
		G#game_xref.pid,
		G#game_xref.type,
		G#game_xref.limit}
	       || G <- mnesia:table(game_xref),
		  G#game_xref.type == GameType,
		  element(1, G#game_xref.limit) == LimitType]),
    L = qlc:e(Q),
    lists:map(fun({GID, Pid, Type, Limit}) ->
		      Expected = cardgame:call(Pid, 'SEAT COUNT'),
		      Joined = cardgame:call(Pid, 'JOINED'),
		      Waiting = 0, % not implemented
		      {?PP_GAME_INFO, GID, Type, 
		       Expected, Joined, Waiting, Limit}
	      end, L).

setup(GameType, SeatCount, Limit, Delay, Timeout, Max) ->
    Game = #game_config {
      id = erlang:phash2(now(), 1 bsl 32),
      type = GameType,
      seat_count = SeatCount,
      limit = Limit,
      start_delay = Delay,
      player_timeout = Timeout,
      max = Max
     },
    F = fun() -> mnesia:write(Game) end,
    mnesia:transaction(F).
    
%% 
%%
%% Test suite
%%

test() ->
    ok.

    
    
