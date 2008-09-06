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

-module(cardgame).
-behavior(gen_fsm).

%% behaviour modules must export this function

-export([behaviour_info/1]).

%% export the gen_fsm interface

-export([start/3, start/5, test_start/5,
	 send_event/2, sync_send_event/2, sync_send_event/3,
	 send_all_state_event/2, sync_send_all_state_event/2,
	 sync_send_all_state_event/3, reply/2, send_event_after/2,
	 start_timer/2, cancel_timer/1]).

%% export the gen_fsm state handler call backs

-export([restart/1, stop/1, dispatch/2, dispatch/3]).

%% export the gen_fsm common call backs

-export([init/1, handle_event/3, handle_sync_event/4,
	 handle_info/3, terminate/3, code_change/4]).

%% our stuff 

-export([call/2, cast/2, test/0]).

-include("proto.hrl").
-include("texas.hrl").
-include("common.hrl").

%% define what callbacks users must export

behaviour_info(callbacks) ->
        gen_fsm:behaviour_info(callbacks);

behaviour_info(Other) -> 
    gen_fsm:behaviour_info(Other).

%% State data

-record(data, {
	  game,
	  modules,
	  stack,
	  state,
	  statedata,
	  parent,
	  context,
	  original_context
	 }).

start(GameType, SeatCount, LimitType) ->
    start(GameType, SeatCount, LimitType, ?START_DELAY, ?PLAYER_TIMEOUT).

start(GameType, SeatCount, LimitType, Delay, Timeout) ->
    %% create game stack. context is used to propagate 
    %% game information from module to module, e.g. button
    %% and blinds position for texas hold'em
    case GameType of
	?GT_IRC_TEXAS ->
	    %% irc texas differs slightly in application of button 
	    %% rules as well as the number of raises allowed
	    Modules = [
		       %% start delay
		       {delayed_start, [Delay]}, 
		       %% irc blind rules
		       {blinds, [irc]},
		       %% deal 2 cards to each player
		       {deal_cards, [2, private]}, 
		       %% start after BB, 100 raises
		       {betting, [100, ?GS_PREFLOP, true]}, 
		       %% show 3 shared cards
		       {deal_cards, [3, shared]}, 
		       %% flop
		       {betting, [100, ?GS_FLOP]}, 
		       %% show 1 more shared card
		       {deal_cards, [1, shared]}, 
		       %% turn
		       {betting, [100, ?GS_TURN]}, 
		       %% show 1 more shared card
		       {deal_cards, [1, shared]}, 
		       %% river
		       {betting, [100, ?GS_RIVER]}, 
		       %% showdown
		       {showdown, []}
		      ],
	    Context = #texas{};
	?GT_TEXAS_HOLDEM ->
	    Modules = [
		       %% start delay
		       {delayed_start, [Delay]}, 
		       %% blind rules
		       {blinds, []},
		       %% deal 2 cards to each player
		       {deal_cards, [2, private]}, 
		       %% start after BB, 3 raises
		       {betting, [?MAX_RAISES, ?GS_PREFLOP, true]}, 
		       %% show 3 shared cards
		       {deal_cards, [3, shared]}, 
		       %% flop
		       {betting, [?MAX_RAISES, ?GS_FLOP]}, 
		       %% show 1 more shared card
		       {deal_cards, [1, shared]}, 
		       %% turn
		       {betting, [?MAX_RAISES, ?GS_TURN]}, 
		       %% show 1 more shared card
		       {deal_cards, [1, shared]}, 
		       %% river
		       {betting, [?MAX_RAISES, ?GS_RIVER]}, 
		       %% showdown
		       {showdown, []}
		      ],
	    Context = #texas{}
    end,
    %% start the cardgame finite state machine
    case gen_fsm:start(?MODULE, [self(), GameType, SeatCount, LimitType, 
				 Context, Modules], []) of
	{ok, Pid} = X ->
	    cardgame:cast(Pid, {'TIMEOUT', Timeout}),
	    X;
	Any ->
	    Any
    end.

test_start(GameType, SeatCount, Limit, Context, Modules) ->
    gen_fsm:start(?MODULE, [self(), GameType, SeatCount, Limit, 
			    Context, Modules], []).
%%
%% The gen_fsm API functions
%%

send_event(FsmRef, Event) ->
    gen_fsm:send_event(FsmRef, Event).

sync_send_event(FsmRef, Event) ->
    gen_fsm:sync_send_event(FsmRef, Event).

sync_send_event(FsmRef, Event, Timeout) ->
    gen_fsm:sync_send_event(FsmRef, Event, Timeout).

send_all_state_event(FsmRef, Event) ->
    gen_fsm:send_all_state_event(FsmRef, Event).

sync_send_all_state_event(FsmRef, Event) ->
    gen_fsm:sync_send_all_state_event(FsmRef, Event).

sync_send_all_state_event(FsmRef, Event, Timeout) ->
    gen_fsm:sync_send_all_state_event(FsmRef, Event, Timeout).

reply(Caller, Reply) ->
    gen_fsm:reply(Caller, Reply).

send_event_after(Time, Event) ->
    gen_fsm:send_event_after(Time, Event).

start_timer(Time, Msg) ->
    gen_fsm:start_timer(Time, Msg).

cancel_timer(Ref) ->
    gen_fsm:cancel_timer(Ref).

%%
%% The gen_fsm call backs
%%

init([Parent, GameType, SeatCount, LimitType, Context, Modules]) 
  when is_pid(Parent), 
       is_number(SeatCount), 
       is_tuple(LimitType),
       is_tuple(Context),
       is_list(Modules) ->
    process_flag(trap_exit, true),
    {Module, Args} = hd(Modules),
    {ok, Game} = game:start(self(), GameType, SeatCount, LimitType),
    Ctx = #data {
      parent = Parent,
      game = Game,
      modules = Modules,
      stack = Modules,
      context = Context,
      original_context = Context
     },
    case Module:init([Game|Args]) of
	{ok, State, Data} ->
	    Ctx1 = Ctx#data {
	      state = State,
	      statedata = Data
	     },
	    send_event_after(0, {'START', Context}),
	    {ok, dispatch, Ctx1};

	{ok, State, Data, Timeout} ->
	    Ctx1 = Ctx#data {
	      state = State,
	      statedata = Data
	     },
	    send_event_after(0, {'START', Context}),
	    {ok, dispatch, Ctx1, Timeout};

	{stop, Reason} ->
	    game:stop(Game),
	    {stop, Reason};

	ignore ->
	    ignore;

	Other ->
	    Other
    end.

dispatch('SHOWDOWN', Ctx) ->
    {next_stage, dispatch, Ctx};

dispatch(Event, Ctx) ->
    {Module, _} = hd(Ctx#data.stack),
    State = Ctx#data.state,
    case Module:State(Event, Ctx#data.statedata) of
	{next_state, NextState, NewData} ->
	    NewCtx = Ctx#data {
		       state = NextState,
		       statedata = NewData
		      },
	    {next_state, dispatch, NewCtx};

	{next_state, NextState, NewData, Timeout} ->
	    NewCtx = Ctx#data {
		       state = NextState,
		       statedata = NewData
		      },
	    {next_state, dispatch, NewCtx, Timeout};

	{stop, Reason, NewData} ->
	    stop(Ctx, Reason, NewData);

	Other ->
	    Other
    end.

dispatch(Event, From, Ctx) ->
    {Module, _} = hd(Ctx#data.stack),
    State = Ctx#data.state,
    case Module:State(Event, From, Ctx#data.statedata) of
	{reply, Reply, NextState, NewData} ->
	    NewCtx = Ctx#data {
		       state = NextState,
		       statedata = NewData
		      },
	    {reply, Reply, dispatch, NewCtx};

	{reply, Reply, NextState, NewData, Timeout} ->
	    NewCtx = Ctx#data {
		       state = NextState,
		       statedata = NewData
		      },
	    {reply, Reply, dispatch, NewCtx, Timeout};

	{next_state, NextState, NewData} ->
	    NewCtx = Ctx#data {
		       state = NextState,
		       statedata = NewData
		      },
	    {next_state, dispatch, NewCtx};

	{next_state, NextState, NewData, Timeout} ->
	    NewCtx = Ctx#data {
		       state = NextState,
		       statedata = NewData
		      },
	    {next_state, dispatch, NewCtx, Timeout};

	{stop, Reason, Reply, NewData} ->
	    NewCtx = Ctx#data {
		       statedata = NewData
		      },
	    {stop, Reason, Reply, NewCtx};

	{stop, Reason, NewData} ->
	    stop(Ctx, Reason, NewData);

	Other ->
	    Other
    end.

handle_event('RESTART', dispatch, Ctx) ->
    start_next_module(Ctx, Ctx#data.modules);

%% intercept rigging of the deck to reset our context.
%% this is needed so that the button in irc texas games
%% starts from seat #1.

handle_event({'CAST', Event = {'RIG', _}}, dispatch, Ctx) ->
    Ctx1 = Ctx#data {
	     context = Ctx#data.original_context
	    },
    gen_server:cast(Ctx1#data.game, Event),
    {next_state, dispatch, Ctx1};

handle_event({'CAST', Event}, dispatch, Ctx) ->
    gen_server:cast(Ctx#data.game, Event),
    {next_state, dispatch, Ctx};

handle_event(Event, dispatch, Ctx) ->
    {Module, _} = hd(Ctx#data.stack),
    State = Ctx#data.state,
    Data = Ctx#data.statedata,
    case Module:handle_event(Event, State, Data) of
	{next_state, NextState, NewData} ->
	    NewCtx = Ctx#data {
		       state = NextState,
		       statedata = NewData
		      },
	    {next_state, dispatch, NewCtx};

	{next_state, NextState, NewData, Timeout} ->
	    NewCtx = Ctx#data {
		       state = NextState,
		       statedata = NewData
		      },
	    {next_state, dispatch, NewCtx, Timeout};

	{stop, Reason, NewData} ->
	    stop(Ctx, Reason, NewData);

	Other ->
	    Other
    end.

handle_sync_event({'CALL', Event}, _From, dispatch, Ctx) ->
    Reply = gen_server:call(Ctx#data.game, Event),
    {reply, Reply, dispatch, Ctx};

handle_sync_event(Event, From, dispatch, Ctx) ->
    {Module, _} = hd(Ctx#data.stack),
    State = Ctx#data.state,
    case Module:handle_sync_event(Event, From, State, Ctx#data.statedata) of
	{reply, Reply, NextState, NewData} ->
	    NewCtx = Ctx#data {
		       state = NextState,
		       statedata = NewData
		      },
	    {reply, Reply, dispatch, NewCtx};
	
	{reply, Reply, NextState, NewData, Timeout} ->
	    NewCtx = Ctx#data {
		       state = NextState,
		       statedata = NewData
		      },
	    {reply, Reply, dispatch, NewCtx, Timeout};
	
	{next_state, NextState, NewData} ->
	    NewCtx = Ctx#data { 
		       state = NextState, 
		       statedata = NewData
		      },
	    {next_state, dispatch, NewCtx};
	
	{next_state, NextState, NewData, Timeout} ->
	    NewCtx = Ctx#data{ 
		       state = NextState, 
		       statedata = NewData
		      },
	    {next_state, dispatch, NewCtx, Timeout};
	
	{stop, Reason, Reply, NewData} ->
	    NewCtx = Ctx#data {
		       statedata = NewData
		      },
	    {stop, Reason, Reply, NewCtx};
	
	{stop, Reason, NewData} ->
	    stop(Ctx, Reason, NewData);

	Other ->
	    Other
    end.

handle_info(stop, dispatch, Ctx) ->
    stop(Ctx, {normal, exit}, none);

handle_info(Event, dispatch, Ctx) ->
    {Module, _} = hd(Ctx#data.stack),
    State = Ctx#data.state,
    case Module:handle_info(Event, State, Ctx#data.statedata) of
	{next_state, NextState, NewData} ->
	    NewCtx = Ctx#data { 
		       state = NextState, 
		       statedata = NewData},
	    {next_state, dispatch, NewCtx};

	{next_state, NextState, NewData, Timeout} ->
	    NewCtx = Ctx#data { 
		       state = NextState, 
		       statedata = NewData
		      },
	    {next_state, dispatch, NewCtx, Timeout};

	{stop, Reason, NewData} ->
	    stop(Ctx, Reason, NewData);

	Other ->
	    Other
    end.

terminate(Reason, dispatch, Ctx) ->
    GID = gen_server:call(Ctx#data.game, 'ID'),
    db:delete(game_xref, GID),
    game:stop(Ctx#data.game),
    if
 	Ctx#data.stack /= [] ->
 	    {Module, _} = hd(Ctx#data.stack),
 	    State = Ctx#data.state,
 	    Data = Ctx#data.statedata,
 	    Module:terminate(Reason, State, Data);
 	true ->
 	    ok
    end.

code_change(OldVersion, dispatch, Ctx, Extra) ->
    {Module, _} = hd(Ctx#data.stack),
    State = Ctx#data.state,
    Data = Ctx#data.statedata,
    case Module:code_change(OldVersion, State, Data, Extra) of
	{ok, NextState, NewData} ->
	    NewCtx = Ctx#data {
		       state = NextState, 
		       statedata = NewData
		      },
	    {ok, dispatch, NewCtx};

	Other ->
	    Other
    end.

%% stop card game

stop(Ctx, shutdown, Data) ->    
    {Module, _} = hd(Ctx#data.stack),
    State = Ctx#data.state,
    Module:terminate(shutdown, State, Data),
    stop(Ctx, normal, Data);

stop(Ctx, {normal, exit}, Data) ->    
    %% send to parent
    Ctx#data.parent ! {'CARDGAME EXIT', self(), exit},
    stop(Ctx, normal, Data);

%% terminate current module
%% and restart at the top

stop(Ctx, {normal, restart}, Data) ->    
    {Module, _} = hd(Ctx#data.stack),
    State = Ctx#data.state,
    Module:terminate({normal, restart}, State, Data),
    start_next_module(Ctx, Ctx#data.modules);

%% terminate current module
%% and restart at the top
%% carrying over the result

stop(Ctx, {normal, restart, Result}, Data) ->    
    {Module, _} = hd(Ctx#data.stack),
    State = Ctx#data.state,
    Module:terminate({normal, restart}, State, Data),
    Ctx1 = Ctx#data {
	     context = Result
	    },
    start_next_module(Ctx1, Ctx#data.modules);

%% terminate current module 
%% and start the next one

stop(Ctx, {normal, Result}, Data) ->    
    {Module, _} = hd(Ctx#data.stack),
    State = Ctx#data.state,
    Module:terminate({normal, Result}, State, Data),
    [_|Stack] = Ctx#data.stack,
    Ctx1 = Ctx#data {
	     context = Result
	    },
    start_next_module(Ctx1, Stack);

%% terminate current module 
%% and start the very last one

stop(Ctx, {endgame, Result}, Data) ->    
    {Module, _} = hd(Ctx#data.stack),
    State = Ctx#data.state,
    Module:terminate({normal, Result}, State, Data),
    Stack = [lists:last(Ctx#data.stack)],
    Ctx1 = Ctx#data {
	     context = Result
	    },
    start_next_module(Ctx1, Stack);

%% stop cardgame

stop(Ctx, Reason, Data) ->
    NewCtx = Ctx#data {
	       statedata = Data
	      },
    {stop, Reason, NewCtx}.

start_next_module(Ctx, []) ->
    %% module stack is empty,
    %% send result to parent
    Ctx#data.parent ! {'CARDGAME EXIT', self(), Ctx#data.context},
    {stop, normal, Ctx};

%% initialize next gen_fsm callback module

start_next_module(Ctx, Modules) ->
    {Module, Args} = hd(Modules),
    case Module:init([Ctx#data.game|Args]) of
	{ok, State, Data} ->
	    NewCtx = Ctx#data {
		       stack = Modules,
		       state = State,
		       statedata = Data
		      },
	    send_event_after(0, {'START', Ctx#data.context}),
	    {next_state, dispatch, NewCtx};

	{ok, State, Data, Timeout} ->
	    NewCtx = Ctx#data {
	      game = game:start(),
	      stack = Modules,
	      state = State,
	      statedata = Data
	     },
	    send_event_after(0, {'START', Ctx#data.context}),
	    {next_state, dispatch, NewCtx, Timeout};

	{stop, Reason} ->
	    {stop, Reason, Ctx};
	
	ignore ->
	    ignore;

	Other ->
	    Other
    end.

stop(CardGameRef) ->
    gen_fsm:send_all_state_event(CardGameRef, stop).

restart(CardGameRef) ->
    gen_fsm:sync_send_all_state_event(CardGameRef, 'RESTART').
    
call(CardGameRef, Event) ->
    gen_fsm:sync_send_all_state_event(CardGameRef, {'CALL', Event}).

cast(CardGameRef, Event) ->
    gen_fsm:send_all_state_event(CardGameRef, {'CAST', Event}).

test() ->
    ok.
