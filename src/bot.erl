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

-module(bot).
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, 
	 handle_info/2, terminate/2, code_change/3]).

-export([start/4, stop/1]).

-include("test.hrl").
-include("common.hrl").
-include("proto.hrl").

-record(bot, {
	  nick,
	  player,
	  game,
	  socket,
	  actions,
	  balance,
	  seat_num,
	  irc_game_id,
	  done
	 }).

new(IRC_ID, Nick, SeatNum, Balance) ->
    Bot = #bot {
      nick = Nick,
      player = none,
      game = none,
      balance = Balance,
      socket = none,
      actions = [],
      seat_num = SeatNum,
      irc_game_id = IRC_ID,
      done = false
     },
    Bot.

start(Nick, IRC_ID, SeatNum, Balance) ->
    gen_server:start(bot, [Nick, IRC_ID, SeatNum, Balance], []).

init([Nick, IRC_ID, SeatNum, Balance]) ->
    process_flag(trap_exit, true),
    {ok, new(Nick, IRC_ID, SeatNum, Balance)}.

stop(Ref) ->
    gen_server:cast(Ref, stop).

terminate(_Reason, Bot) ->
    case Bot#bot.socket of
	none ->
	    ignore;
	Socket ->
	    if 
		not Bot#bot.done ->
		    error_logger:warning_report([{message, "Premature connection close"},
						 {module, ?MODULE},
						 {line, ?LINE},
						 {bot, Bot}]);
		true ->
		    ok
	    end,
	    gen_tcp:close(Socket)
    end,
    ok.

handle_cast({'SET ACTIONS', Actions}, Bot) ->
    Bot1 = Bot#bot {
	     actions = Actions
	    },
    {noreply, Bot1};
    
handle_cast(stop, Bot) ->
    {stop, normal, Bot};

handle_cast(Event, Bot) ->
    ok = ?tcpsend(Bot#bot.socket, Event),
    {noreply, Bot}.

handle_call({'CONNECT', Host, Port}, _From, Bot) ->
    {ok, Sock} = tcp_server:start_client(Host, Port, 1024),
    Bot1 = Bot#bot {
	     socket = Sock
	    },
    {reply, ok, Bot1};
    
handle_call('ACTIONS', _From, Bot) ->
    {reply, Bot#bot.actions, Bot};

handle_call('SOCKET', _From, Bot) ->
    {reply, Bot#bot.socket, Bot};

handle_call(Event, From, Bot) ->
    error_logger:info_report([{module, ?MODULE}, 
			      {line, ?LINE},
			      {self, self()}, 
			      {from, From},
			      {message, Event}]),
    {noreply, Bot}.

handle_info({tcp_closed, Socket}, Bot) ->
    if 
	not Bot#bot.done ->
	    error_logger:warning_report([{message, "Premature connection close"},
					 {module, ?MODULE},
					 {line, ?LINE},
					 {socket, Socket},
					 {bot, Bot}]);
	true ->
	    ok
    end,
    {stop, normal, Bot};

handle_info({tcp, _Socket, Bin}, Bot) ->
    case proto:read(Bin) of
	none ->
	    {noreply, Bot};
	Event ->
	    handle(Event, Bot)
    end;
	    
handle_info({'EXIT', _Pid, _Reason}, Bot) ->
    %% child exit?
    {noreply, Bot};

handle_info(Info, Bot) ->
    error_logger:info_report([{module, ?MODULE}, 
			      {line, ?LINE},
			      {self, self()}, 
			      {message, Info}]),
    {noreply, Bot}.

code_change(_OldVsn, Bot, _Extra) ->
    {ok, Bot}.

handle({?PP_PID, PID}, Bot) ->
    Bot1 = Bot#bot {
	     player = PID
	    },
    {noreply, Bot1};

handle({?PP_GAME_INFO, _GID, ?GT_IRC_TEXAS, 
	_Expected, _Joined, _Waiting,
	{?LT_FIXED_LIMIT, _Low, _High}}, Bot) ->
    {noreply, Bot};

handle({?PP_PLAYER_INFO, _PID, _InPlay, _Nick, _Location}, Bot) ->
    {noreply, Bot};

handle({?PP_NOTIFY_JOIN, GID, PID, _SeatNum, _Seq}, Bot) ->
    Bot1 = if
	       PID == Bot#bot.player ->
		   Bot#bot {
		     game = GID
		    };
	       true ->
		   Bot
	   end,
    {noreply, Bot1};

handle({?PP_NOTIFY_CHAT, _GID, _PID, _Seq, _Message}, Bot) ->
    {noreply, Bot};

handle({?PP_BET_REQ, GID, Amount}, Bot) ->
    GID = Bot#bot.game,
    %%io:format("~w: BLIND_REQ: ~w/~w, ~.2. f~n", 
    %%	      [GID, Bot#bot.player, Bot#bot.seat_num, Amount]),
    [Action|Rest] = Bot#bot.actions,
    Bot1 = Bot#bot {
	     actions = Rest
	    },
    case Action of
	'SIT OUT' ->
	    handle_cast({?PP_SIT_OUT, Bot1#bot.game}, Bot1),
	    {noreply, Bot1};
	'BLIND' ->
	    handle_cast({?PP_CALL, Bot1#bot.game, Amount}, Bot1),
	    Bot2 = Bot1#bot {
		     balance = Bot1#bot.balance - Amount
		    },
	    {noreply, Bot2};
	{'BLIND', allin} ->
	    handle_cast({?PP_CALL, Bot1#bot.game, Bot1#bot.balance}, Bot1),
	    Bot2 = Bot1#bot {
		     balance = 0
		    },
	    {noreply, Bot2};
	'FOLD' ->
	    handle_cast({?PP_FOLD, Bot1#bot.game}, Bot1),
	    {noreply, Bot1};
	_ ->
	    error_logger:error_report([{message, "Unexpected blind request, folding!"},
				       {module, ?MODULE}, 
				       {line, ?LINE},
				       {bot, Bot1},
				       {amount, Amount},
				       {now, now()}]),
	    handle_cast({?PP_FOLD, Bot1#bot.game}, Bot1),
	    {noreply, Bot1}
    end;

handle({?PP_BET_REQ, GID, Call, RaiseMin, RaiseMax}, Bot) ->
    GID = Bot#bot.game,
    [Action|Rest] = Bot#bot.actions,
    Bot1 = Bot#bot {
	     actions = Rest
	    },
    %%io:format("#~w/~w: BET_REQ ~.2. f/~.2. f/~.2. f~n", 
    %%	      [Bot#bot.player, Bot#bot.seat_num, Call, RaiseMin, RaiseMax]),
    %%io:format("#~w/~w: Actions: ~w~n", 
    %%	      [Bot#bot.player, Bot#bot.seat_num, Bot#bot.actions]),
    %%io:format("#~w/~w: Balance: ~.2. f~n", 
    %%	      [Bot#bot.player, Bot#bot.seat_num, Bot#bot.balance * 1.0]),
    case Action of
	'SIT OUT' ->
	    handle_cast({?PP_SIT_OUT, Bot1#bot.game}, Bot1),
	    {noreply, Bot1};
	'BLIND' ->
	    handle_cast({?PP_CALL, Bot1#bot.game, Call}, Bot1),
	    Bot2 = Bot1#bot {
		     balance = Bot1#bot.balance - Call
		    },
	    {noreply, Bot2};
	{'BLIND', allin} ->
	    handle_cast({?PP_CALL, Bot1#bot.game, Bot1#bot.balance}, Bot1),
	    Bot2 = Bot1#bot {
		     balance = 0
		    },
	    {noreply, Bot2};
	'CHECK' ->
	    handle_cast({?PP_CALL, Bot1#bot.game, 0}, Bot1),
	    {noreply, Bot1};
	'CALL' ->
	    handle_cast({?PP_CALL, Bot1#bot.game, Call}, Bot1),
	    Bot2 = Bot1#bot {
		     balance = Bot1#bot.balance - Call
		    },
	    {noreply, Bot2};
	{'CALL', allin} ->
	    handle_cast({?PP_CALL, Bot1#bot.game, Bot1#bot.balance}, Bot1),
	    Bot2 = Bot1#bot {
		     balance = 0
		    },
	    {noreply, Bot2};
	'RAISE' ->
	    handle_cast({?PP_RAISE, Bot1#bot.game, RaiseMin}, Bot1),
	    Bot2 = Bot1#bot {
		     balance = Bot1#bot.balance - Call - RaiseMin
		    },
	    {noreply, Bot2};
	{'RAISE', allin} ->
	    handle_cast({?PP_RAISE, Bot1#bot.game, Bot1#bot.balance - Call}, Bot1),
	    Bot2 = Bot1#bot {
		     balance = 0
		    },
	    {noreply, Bot2};
	'BET' ->
	    handle_cast({?PP_RAISE, Bot1#bot.game, RaiseMin}, Bot1),
	    Bot2 = Bot1#bot {
		     balance = Bot1#bot.balance - RaiseMin
		    },
	    {noreply, Bot2};
	{'BET', allin} ->
	    handle_cast({?PP_RAISE, Bot1#bot.game, Bot1#bot.balance - Call}, Bot1),
	    Bot2 = Bot1#bot {
		     balance = 0
		    },
	    {noreply, Bot2};
	'FOLD' ->
	    handle_cast({?PP_FOLD, Bot1#bot.game}, Bot1),
	    {noreply, Bot1};
	'QUIT' ->
	    handle_cast({?PP_FOLD, Bot1#bot.game}, Bot1),
	    {noreply, Bot1};
	_ ->
	    error_logger:error_report([{message, "Unexpected bet request, folding!"},
				       {module, ?MODULE}, 
				       {line, ?LINE},
				       {bot, Bot1},
				       {call, Call},
				       {raise_min, RaiseMin},
				       {raise_max, RaiseMax},
				       {now, now()}]),
	    handle_cast({?PP_FOLD, Bot1#bot.game}, Bot1),
	    {noreply, Bot1}
    end;

handle({?PP_PLAYER_STATE, _GID, _PID, _State, _Seq}, Bot) ->
    {noreply, Bot};

handle({?PP_NOTIFY_LEAVE, _GID, PID, _Seq}, Bot) ->
    if
	PID == Bot#bot.player ->
	    ok = ?tcpsend(Bot#bot.socket, ?PP_LOGOUT),
	    {stop, leave, Bot};
	true ->
	    {noreply, Bot}
    end;

handle({?PP_GAME_STAGE, _GID, _Stage, _Seq}, Bot) ->
    {noreply, Bot};

handle({?PP_NOTIFY_START_GAME, _GID, _Seq}, Bot) ->
    {noreply, Bot};

handle({?PP_NOTIFY_END_GAME, GID, _Seq}, Bot) ->
    %%io:format("Bot ~w leaving ~w at ~w~n",
    %%		      [Bot#bot.player, GID, now()]),
    ok = ?tcpsend(Bot#bot.socket, {?PP_LEAVE, GID}),
    ok = ?tcpsend(Bot#bot.socket, ?PP_LOGOUT),
    Bot1 = Bot#bot {
	     done = true
	    },
    {stop, normal, Bot1};

handle({?PP_NOTIFY_CANCEL_GAME, GID, _Seq}, Bot) ->
    ok = ?tcpsend(Bot#bot.socket, {?PP_JOIN, GID, 
				   Bot#bot.seat_num, 
				   Bot#bot.balance}),
    {noreply, Bot};

handle({Cmd, _GID, _PID, _Amount, _Seq}, Bot)
  when Cmd == ?PP_NOTIFY_WIN;
       Cmd == ?PP_NOTIFY_CALL;
       Cmd == ?PP_NOTIFY_RAISE;
       Cmd == ?PP_NOTIFY_BET ->
    {noreply, Bot};

handle({?PP_NOTIFY_DRAW, _GID, _Card, _Seq}, Bot) ->
    {noreply, Bot};

handle({?PP_NOTIFY_PRIVATE, _GID, _PID, _Seq}, Bot) ->
    {noreply, Bot};

handle({?PP_NOTIFY_BUTTON, _GID, _SeatNum, _Seq}, Bot) ->
    {noreply, Bot};

handle({?PP_NOTIFY_SB, _GID, _SeatNum, _Seq}, Bot) ->
    {noreply, Bot};

handle({?PP_NOTIFY_BB, _GID, _SeatNum, _Seq}, Bot) ->
    {noreply, Bot};

handle({?PP_GOOD, _, _}, Bot) ->
    {noreply, Bot};

handle({Cmd, _GID, {_Face, _Suit}, _Seq}, Bot) 
  when Cmd == ?PP_NOTIFY_DRAW;
       Cmd == ?PP_NOTIFY_SHARED ->
    {noreply, Bot};

%% Sink

handle(Event, Bot) ->
    error_logger:info_report([{module, ?MODULE}, 
			      {line, ?LINE},
			      {self, self()}, 
			      {event, Event}]),
    {noreply, Bot}.

    


