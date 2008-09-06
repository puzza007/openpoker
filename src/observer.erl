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

-module(observer).
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, 
	 handle_info/2, terminate/2, code_change/3]).

-export([start/1, stop/1]).

-include("test.hrl").
-include("common.hrl").
-include("proto.hrl").

-record(data, {
	  oid,
	  trace,
	  parent,
	  gid,
	  player,
	  socket,
	  winners,
	  seats
	 }).

new(Parent) ->
    #data {
     trace = false,
     parent = Parent,
     player = none,
     socket = none,
     winners = gb_trees:empty(),
     seats = gb_trees:empty()
    }.

start(Parent) ->
    gen_server:start(observer, [Parent], []).

init([Parent]) ->
    process_flag(trap_exit, true),
    {ok, new(Parent)}.

stop(Ref) ->
    gen_server:cast(Ref, stop).

terminate(_Reason, Data) ->
    case Data#data.socket of
	none ->
	    ignore;
	Socket ->
	    gen_tcp:close(Socket)
    end,
    ok.

handle_cast({'ID', OID}, Data) ->
    Data1 = Data#data {
	      oid = OID
	     },
    {noreply, Data1};

handle_cast({'TRACE', On}, Data) ->
    Data1 = Data#data {
	      trace = On
	     },
    {noreply, Data1};

handle_cast(stop, Data) ->
    {stop, normal, Data};

handle_cast(Event, Data) ->
    ok = ?tcpsend(Data#data.socket, Event),
    {noreply, Data}.

handle_call({'CONNECT', Host, Port}, _From, Data) ->
    {ok, Sock} = tcp_server:start_client(Host, Port, 1024),
    Data1 = Data#data {
	      socket = Sock
	     },
    {reply, ok, Data1};

handle_call('ID', _From, Data) ->
    {reply, Data#data.oid, Data};

handle_call(Event, From, Data) ->
    error_logger:info_report([{module, ?MODULE}, 
			      {line, ?LINE},
			      {self, self()}, 
			      {from, From},
			      {message, Event}]),
    {noreply, Data}.

handle_info({tcp_closed, _Socket}, Data) ->
    {stop, normal, Data};

handle_info({tcp, _Socket, <<?PP_PID, PID:32>>}, Data) ->
    Data1 = Data#data {
	      player = PID
	     },
    {noreply, Data1};

handle_info({tcp, _Socket, Bin}, Data) ->
    case proto:read(Bin) of
	none ->
	    {noreply, Data};
	Event ->
	    handle(Event, Data)
    end;
	    
handle_info({'EXIT', _Pid, _Reason}, Data) ->
    %% child exit?
    {noreply, Data};

handle_info(Info, Data) ->
    error_logger:info_report([{module, ?MODULE}, 
			      {line, ?LINE},
			      {self, self()}, 
			      {message, Info}]),
    {noreply, Data}.

code_change(_OldVsn, Data, _Extra) ->
    {ok, Data}.

handle({?PP_GAME_INFO, GID, ?GT_IRC_TEXAS, 
	Expected, Joined, Waiting,
	{?LT_FIXED_LIMIT, Low, High}}, Data) ->
    if 
	Data#data.trace ->
	    io:format("Game #~w, #players: ~w, joined: ~w, waiting: ~w; ",
		      [GID, Expected, Joined, Waiting]),
	    io:format("limit: low: ~w, high: ~w~n", [Low, High]);
	true ->
	    ok
    end,
    {noreply, Data};

handle({?PP_PLAYER_INFO, PID, InPlay, Nick, Location}, Data) ->
    if
	Data#data.trace ->
	    io:format("Player: #~w, in-play: ~w, nick: ~w, location: ~w~n",
		      [PID, InPlay, Nick, Location]),
	    Amount = gb_trees:get(PID, Data#data.winners),
	    T1 = gb_trees:delete(PID, Data#data.winners),
	    Nick1 = list_to_atom(Nick),
	    io:format("Observer: Nick: ~w, Amount: ~w~n", [Nick1, Amount]),
	    Data1 = Data#data {
		      winners = gb_trees:insert(Nick1, Amount, T1)
		     },
	    {noreply, Data1};
	true ->
	    {noreply, Data}
    end;

handle({?PP_NOTIFY_JOIN, GID, PID, SeatNum, _Seq}, Data) ->
    if
	Data#data.trace ->
	    io:format("~w: JOIN: ~w at seat#~w~n",
		      [GID, PID, SeatNum]);
	true ->
	    ok
    end,
    Data1 = Data#data {
	      seats = gb_trees:insert(PID, SeatNum, Data#data.seats)
	     },
    {noreply, Data1};

handle({?PP_NOTIFY_CHAT, GID, PID, _Seq, Message}, Data) ->
    if
	Data#data.trace ->
	    io:format("~w: CHAT: ~w: ~s~n",
		      [GID, PID, Message]);
	true ->
	    ok
    end,
    {noreply, Data};

handle({?PP_PLAYER_STATE, GID, PID, State, _Seq}, Data) ->
    if
	Data#data.trace ->
	    io:format("~w: STATE: ~w = ~w~n",
		      [GID, PID, State]);
	true ->
	    ok
    end,
    {noreply, Data};

handle({?PP_NOTIFY_LEAVE, GID, PID, _Seq}, Data) ->
    if
	Data#data.trace ->
	    io:format("~w: LEAVE: ~w~n",
		      [GID, PID]);
	true ->
	    ok
    end,
    {noreply, Data};

handle({?PP_NOTIFY_PRIVATE, GID, PID, _Seq}, Data) ->
    if
	Data#data.trace ->
	    io:format("~w: CARD: ~w~n",
		      [GID, PID]);
	true ->
	    ok
    end,
    {noreply, Data};

handle({?PP_GAME_STAGE, GID, Stage, _Seq}, Data) ->
    if
	Data#data.trace ->
	    io:format("~w: STAGE: ~w~n",
		      [GID, Stage]);
	true ->
	    ok
    end,
    {noreply, Data};

handle({?PP_NOTIFY_BET, GID, PID, Amount, _Seq}, Data) ->
    if
	Data#data.trace ->
	    io:format("~w: BET: ~w, ~-14.2. f~n",
		      [GID, PID, Amount]);
	true ->
	    ok
    end,
    {noreply, Data};

handle({?PP_NOTIFY_CALL, GID, PID, Amount, _Seq}, Data) ->
    if
	Data#data.trace ->
	    io:format("~w: CALL: ~w, ~-14.2. f~n",
		      [GID, PID, Amount]);
	true ->
	    ok
    end,
    {noreply, Data};

handle({?PP_NOTIFY_RAISE, GID, PID, Amount, _Seq}, Data) ->
    if
	Data#data.trace ->
	    io:format("~w: RAISE: ~w, ~-14.2. f~n",
		      [GID, PID, Amount]);
	true ->
	    ok
    end,
    {noreply, Data};

handle({?PP_NOTIFY_SB, GID, PID, Amount, _Seq}, Data) ->
    if
	Data#data.trace ->
	    io:format("~w: SB: ~w, ~-14.2. f~n",
		      [GID, PID, Amount]);
	true ->
	    ok
    end,
    {noreply, Data};

handle({?PP_NOTIFY_BB, GID, PID, Amount, _Seq}, Data) ->
    if
	Data#data.trace ->
	    io:format("~w: BB: ~w, ~-14.2. f~n",
		      [GID, PID, Amount]);
	true ->
	    ok
    end,
    {noreply, Data};

handle({?PP_NOTIFY_SHARED, GID, {Face, Suit}, _Seq}, Data) ->
    if
	Data#data.trace ->
	    io:format("~w: BOARD: {~w, ~w}~n",
		      [GID, Face, Suit]);
	true ->
	    ok
    end,
    {noreply, Data};

handle({?PP_NOTIFY_WIN, GID, PID, Amount, _Seq}, Data) ->
    if
	Data#data.trace ->
	    io:format("~w: WIN: ~w, ~-14.2. f~n",
		      [GID, PID, Amount]);
	true ->
	    ok
    end,
    SeatNum = gb_trees:get(PID, Data#data.seats),
    Data1 = Data#data {
	      winners = gb_trees:insert(SeatNum, 
					Amount, 
					Data#data.winners)
	     },
    {noreply, Data1};

handle({?PP_NOTIFY_START_GAME, GID, _Seq}, Data) ->
    if
	Data#data.trace ->
	    io:format("~w: START~n", [GID]);
	true ->
	    ok
    end,
    Data#data.parent ! {'START', GID},
    {noreply, Data};

handle({?PP_NOTIFY_BUTTON, GID, SeatNum, _Seq}, Data) ->
    if
	Data#data.trace ->
	    io:format("~w: DEALER: seat#~w~n", [GID, SeatNum]);
	true ->
	    ok
    end,
    {noreply, Data};

handle({?PP_NOTIFY_SB, GID, SeatNum, _Seq}, Data) ->
    if
	Data#data.trace ->
	    io:format("~w: SB: seat#~w~n", [GID, SeatNum]);
	true ->
	    ok
    end,
    {noreply, Data};

handle({?PP_NOTIFY_BB, GID, SeatNum, _Seq}, Data) ->
    if
	Data#data.trace ->
	    io:format("~w: BB: seat#~w~n", [GID, SeatNum]);
	true ->
	    ok
    end,
    {noreply, Data};

handle({?PP_NOTIFY_CANCEL_GAME, GID, _Seq}, Data) ->
    if
	Data#data.trace ->
	    io:format("~w: CANCEL~n", [GID]);
	true ->
	    ok
    end,
    {noreply, Data};

handle({?PP_NOTIFY_END_GAME, GID, _Seq}, Data) ->
    if
	Data#data.trace ->
	    io:format("~w: END~n", [GID]);
	true ->
	    ok
    end,
    Data#data.parent ! {'END', GID, Data#data.winners},
    ok = ?tcpsend(Data#data.socket, {?PP_UNWATCH, GID}),
    {stop, normal, Data};

handle({?PP_GOOD, _, _}, Data) ->
    {noreply, Data};

%% Sink

handle(Event, Data) ->
    error_logger:info_report([{module, ?MODULE}, 
			      {line, ?LINE},
			      {self, self()}, 
			      {event, Event}]),
    {noreply, Data}.

    


