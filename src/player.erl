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

-module(player).
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, 
	 handle_info/2, terminate/2, code_change/3]).
-export([start/1, stop/1, cast/2, call/2, test/0]).
-export([create/4]).

-include("test.hrl").
-include("common.hrl").
-include("proto.hrl").
-include("schema.hrl").

-record(data, {
	  oid,
	  socket = none,
	  inplay = 0
	 }).

new(OID) ->
    #data {
     oid = OID
    }.

start(Nick) 
  when is_list(Nick) ->
    gen_server:start(player, [Nick], []).

init([Nick]) 
  when is_list(Nick) ->
    process_flag(trap_exit, true),
    %% make sure we exist
    case db:find(player, nick, Nick) of
	{atomic, [Player]} ->
	    %% update process id
	    OID = Player#player.oid,
	    case db:set(player, OID, {pid, self()}) of
		{atomic, ok} ->
		    %% all good
		    {ok, new(OID)};
		Any ->
		    {stop, Any}
	    end;
	Any ->
	    {stop, Any}
    end.

stop(Player) 
  when is_pid(Player) ->
    gen_server:cast(Player, stop).

terminate(_Reason, Data) ->
    db:set(player, Data#data.oid, {pid, none}),
    ok.

handle_cast('LOGOUT', Data) ->
    spawn(fun() -> 
		  login:logout(Data#data.oid) 
	  end),
    {noreply, Data};

handle_cast('DISCONNECT', Data) ->
    %% ignore
    {noreply, Data};

handle_cast({'SOCKET', Socket}, Data) 
  when is_pid(Socket) ->
    Data1 = Data#data {
	      socket = Socket
	     },
    {noreply, Data1};
    
handle_cast({'INPLAY-', Amount}, Data)
  when is_number(Amount), Amount >= 0 ->
    %%db:dec(player, Data#data.oid, {inplay, Amount}),
    Data1 = Data#data {
	      inplay = Data#data.inplay - Amount
	     },
    {noreply, Data1};
    
handle_cast({'INPLAY+', Amount}, Data) 
  when is_number(Amount), Amount > 0 ->
    %%db:dec(player, Data#data.oid, {inplay, Amount}),
    Data1 = Data#data {
	      inplay = Data#data.inplay + Amount
	     },
    {noreply, Data1};
    
handle_cast({?PP_WATCH, Game}, Data) 
  when is_pid(Game) ->
    cardgame:cast(Game, {?PP_WATCH, self()}),
    {noreply, Data};

handle_cast({?PP_UNWATCH, Game}, Data) 
  when is_pid(Game) ->
    cardgame:cast(Game, {?PP_UNWATCH, self()}),
    {noreply, Data};

handle_cast({Event, Game, Amount}, Data)
  when Event == ?PP_CALL;
       Event == ?PP_RAISE ->
    cardgame:send_event(Game, {Event, self(), Amount}),
    {noreply, Data};

handle_cast({?PP_JOIN, Game, SeatNum, BuyIn}, Data) ->
    cardgame:send_event(Game, {?PP_JOIN, self(), SeatNum, BuyIn}),
    {noreply, Data};

handle_cast({?PP_LEAVE, Game}, Data) ->
    cardgame:send_event(Game, {?PP_LEAVE, self()}),
    %% move inplay amount back to balance
    {atomic, ok} = db:set(player, 
			  Data#data.oid, 
			  {inplay, Data#data.inplay}),
    {atomic, ok} = db:move_amt(player, 
			       Data#data.oid, 
			       {inplay, balance, Data#data.inplay}),
    Data1 = Data#data {
	      inplay = 0
	     },
    {noreply, Data1};

handle_cast({Event, Game}, Data) 
  when Event == ?PP_FOLD;
       Event == ?PP_SIT_OUT;
       Event == ?PP_COME_BACK ->
    cardgame:send_event(Game, {Event, self()}),
    {noreply, Data};

handle_cast({?PP_CHAT, Game, Message}, Data) ->
    cardgame:cast(Game, {?PP_CHAT, self(), Message}),
    {noreply, Data};

handle_cast({?PP_SEAT_QUERY, Game}, Data) ->
    GID = cardgame:call(Game, 'ID'),
    L = cardgame:call(Game, 'SEAT QUERY'),
    F = fun({SeatNum, State, Player}) -> 
		PID = if 
			  Player == self() ->
			      Data#data.oid;
			  State /= ?SS_EMPTY ->
			      gen_server:call(Player, 'ID');
			  true ->
			      0
		      end,
		handle_cast({?PP_SEAT_STATE, GID, SeatNum, State, PID}, Data) 
	end,
    lists:foreach(F, L),
    {noreply, Data};

handle_cast({?PP_PLAYER_INFO_REQ, PID}, Data) ->
    case db:find(player, PID) of
	{atomic, [Player]} ->
	    handle_cast({?PP_PLAYER_INFO, 
			 Player#player.pid, 
			 Player#player.inplay,
			 Player#player.nick,
			 Player#player.location}, Data);
	_ ->
	    oops
    end,
    {noreply, Data};

handle_cast({?PP_NEW_GAME_REQ, GameType, Expected, Limit}, Data) ->
    {atomic, DynamicGames} = db:get(cluster_config, 0, enable_dynamic_games),
    if
	DynamicGames ->
	    case cardgame:start(GameType, Expected, Limit) of
		{ok, Pid} ->
		    GID = cardgame:call(Pid, 'ID'),
		    handle_cast({?PP_GOOD, ?PP_NEW_GAME_REQ, GID}, Data);
		_ ->
		    handle_cast({?PP_BAD, ?PP_NEW_GAME_REQ, ?ERR_UNKNOWN}, Data)
	    end;
	true ->
	    handle_cast({?PP_BAD, ?PP_NEW_GAME_REQ, ?ERR_START_DISABLED}, Data)
    end,
    {noreply, Data};

handle_cast(?PP_BALANCE_REQ, Data) ->
    case db:find(player, Data#data.oid) of
	{atomic, [Player]} ->
	    handle_cast({?PP_BALANCE_INFO, 
			 Player#player.balance,
			 Player#player.inplay}, Data);
	_ ->
	    oops
    end,
    {noreply, Data};

handle_cast(stop, Data) ->
    {stop, normal, Data};

handle_cast(Event, Data) ->
    if 
	Data#data.socket /= none ->
	    Data#data.socket ! {packet, Event};
	true ->
	    ok
    end,
    {noreply, Data}.

handle_call('ID', _From, Data) ->
    {reply, Data#data.oid, Data};

handle_call('INPLAY', _From, Data) ->
    {reply, Data#data.inplay, Data};

handle_call(Event, From, Data) ->
    error_logger:info_report([{module, ?MODULE}, 
			      {line, ?LINE},
			      {self, self()}, 
			      {message, Event}, 
			      {from, From}]),
    {noreply, Data}.

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

%%%
%%% Utility
%%%

cast(PID, Event) ->
    case db:find(player, PID) of
	{atomic, [Player]} ->
	    gen_server:cast(Player#player.pid, Event);
	_ ->
	    none
    end.

call(PID, Event) ->
    case db:find(player, PID) of
	{atomic, [Player]} ->
	    gen_server:call(Player#player.pid, Event);
	_ ->
	    none
    end.

create(Nick, Pass, Location, Balance)
  when is_list(Nick),
       is_list(Pass),
       is_list(Location),
       is_number(Balance) ->
    OID = counter:bump(player),
    Player = #player {
      oid = OID,
      nick = Nick,
      %% store a hash of the password
      %% instead of the password itself
      password = erlang:phash2(Pass, 1 bsl 32),
      location = Location,
      balance = Balance,
      inplay = 0.00
     },
    mnesia:transaction(fun() -> 
			       mnesia:write(Player),
			       OID
		       end).

%%%
%%% Test suite
%%%

test() ->
    ok.


    
    
