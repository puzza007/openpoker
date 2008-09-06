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

-record(counter, {
	  type,
	  value
	 }).

-record(player, {
	  oid, % object id
	  nick,
	  password,
	  location,
	  balance = 0.0,
	  inplay = 0.0, % only used while in a game
	  login_errors = 0,
	  pid = none, % process id
	  socket = none, % socket process
	  game = none, % current game
	  disabled = false % player is disabled
	 }).

-record(game_xref, {
	  oid,
	  pid,
	  type,
	  limit
	 }).

-record(seat_history, {
	  nick,
	  hand,
	  state
	 }).

-record(game_history, {
	  key,
	  type,
	  seats,
	  limit,
	  board,
	  pot,
	  events
	 }).

%% app config

-record(game_config, {
	  id,
	  type,
	  seat_count,
	  limit,
	  start_delay,
	  player_timeout,
	  max
	 }).

-record(cluster_config, {
	  id,
	  gateways = [],
	  mnesia_masters = [],
	  logdir = "/tmp",
	  max_login_errors = 5,
	  %% players can start games
	  enable_dynamic_games = false
	 }).

