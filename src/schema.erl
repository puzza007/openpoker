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

-module(schema).

-export([install/1, populate/0]).

-include("schema.hrl").
-include("common.hrl").
-include("proto.hrl").

install(Nodes) when is_list(Nodes) ->
    mnesia:stop(),
    mnesia:delete_schema(Nodes),
    catch(mnesia:create_schema(Nodes)),
    mnesia:start(),
    %% counter
    case mnesia:create_table(counter, 
			     [
			      {disc_copies, Nodes}, 
			      {type, set}, 
			      {attributes, record_info(fields, counter)}
			     ]) of
	{atomic, ok} ->
	    ok;
	Any ->
	    error_logger:error_report([{message, "Cannot install table"},
				       {table, counter},
				       {error, Any},
				       {nodes, Nodes}])
    end,
    %% player 
    case mnesia:create_table(player, 
			     [
			      {disc_copies, Nodes}, 
			      {index, [nick]}, 
			      {type, set}, 
			      {attributes, record_info(fields, player)}
			     ]) of
	{atomic, ok} ->
	    ok;
	Any1 ->
	    error_logger:error_report([{message, "Cannot install table"},
				       {table, player},
				       {error, Any1},
				       {nodes, Nodes}])
    end,
    %% online game
    case mnesia:create_table(game_xref, 
			     [
			      {disc_copies, Nodes}, 
			      {type, set}, 
			      {attributes, record_info(fields, game_xref)}
			     ]) of
	{atomic, ok} ->
	    ok;
	Any3 ->
	    error_logger:error_report([{message, "Cannot install table"},
				       {table, game_xref},
				       {error, Any3},
				       {nodes, Nodes}])
    end,
    %% game history
    case mnesia:create_table(game_history, 
			     [
			      {disc_copies, Nodes}, 
			      {type, set}, 
			      {attributes, record_info(fields, game_history)}
			     ]) of
	{atomic, ok} ->
	    ok;
	Any4 ->
	    error_logger:error_report([{message, "Cannot install table"},
				       {table, game_history},
				       {error, Any4},
				       {nodes, Nodes}])
    end,
    %% cluster configuration
    case mnesia:create_table(cluster_config, 
			     [
			      {disc_copies, Nodes}, 
			      {type, set}, 
			      {attributes, record_info(fields, cluster_config)}
			     ]) of
	{atomic, ok} ->
	    ok;
	Any5 ->
	    error_logger:error_report([{message, "Cannot install table"},
				       {table, cluster_config},
				       {error, Any5},
				       {nodes, Nodes}])
    end,
    Conf = #cluster_config {
      id = 0,
      mnesia_masters = Nodes
     },
    F = fun() ->
		mnesia:write(Conf)
	end,
    {atomic, ok} = mnesia:transaction(F),
    case mnesia:create_table(game_config, 
			     [
			      {disc_copies, Nodes}, 
			      {type, set}, 
			      {attributes, record_info(fields, game_config)}
			     ]) of
	{atomic, ok} ->
	    ok;
	Any6 ->
	    error_logger:error_report([{message, "Cannot install table"},
				       {table, game_config},
				       {error, Any6},
				       {nodes, Nodes}])
    end,
    populate(),
    ok.

populate() ->
    game:setup(?GT_IRC_TEXAS, 20, 
			 {?LT_FIXED_LIMIT, 10, 20}, 
			 ?START_DELAY, ?PLAYER_TIMEOUT,
			 10),
    game:setup(?GT_TEXAS_HOLDEM, 10, 
	       {?LT_FIXED_LIMIT, 10, 20}, 
	       ?START_DELAY, ?PLAYER_TIMEOUT,
	       100).
    
