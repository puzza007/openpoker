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

-module(fixed_limit).
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, 
	 handle_info/2, terminate/2, code_change/3]).

-export([start/2, start_link/2, stop/1, test/0]).

-include("test.hrl").
-include("common.hrl").
-include("proto.hrl").

-record(fixed_limit, {
	  high,
	  low
	 }).

new(Low, High) ->
    #fixed_limit {
     high = High,
     low = Low
    }.

start(Low, High) ->
    gen_server:start(fixed_limit, [Low, High], []).

start_link(Low, High) ->
    gen_server:start_link(fixed_limit, [Low, High], []).

init([Low, High]) ->
    process_flag(trap_exit, true),
    {ok, new(Low, High)}.

stop(LimitRef) ->
    gen_server:cast(LimitRef, stop).

terminate(_Reason, _Limit) ->
    ok.

handle_cast(stop, Limit) ->
    {stop, normal, Limit};

handle_cast(Event, Limit) ->
    error_logger:info_report([{module, ?MODULE}, 
			      {line, ?LINE},
			      {self, self()}, 
			      {message, Event}]),
    {noreply, Limit}.

handle_call('INFO', _From, Limit) ->
    {reply, {?LT_FIXED_LIMIT, 
	     Limit#fixed_limit.low,
	     Limit#fixed_limit.high}, Limit};

handle_call({'RAISE SIZE', _Game, _Player, Stage}, _From, Limit) ->
    {reply, raise_size(Limit, Stage), Limit};

handle_call('BLINDS', _From, Limit) ->
    {reply, {Limit#fixed_limit.low div 2, Limit#fixed_limit.low}, Limit};

handle_call(Event, From, Limit) ->
    error_logger:info_report([{module, ?MODULE}, 
			      {line, ?LINE},
			      {self, self()}, 
			      {message, Event}, 
			      {from, From}]),
    {noreply, Limit}.

handle_info({'EXIT', _Pid, _Reason}, Limit) ->
    %% child exit?
    {noreply, Limit};

handle_info(Info, Limit) ->
    error_logger:info_report([{module, ?MODULE}, 
			      {line, ?LINE},
			      {self, self()}, 
			      {message, Info}]),
    {noreply, Limit}.

code_change(_OldVsn, Limit, _Extra) ->
    {ok, Limit}.

raise_size(Limit, Stage) when ?GS_PREFLOP =:= Stage; 
			      ?GS_FLOP =:= Stage ->
    {Limit#fixed_limit.low, Limit#fixed_limit.low};

raise_size(Limit, _Stage) ->
    {Limit#fixed_limit.high, Limit#fixed_limit.high}.

test() ->
    ok.
