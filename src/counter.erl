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

-module(counter).

-export([bump/1, bump/2, reset/1]).

-include("schema.hrl").

bump(Type) ->
    bump(Type, 1).

bump(Type, Inc) ->
    mnesia:dirty_update_counter(counter, Type, Inc).    

reset(Type) ->
    Counter = #counter {
      type = Type,
      value = 0
     },
    mnesia:transaction(fun() ->
			       mnesia:write(Counter)
		       end).
