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

-module(id).

-export([pid2key/1, pid2id/1, key2id/1]).

pid2key(Pid) when is_pid(Pid) ->
    {erlang:phash2(now(), 1 bsl 32),
     erlang:phash2(Pid, 1 bsl 32)}.

key2id(Key) 
  when is_tuple(Key), size(Key) == 2 ->
    erlang:phash2(Key, 1 bsl 32).

pid2id(Pid) when is_pid(Pid) ->
    key2id(pid2key(Pid)).

    
    


    
