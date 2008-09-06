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

-define(error1(Expr, Expected, Actual),
	io:format("~s is ~w instead of ~w at ~w:~w~n",
		  [??Expr, Actual, Expected, ?MODULE, ?LINE])).

-define(error2(Message),
	io:format("~s at ~w:~w~n",
		  [Message, ?MODULE, ?LINE])).

-define(match(Expected, Expr),
        fun() ->
		Actual = (catch (Expr)),
		case Actual of
		    Expected ->
			{success, Actual};
		    _ ->
			?error1(Expr, Expected, Actual),
			erlang:error("match failed", Actual)
		end
	end()).

-define(differ(Expected, Expr),
        fun() ->
		Actual = (catch (Expr)),
		case Actual of
		    Expected ->
			?error1(Expr, Expected, Actual),
			erlang:error("differ failed", Actual);
		    _ ->
			{success, Actual}
		end
	end()).

-define(waitmsg(Message, Timeout),
	fun() ->
		receive
		    Message ->
			success;
		    Other ->
			{error, Other}
		after Timeout ->
			{error, timeout}
		end
	end()).

-define(waitexit(Pid, Timeout),
	fun() ->
		receive
		    {'CARDGAME EXIT', Pid, Data} ->
			{success, Data};
		    Other ->
			{error, Other}
		after Timeout ->
			{error, timeout}
		end
	end()).


-define(waittcp(Message, Timeout),
	fun() ->
		receive
		    {tcp, _, Bin} ->
			case proto:read(Bin) of
			    Message ->
				success;
			    Any -> 
				{error, Any}
			end;
		    Other ->
			{error, Other}
		after Timeout ->
			{error, timeout}
		end
	end()).

-define(tcpsend(Socket, Data),
	fun() ->
		XXX = proto:write(Data),
		case gen_tcp:send(Socket, XXX) of
		    ok ->
			ok;
		    {error, closed} ->
			ok;
		    {error,econnaborted} ->
			ok;
		    Any ->
			error_logger:error_report([
						   {message, "gen_tcp:send error"},
						   {module, ?MODULE}, 
						   {line, ?LINE},
						   {socket, Socket}, 
						   {port_info, erlang:port_info(Socket, connected)},
						   {data, Data},
						   {bin, XXX},
						   {error, Any}])
		end
	end()).


