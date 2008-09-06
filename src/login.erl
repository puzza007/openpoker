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

-module(login).

-export([login/3, logout/1, test/0]).

-include("proto.hrl").
-include("test.hrl").
-include("schema.hrl").

login({atomic, []}, _) ->
    %% player not found
    {error, ?ERR_BAD_LOGIN};

login({atomic, [Player]}, [_Nick, Pass|_] = Args) 
  when is_record(Player, player) ->
    %% replace dead pids with none
    Player1 = Player#player {
		socket = fix_pid(Player#player.socket),
		pid = fix_pid(Player#player.pid)
	       },
    %% check player state and login
    Condition = check_player(Player1, [Pass], 
			     [
			      fun is_account_disabled/2,
			      fun is_bad_password/2,
			      fun is_player_busy/2,
			      fun is_player_online/2,
			      fun is_client_down/2,
			      fun is_offline/2
			     ]),
    {Player2, Result} = login(Player1, Condition, Args),
    F = fun() -> mnesia:write(Player2) end,
    case mnesia:transaction(F) of
	{atomic, ok} ->
	    Result;
	_ ->
	    {error, ?ERR_UNKNOWN}
    end.

login(Nick, Pass, Socket) 
  when is_list(Nick),
       is_list(Pass),
       is_pid(Socket) -> % socket handler process
    login(db:find(player, nick, Nick), [Nick, Pass, Socket]);

login(Player, bad_password, _) ->
    N = Player#player.login_errors + 1,
    {atomic, MaxLoginErrors} = 
	db:get(cluster_config, 0, max_login_errors),
    if
	N > MaxLoginErrors ->
	    %% disable account
	    Player1 = Player#player {
			disabled = true
		       },
	    {Player1, {error, ?ERR_ACCOUNT_DISABLED}};
	true ->
	    Player1 = Player#player {
			login_errors = N
		       },
	    {Player1, {error, ?ERR_BAD_LOGIN}}
    end;

login(Player, account_disabled, _) ->
    {Player, {error, ?ERR_ACCOUNT_DISABLED}};

login(Player, player_online, Args) ->
    %% player is idle
    logout(Player#player.oid),
    login(Player, player_offline, Args);

login(Player, client_down, [_, _, Socket]) ->
    %% tell player process to talk to the new socket
    gen_server:cast(Player#player.pid, {'SOCKET', Socket}),
    Player1 = Player#player {
		socket = Socket
	       },
    {Player1, {ok, Player#player.pid}};

login(Player, player_busy, Args) ->
    Temp = login(Player, client_down, Args),
    cardgame:cast(Player#player.game, 
		  {'RESEND UPDATES', Player#player.pid}),
    Temp;

login(Player, player_offline, [Nick, _, Socket]) ->
    %% start player process
    {ok, Pid} = player:start(Nick),
    OID = gen_server:call(Pid, 'ID'),
    gen_server:cast(Pid, {'SOCKET', Socket}),
    %% update player record
    Player1 = Player#player {
		oid = OID,
		pid = Pid,
		socket = Socket
	       },
    {Player1, {ok, Pid}}.

%%% 
%%% Check player state
%%%

check_player(Player, Args, [Guard|Rest]) ->
    case Guard(Player, Args) of
	{true, Condition} ->
	    Condition;
	_ ->
	    check_player(Player, Args, Rest)
    end;

check_player(_Player, _Args, []) ->
    %% fall through
    unknown_error.

is_bad_password(Player, [Pass]) ->
    Hash = erlang:phash2(Pass, 1 bsl 32),
    Match = Player#player.password == Hash,
    {not Match, bad_password}.

is_account_disabled(Player, _) ->
    {Player#player.disabled, account_disabled}.

is_player_busy(Player, _) ->
    {Online, _} = is_player_online(Player, []),
    Playing = Player#player.game /= none,
    {Online and Playing, player_busy}.

is_player_online(Player, _) ->
    SocketAlive = Player#player.socket /= none,
    PlayerAlive = Player#player.pid /= none,
    {SocketAlive and PlayerAlive, player_online}.

is_client_down(Player, _) ->
    SocketDown = Player#player.socket == none,
    PlayerAlive = Player#player.pid /= none,
    {SocketDown and PlayerAlive, client_down}.

is_offline(Player, _) ->
    SocketDown = Player#player.socket == none,
    PlayerDown = Player#player.pid == none,
    {SocketDown and PlayerDown, player_offline}.

fix_pid(Pid)
  when is_pid(Pid) ->
    case util:is_process_alive(Pid) of
	true ->
	    Pid;
	_ ->
	    none
    end;

fix_pid(Pid) ->
    Pid.

logout(OID) ->
    case db:find(player, OID) of
	{atomic, [Player]} ->
	    player:stop(Player#player.pid),
	    {atomic, ok} = db:set(player, OID, 
				  [{pid, none},
				   {socket, none}]);
	_ ->
	    oops
    end.

%%%
%%% Test suite
%%%

test() ->
    ok.

