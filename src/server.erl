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

-module(server).
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, 
	 handle_info/2, terminate/2, code_change/3]).

-export([start/1, start/2, start/3, stop/1, test/0]).

-include("common.hrl").
-include("proto.hrl").
-include("texas.hrl").
-include("schema.hrl").
-include("test.hrl").

-record(server, {
	  port,
	  host,
	  avg,
	  games,
	  test_mode
	 }).

-record(client, {
	  server = none,
	  player = none
	 }).

start([Port, Host]) 
  when is_atom(Port),
       is_atom(Host) ->
    Port1 = list_to_integer(atom_to_list(Port)),
    Host1 = atom_to_list(Host),
    start(Host1, Port1).

start(Host, Port) ->
    start(Host, Port, false).

start(Host, Port, TestMode) ->
    mnesia:start(),
    case mnesia:wait_for_tables([game_config, game_xref], 10000) of 
	ok ->
	    case gen_server:start(server, [Host, Port, TestMode], []) of
		{ok, Pid} ->
		    %%io:format("server:start: pid ~w~n", [Pid]),
		    pg2:create(?GAME_SERVERS),
		    ok = pg2:join(?GAME_SERVERS, Pid),
		    {ok, Pid};
		Result ->
		    error_logger:error_report(
		      [{module, ?MODULE},
		       {line, ?LINE},
		       {message, "Unexpected result"},
		       {call, 'gen_server:start(server)'}, 
		       {result, Result},
		       {port, Port},
		       {now, now()}]),
		    Result
	    end;
	Other ->
	    error_logger:error_report(
	      [{module, ?MODULE},
	       {line, ?LINE},
	       {message, "Unexpected result"},
	{result, Other},
	       {call, 'mnesia:wait_for_tables'}, 
	       {now, now()}]),
	    Other
    end.

init([Host, Port, TestMode]) ->
    process_flag(trap_exit, true), 
    %%error_logger:logfile({open, "/tmp/" 
    %%		  ++ atom_to_list(node()) 
    %%		  ++ ".log"}),
    Client = #client {
      server = self()
     },
    F = fun(Sock) -> parse_packet(Sock, Client) end, 
    tcp_server:stop(Port),
    {ok, _} = tcp_server:start_raw_server(Port, F, 10240, 2048),
    Server = #server {
      host = Host,
      port = Port,
      avg = 0,
      games = start_games(),
      test_mode = TestMode
     },
    {ok, Server}.

stop(Server) ->
    gen_server:cast(Server, stop).

terminate(normal, Server) ->
    kill_games(Server#server.games),
    tcp_server:stop(Server#server.port),
    ok.

handle_cast(stop, Server) ->
    {stop, normal, Server};

handle_cast(Event, Server) ->
    error_logger:info_report([{module, ?MODULE}, 
			      {line, ?LINE},
			      {self, self()}, 
			      {message, Event}]),
    {noreply, Server}.


handle_call('WHERE', _From, Server) ->
    {reply, {Server#server.host, Server#server.port}, Server};
%%     {ok, [{X, _, _}|_]} = inet:getif(),
%%     io:format("Server address: ~w~n", [X]),
%%     Host = io_lib:format("~.B.~.B.~.B.~.B", 
%% 			 [element(1, X),
%% 			  element(2, X),
%% 			  element(3, X),
%% 			  element(4, X)]),
%%     {reply, {Host, Server#server.port}, Server};

handle_call('USER COUNT', _From, Server) ->
    Children = tcp_server:children(Server#server.port),
    {reply, length(Children), Server};

handle_call('TEST MODE', _From, Server) ->
    {reply, Server#server.test_mode, Server};

handle_call(Event, From, Server) ->
    error_logger:info_report([{module, ?MODULE}, 
			      {line, ?LINE},
			      {self, self()}, 
			      {message, Event}, 
			      {from, From}]),
    {noreply, Server}.

handle_info({'EXIT', _Pid, _Reason}, Server) ->
    %% child exit?
    {noreply, Server};

handle_info(Info, Server) ->
    error_logger:info_report([{module, ?MODULE}, 
			      {line, ?LINE},
			      {self, self()}, 
			      {message, Info}]),
    {noreply, Server}.

code_change(_OldVsn, Server, _Extra) ->
    {ok, Server}.

parse_packet(Socket, Client) ->
    receive
	{tcp, Socket, Bin} ->
	    %%io:format("--> ~w~n", [Bin]),
	    case proto:read(Bin) of
		{?PP_LOGIN, Nick, Pass} ->
		    %%io:format("Logging in ~s~n", [Nick]),
		    case login:login(Nick, Pass, self()) of
			{error, Error} ->
			    %%io:format("Login error: ~w~n", [Error]),
			    ok = ?tcpsend(Socket, {?PP_BAD, 
						   ?PP_LOGIN,
						   Error}), 
			    parse_packet(Socket, Client);
			{ok, Player} ->
			    %% disconnect visitor
			    if
				Client#client.player /= none ->
				    gen_server:cast(Client#client.player, 
						    'DISCONNECT');
				true ->
				    ok
			    end,
			    ID = gen_server:call(Player, 'ID'),
			    ok = ?tcpsend(Socket, {?PP_PID, ID}),
			    Client1 = Client#client {
				       player = Player
				      },
			    parse_packet(Socket, Client1)
		    end;
		?PP_LOGOUT ->
		    gen_server:cast(Client#client.player, 'LOGOUT'),
		    ok = ?tcpsend(Socket, {?PP_GOOD, 
					   ?PP_LOGOUT, 
					   0}),
		    %% Replace player process with a visitor
		    {ok, Visitor} = visitor:start(),
		    Client1 = Client#client {
				player = Visitor
			       },
		    gen_server:cast(Visitor, {'SOCKET', self()}),
		    parse_packet(Socket, Client1);
		{?PP_GAME_QUERY, 
		 GameType, LimitType,
		 ExpOp, Expected, 
		 JoinOp, Joined,
		 WaitOp, Waiting} ->
		    _ST = now(),
		    find_games(Socket, 
			       GameType, LimitType,
			       ExpOp, Expected, 
			       JoinOp, Joined,
			       WaitOp, Waiting),
		    _ET = now(),
		    %%Elapsed = timer:now_diff(ET, ST) / 1000,
		    %%io:format("~wms to send games to ~w~n", 
		    %%	      [Elapsed, Socket]),
		    parse_packet(Socket, Client);
		{?PP_MAKE_TEST_GAME, Data} ->
		    case gen_server:call(Client#client.server,
					 'TEST MODE') of
			true ->
			    ok = ?tcpsend(Socket, start_test_game(Data));
			_ ->
			    ok
		    end,
		    parse_packet(Socket, Client);
		?PP_PING ->
		    ok = ?tcpsend(Socket, ?PP_PONG),
		    parse_packet(Socket, Client);
		none ->
		    io:format("Unrecognized packet: ~w~n", [Bin]);
		Event ->
		    Client1 = if 
				  Client#client.player == none ->
				      %% start a proxy
				      {ok, Visitor} = visitor:start(),
				      gen_server:cast(Visitor, 
						      {'SOCKET', self()}),
				      Client#client {
					player = Visitor
				       };
				  true ->
				      Client
			      end,
		    gen_server:cast(Client1#client.player, Event),
		    parse_packet(Socket, Client1)
	    end;
	{tcp_closed, Socket} ->
	    gen_server:cast(Client#client.player, 'DISCONNECT');
	{packet, Packet} ->
	    %%io:format("<-- ~w~n", [Packet]),
	    ok = ?tcpsend(Socket, Packet),
	    parse_packet(Socket, Client)
    end.

find_games(Socket, 
	   GameType, LimitType,
	   ExpOp, Expected, 
	   JoinOp, Joined,
	   WaitOp, Waiting) ->
    {atomic, L} = game:find(GameType, LimitType,
			    ExpOp, Expected, 
			    JoinOp, Joined,
			    WaitOp, Waiting),
    lists:foreach(fun(Packet) ->
			  ?tcpsend(Socket, Packet)
		  end, L).

start_games() ->
    {atomic, Games} = db:find(game_config),
    start_games(Games, []).

start_games([Game|Rest], Acc) ->
    Acc1 = start_games(Game, Game#game_config.max, Acc),
    start_games(Rest, Acc1);

start_games([], Acc) ->
    Acc.

start_games(_Game, 0, Acc) ->
    Acc;

start_games(Game, N, Acc) ->
    {ok, Pid} = cardgame:start(Game#game_config.type, 
			       Game#game_config.seat_count, 
			       Game#game_config.limit,
			       Game#game_config.start_delay,
			       Game#game_config.player_timeout),
    start_games(Game, N - 1, [Pid|Acc]).

kill_games([]) ->
    ok;

kill_games([Pid|Rest]) ->
    cardgame:stop(Pid),
    kill_games(Rest).

start_test_game(Bin) 
  when is_binary(Bin) ->
    {GameType, Expected, Limit, Delay, Timeout, Cards} = 
	binary_to_term(Bin),
    {ok, Pid} = cardgame:start(GameType,
			       Expected,
			       Limit,
			       Delay,
			       Timeout),
    cardgame:cast(Pid, {'RIG', Cards}),
    cardgame:cast(Pid, {'REQUIRED', Expected}),
    GID = cardgame:call(Pid, 'ID'),
    {?PP_GOOD, ?PP_MAKE_TEST_GAME, GID}.
	    
%%
%% Test suite
%%

test() ->
    ok.
