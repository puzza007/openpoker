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

-module(db).

-export([test/0, set/3, get/3, inc/3, dec/3, move_amt/3]).
-export([delete/1, delete/2, find/1, find/2, find/3]).

-include("test.hrl").
-include("schema.hrl").

%%% Find the position of an atom in a list

fieldnum(Field, []) 
  when is_atom(Field) ->
    none;

fieldnum(Field, Fields)
  when is_atom(Field),
       is_list(Fields) ->
    fieldnum(Field, Fields, 1).

fieldnum(_Field, [], _N) ->
    none;

fieldnum(Field, [H|T], N) ->
    if
	Field == H ->
	    N;
	true ->
	    fieldnum(Field, T, N + 1)
    end.

%%% Update Field in Table with Value
%%% using Key to lookup the record.
%%% Fun is fun(OldFieldValue, Value)
%%% and should return the new value 
%%% of the field or the tupe {error, reason}.

set(Table, Key, {Field, Value}, Fun) 
  when is_atom(Table),
       is_atom(Field) ->
    Fields = mnesia:table_info(Table, attributes),
    case fieldnum(Field, Fields) of
	none ->
	    {atomic, {error, field_not_found}};
	N ->
	    F = fun() ->
			case mnesia:read({Table, Key}) of
			    [] ->
				{error, key_not_found};
			    [Data] ->
				case Fun(element(N + 1, Data), Value) of
				    {error, Reason} ->
					{error, Reason};
				    Value1 ->
					Data1 = setelement(N + 1, 
							   Data, 
							   Value1),
					mnesia:write(Data1)
				end;
			    Any ->
				Any
			end
		end,
	    mnesia:transaction(F)
    end.

set(Table, Key, {Field, _Value} = V) 
  when is_atom(Table),
       is_atom(Field) ->
    F = fun(_Old, New) -> New end,
    set(Table, Key, V, F);

%%% Simple set using a list of fields and values

set(Table, Key, Values) 
  when is_atom(Table),
       is_list(Values) ->
    Fields = mnesia:table_info(Table, attributes),
    case find(Table, Key) of
	{atomic, [Data]} ->
	    set(Data, Fields, Values);
	Any ->
	    Any
    end;

set(Data, _Fields, []) ->
    mnesia:transaction(fun() ->
			       mnesia:write(Data)
		       end);

set(Data, Fields, [{Field, Value}|Rest]) 
  when is_tuple(Data),
       is_list(Fields),
       is_atom(Field) ->
    case fieldnum(Field, Fields) of
	none ->
	    {atomic, {error, field_not_found}};
	N ->
	    Data1 = setelement(N + 1, 
			       Data, 
			       Value),
	    set(Data1, Fields, Rest)
    end.

%%% Retrieve value in Table 
%%% using Key to lookup the record.

get(Table, Key, Field)
  when is_atom(Table),
       is_atom(Field) ->
    Fields = mnesia:table_info(Table, attributes),
    case fieldnum(Field, Fields) of
	none ->
	    {atomic, {error, field_not_found}};
	N ->
	    F = fun() ->
			case mnesia:read({Table, Key}) of
			    [] ->
				{error, key_not_found};
			    [Data] ->
				element(N + 1, Data);
			    Any ->
				Any
			end
		end,
	    mnesia:transaction(F)
    end.


dec(Table, Key, Field) 
  when is_atom(Table),
       is_tuple(Field) ->
    F = fun(Balance, Amount) ->
		if 
		    Amount > Balance ->
			{error, out_of_balance};
		    true ->
			Balance - Amount
		end
	end,
    set(Table, Key, Field, F).

inc(Table, Key, Field) 
  when is_atom(Table),
       is_tuple(Field) ->
    F = fun(Balance, Amount) -> Balance + Amount end,
    set(Table, Key, Field, F).

move_amt(Table, Key, {From, To, Value}) 
  when is_atom(Table),
       is_atom(From),
       is_atom(To),
       is_number(Value) ->
    Fields = mnesia:table_info(Table, attributes),
    FromN = fieldnum(From, Fields),
    ToN = fieldnum(To, Fields),
    F = fun() ->
		case mnesia:read({Table, Key}) of
		    [] ->
			{error, key_not_found};
		    [Data] ->
			FromVal = element(FromN + 1, Data),
			ToVal = element(ToN + 1, Data),
			if
			    FromVal - Value < 0 ->
				{error, out_of_balance};
			    true ->
				Data1 = setelement(FromN + 1,
						   Data,
						   FromVal - Value),
				Data2 = setelement(ToN + 1,
						   Data1,
						   ToVal + Value),
				mnesia:write(Data2)
			end;
		    Any ->
			Any
		end
	end,
    if
	(FromN == none) or (ToN == none) ->
	    {atomic, {error, field_not_found}};
	true ->
	    mnesia:transaction(F)
    end.

delete(Table) 
  when is_atom(Table) ->
    mnesia:clear_table(Table).

delete(Table, KeyVal) 
  when is_atom(Table) ->
    F = fun() -> mnesia:delete({Table, KeyVal}) end,
    mnesia:transaction(F).

%%% Make a {table_name, '_', ...} pattern
%%% to match and retrieve all table rows.

makepat(Table)
  when is_atom(Table) ->
    Fields = mnesia:table_info(Table, attributes),
    makepat(Fields, [Table]).

makepat([], Acc) ->
    list_to_tuple(lists:reverse(Acc));

makepat([_H|T], Acc) ->
    makepat(T, ['_'|Acc]).

find(Table) 
  when is_atom(Table) ->
    Pat = makepat(Table),
    F = fun() -> mnesia:match_object(Pat) end,
    mnesia:transaction(F).

%%% Lookup using primary key value

find(Table, KeyVal) 
  when is_atom(Table) ->
    F = fun() -> mnesia:read({Table, KeyVal}) end,
    mnesia:transaction(F).
    
%%% Lookup using a secondary index

find(Table, Field, Value) 
  when is_atom(Table),
       is_atom(Field) ->
    Fields = mnesia:table_info(Table, attributes),
    case fieldnum(Field, Fields) of
	none ->
	    {atomic, {error, field_not_found}};
	N ->
	    F = fun() -> 
			mnesia:index_read(Table, Value, N + 1) 
		end,
	    mnesia:transaction(F)
    end.

%%% 
%%% Test harness
%%%

test() ->
    test1(),
    test2(),
    test3(),
    ok.

test1() ->
    ?match(none, fieldnum(foo, [])),
    ?match(none, fieldnum(foo, [bar, baz])),
    ?match(1, fieldnum(foo, [foo, bar, baz])),
    ?match(3, fieldnum(baz, [foo, bar, baz])).

test2() ->
    ?match({game_xref, '_', '_', '_', '_'}, 
	   makepat(game_xref)).

test3() ->
    Config = #cluster_config {
      id = 1,
      max_login_errors = 0
     },
    F = fun() -> mnesia:write(Config) end,
    {atomic, ok} = mnesia:transaction(F),
    %% bad table name
    Result1 = (catch set(foo, 1, {max_login_errors, 3})),
    ?match({'EXIT',{aborted,{no_exists,foo,attributes}}}, Result1),
    %% bad key value
    Result2 = set(cluster_config, 2, {max_login_errors, 3}),
    ?match({atomic, {error, key_not_found}}, Result2),
    %% bad field name
    Result3 = set(cluster_config, 1, {foo, 3}),
    ?match({atomic, {error, field_not_found}}, Result3),
    %% error 
    Fun1 = fun(_, _) -> {error, balance} end,
    Result4 = set(cluster_config, 1, {max_login_errors, 3}, Fun1),
    ?match({atomic, {error, balance}}, Result4),
    %% should work
    Result5 = set(cluster_config, 1, {max_login_errors, 3}),
    ?match({atomic, ok}, Result5),
    ?match({atomic, 3}, get(cluster_config, 1, max_login_errors)),
    %% bump it up
    Result6 = inc(cluster_config, 1, {max_login_errors, 4}),
    ?match({atomic, ok}, Result6),
    ?match({atomic, 7}, get(cluster_config, 1, max_login_errors)),
    %% bump it down
    Result7 = dec(cluster_config, 1, {max_login_errors, 3}),
    ?match({atomic, ok}, Result7),
    ?match({atomic, 4}, get(cluster_config, 1, max_login_errors)),
    %% list of field values
    {atomic, ok} = set(cluster_config, 1, 
		       [{logdir, "/tmp/foo"}, 
			{max_login_errors, 10}]),
    ?match({atomic, "/tmp/foo"}, get(cluster_config, 1, logdir)),
    ?match({atomic, 10}, get(cluster_config, 1, max_login_errors)),
    %% clean up
    ?match({atomic, ok}, delete(cluster_config, 1)).

		  

    

    
