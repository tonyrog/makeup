%%% File    : makeup_wbxml_ext.erl
%%% Author  : Tony Rogvall <tony@PBook.local>
%%% Description : WBXML extension help functions
%%% Created : 20 Aug 2006 by Tony Rogvall <tony@PBook.local>

-module(makeup_wbxml_ext).

-export([wml_value_encode/3]).
-export([wml_value_decode/4]).

-export([wml_content_encode/3]).

-export([env_value_encode/1]).
-export([env_value_decode/1]).

-export([si_datetime_encode/3]).
-export([si_datetime_decode/4]).

-import(lists, [reverse/1]).

%%
%% Encode environment values
%% ${<varname>}
%%   this will be encoded as {env,VarName}
%%
env_value_encode(Cs) ->
    enc_env(Cs, [], []).

env_value_decode({env,Name}) ->
    "${"++Name++"}".

enc_env("$$"++Cs, Str, Codes) ->
    enc_env(Cs, [$$,$$|Str], Codes);
enc_env("${"++Cs, Str, Codes)  ->
    case varname(Cs) of
	{true,Name,"}"++Cs1} ->
	    e_env(Cs1,Str,Name,Codes);
	_ ->
	    enc_env(Cs, "{\$"++Str, Codes)
    end;
enc_env([C|Cs], Str, Codes) ->
    enc_env(Cs, [C|Str], Codes);
enc_env([], [], Codes) ->
    lists:reverse(Codes);
enc_env([], Str, Codes) ->
    lists:reverse([{string,lists:reverse(Str)}|Codes]).

e_env(Cs1,[],Name,Codes) ->
    enc_env(Cs1,[],[{env,Name}|Codes]);
e_env(Cs1,Str,Name,Codes) ->
    enc_env(Cs1,[],[{env,Name},{string,lists:reverse(Str)}|Codes]).


%%
%% Encode/Decode WML variable extension
%%  $(<varname>:escape)  (e|E|escape|ESCAPE)
%%  $(<varname>:unesc)   (u|U|unesc|UNESC)
%%  $(<varname>:noesc)   (n|N|noesc|NOESC)
%%  $<varname>           (same as noesc)
%%
wml_value_decode(ext0,_,0,Var) ->
    "$("++Var++":escape)";
wml_value_decode(ext1,_,0,Var) ->
    "$("++Var++":unesc)";
wml_value_decode(ext2,_,0,Var) ->
    "$("++Var++")".

%% wml_value (assume that Cs = "$(" ++) to speed things up a bit
wml_value_encode(Cs,_Context,_Page) ->
    enc_wml(Cs).

%% wml_content scan the content for wml variables.
wml_content_encode(Cs, _Context, _Page) ->
    enc_wml(Cs, [], []).

enc_wml(Cs=[$\$|_], CsAcc, Codes) ->
    case enc_wml(Cs) of
	{{string,Cs1},Cs2} ->
	    enc_wml(Cs2, reverse(Cs1)++CsAcc, Codes);
	{Code,Cs2} ->
	    if CsAcc == [] ->
		    enc_wml(Cs2, [], [Code|Codes]);
	       true ->
		    enc_wml(Cs2, [], [Code,{string,reverse(CsAcc)}|Codes])
	    end
    end;
enc_wml([C|Cs], CsAcc, Codes) ->
    enc_wml(Cs, [C|CsAcc], Codes);
enc_wml([], [], Codes) ->
    reverse(Codes);
enc_wml([], CsAcc, Codes) ->
    reverse([{string,reverse(CsAcc)}|Codes]).

%%
%% one step encode value|content
%% return {Code, Cs'}
%%
enc_wml("$$"++Cs) ->
    {{string,"$$"}, Cs};
enc_wml("$("++Cs) ->
    case varname(Cs) of
	false ->
	    {{string,"(\$"}, Cs};
	{true,Name,Cs1} ->
	    case Cs1 of
		":e)"++Cs2 ->
		    {{extension,ext0,Name}, Cs2};
		":E)"++Cs2 ->
		    {{extension,ext0,Name}, Cs2};
		":escape)"++Cs2 ->
		    {{extension,ext0,Name}, Cs2};
		":ESCAPE)"++Cs2 ->
		    {{extension,ext0,Name}, Cs2};
		":u)"++Cs2 ->
		    {{extension,ext1,Name}, Cs2};
		":U)"++Cs2 ->
		    {{extension,ext1,Name}, Cs2};
		":unesc)"++Cs2 ->
		    {{extension,ext1,Name}, Cs2};
		":UNESC)"++Cs2 ->
		    {{extension,ext1,Name}, Cs2};
		":n)"++Cs2  -> 
		    {{extension,ext2,Name}, Cs2};
		":N)"++Cs2  -> 
		    {{extension,ext2,Name}, Cs2};
		":noesc)"++Cs2  -> 
		    {{extension,ext2,Name}, Cs2};
		":NOESC)"++Cs2  -> 
		    {{extension,ext2,Name}, Cs2};
		")"++Cs2 -> 
		    {{extension,ext2,Name}, Cs2};
		_ -> 
		    {{string,"(\$"}, Cs}
	    end
    end;
enc_wml([$$|Cs]) ->
    case varname(Cs) of
	false ->
	    {{string,[$$]}, Cs};
	{true,Name,Cs1} ->
	    {{extension,ext2,Name},Cs1}
    end;
enc_wml([C|Cs]) ->
    {{string,[C]}, Cs};
enc_wml([]) ->
    {{string,[]}, []}.

    
%% scan varname
varname([$_|Cs]) -> varname(Cs,[$_]);
varname([C|Cs]) when C >= $a, C =< $z -> varname(Cs,[C]);
varname([C|Cs]) when C >= $A, C =< $Z -> varname(Cs,[C]);
varname(_) -> false.

varname([$_|Cs],Acc) -> varname(Cs,[$_|Acc]);
varname([C|Cs],Acc) when C >= $a, C =< $z -> varname(Cs,[C|Acc]);
varname([C|Cs],Acc) when C >= $A, C =< $Z -> varname(Cs,[C|Acc]);
varname([C|Cs],Acc) when C >= $0, C =< $9 -> varname(Cs,[C|Acc]);
varname(Cs,Acc) -> {true,lists:reverse(Acc),Cs}.

%%
%% Handle the si opaque format for encoding date-time:
%% format: YYYY-MM-DDTHH:MM:SSZ
%%
si_datetime_encode(Cs,_Context,_Page) ->
    {{opaque, encode_datetime(Cs,[])},[]}.

si_datetime_decode(opaque,_,_Page,DayTime) ->
    decode_datetime(DayTime).
     

encode_datetime([$T|Cs],Acc) -> encode_datetime(Cs,Acc);
encode_datetime([$Z|Cs],Acc) -> encode_datetime(Cs,Acc);
encode_datetime([$-|Cs],Acc) -> encode_datetime(Cs,Acc);
encode_datetime([$:|Cs],Acc) -> encode_datetime(Cs,Acc);
encode_datetime([C1,C2|Cs],Acc) when C1 >= $0, C1 =< $9,
				     C2 >= $0, C2 =< $9 ->
    Octet = ((C1-$0) bsl 4) + (C2-$0),
    encode_datetime(Cs,[Octet|Acc]);
encode_datetime([], [0|Acc]) -> encode_datetime([],Acc);
encode_datetime([], Acc) ->
    lists:reverse(Acc).

decode_datetime(DayTime) ->
    L = length(DayTime),
    [Y1,Y2,Mon,Day,H,M,S] =
	if L < 7 -> DayTime++lists:duplicate(7-L, 0);
	   true -> DayTime
	end,
    [bcd(Y1 bsr 4), bcd(Y1),
     bcd(Y2 bsr 4), bcd(Y2), $-,
     bcd(Mon bsr 4),bcd(Mon), $-,
     bcd(Day bsr 4),bcd(Day), $T,
     bcd(H bsr 4),bcd(H), $:,
     bcd(M bsr 4),bcd(M), $:,
     bcd(S bsr 4),bcd(S), $Z].

bcd(I) ->
    IH = (I band 16#f),
    if IH < 10 -> (IH+$0);
       true -> $?
    end.
    


