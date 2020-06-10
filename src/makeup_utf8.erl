%%% File    : makeup_utf8.erl
%%% Author  : Tony Rogvall <tony@iMac.local>
%%% Description : UTF-8 processing
%%% Created : 27 Feb 2006 by Tony Rogvall <tony@iMac.local>

-module(makeup_utf8).

-rcsid("$Id: makeup_utf8.erl,v 1.3 2007/01/25 15:37:39 tony Exp $\n").

-vsn("$Revision: 1.3 $ ").

-export([input/1, output/1]).

-include("makeup_utf.hrl").

-define(ERL_R13, true).

%%
%% Convert from utf8 to unicode
%%
%% return {Valid, [code()], Continuation/1}
%%
%%
-ifdef(ERL_R13).

input(Bin) when is_binary(Bin) ->
    input_1(Bin);
input(List) when is_list(List) ->
    input_1(list_to_binary(List)).

input_1(Bin) ->
    case unicode:characters_to_list(Bin, utf8) of
	{error,List,Rest} -> input_more(false,List,Rest);
	{incomplete,List,Rest} ->input_more(true,List,Rest);
	List -> {true,List,fun input/1}
    end.
	
input_more(Valid,List,<<>>) ->
    {Valid,List,fun input/1};
input_more(Valid,List,Rest) ->
    {Valid,List,fun(Data)-> input_1(list_to_binary([Rest,Data])) end}.
	     
-else.

input(Bin) when is_binary(Bin) ->
    input(Bin, [], true);
input(List) when is_list(List) ->
    input(list_to_binary(List), [], true).

input(Bin, Acc, Valid) ->
    case Bin of
	<<2#0:1, X0:7, Bin1/binary>> ->
	    input(Bin1, [X0|Acc], Valid);
	<<2#110:3, X0:5, Bin1/binary>> ->
	    input_n(1, X0, Bin1, Acc, Valid);
	<<2#1110:4, X0:4, Bin1/binary>> ->
	    input_n(2, X0, Bin1, Acc, Valid);
	<<2#11110:5,X0:3, Bin1/binary>> ->
	    input_n(3, X0, Bin1, Acc, Valid);
	<<2#111110:6,X0:2, Bin1/binary>> ->
	    input_n(4, X0, Bin1, Acc, Valid);
	<<2#1111110:7,X0:1, Bin1/binary>> ->
	    input_n(5, X0, Bin1, Acc, Valid);
	<<>> ->
	    {Valid, lists:reverse(Acc), fun input/1}
    end.

input_n(N, X0, Bin, Acc, Valid) ->
    Sz = size(Bin),
    if Sz >= N ->
	    decode_n(N,X0,Bin,Acc,Valid);
       true ->
	    {Valid, lists:reverse(Acc),
	     fun(Data) ->
		     input_n(N, X0, list_to_binary([Bin,Data]), [], Valid)
	     end}
    end.

decode_n(N, X0, Bin, Acc, Valid) ->
    case N of
	1 ->
	    case Bin of
		<<2#10:2, X1:6, Bin1/binary>> ->
		    C = (X0 bsl 6) + X1,
		    Valid1 = (C =< ?UNI_MAX_LEGAL_UTF32) andalso Valid,
		    input(Bin1,[C|Acc],Valid1);
		<<_, Bin1/binary>> ->
		    input(Bin1,[?UNI_REPLACEMENT_CHAR|Acc],false)
	    end;
	2 ->
	    case Bin of
		<<2#10:2,X1:6,2#10:2,X2:6, Bin1/binary>> ->
		    C = (X0 bsl 12) + (X1 bsl 6) + X2,
		    Valid1 = (C =< ?UNI_MAX_LEGAL_UTF32) andalso Valid,
		    input(Bin1,[C|Acc],Valid1);
		<<_:2/binary, Bin1/binary>> ->
		    input(Bin1,[?UNI_REPLACEMENT_CHAR|Acc],false)
	    end;
	3 ->
	    case Bin of
		<<2#10:2,X1:6,2#10:2,X2:6,2#10:2,X3:6,Bin1/binary>> ->
		    C = (X0 bsl 18) + (X1 bsl 12) + (X2 bsl 6) + X3,
		    Valid1 = (C =< ?UNI_MAX_LEGAL_UTF32) andalso Valid,
		    input(Bin1,[C|Acc],Valid1);
		<<_:3/binary,Bin1/binary>> ->
		    input(Bin1,[?UNI_REPLACEMENT_CHAR|Acc],false)
	    end;
	4 -> 
	    case Bin of
		<<2#10:2,X1:6,2#10:2,X2:6,2#10:2,X3:6,2#10:2,X4:6,Bin1/binary>> ->
		    C = (X0 bsl 24) + (X1 bsl 18) + (X2 bsl 12) + (X3 bsl 6) + 
			X4,
		    Valid1 = (C =< ?UNI_MAX_LEGAL_UTF32) andalso Valid,
		    input(Bin1,[C|Acc],Valid1);
		<<_:4/binary,Bin1/binary>> ->
		    input(Bin1,[?UNI_REPLACEMENT_CHAR|Acc],false)
	    end;
	5 ->
	    case Bin of
		<<2#10:2,X1:6,2#10:2,X2:6,2#10:2,X3:6,
		 2#10:2,X4:6,2#10:2,X5:6,Bin1/binary>> ->
		    C = (X0 bsl 30) + (X1 bsl 24) + (X2 bsl 18) + (X3 bsl 12) + 
			(X4 bsl 6) + X5,
		    Valid1 = (C =< ?UNI_MAX_LEGAL_UTF32) andalso Valid,
		    input(Bin1,[C|Acc],Valid1);
		<<_:5/binary, Bin1/binary>> ->
		    input(Bin1,[?UNI_REPLACEMENT_CHAR|Acc],false)
	    end
    end.

-endif.

%%
%% Convert UTF-32 to UTF-8
%%
output(Us) ->
    output(Us, [], true).

output([U|Us], Acc, Valid) when is_integer(U) ->
    Valid1 = if U >= ?UNI_SUR_HIGH_START, U =< ?UNI_SUR_LOW_END ->
		     false;
		true -> Valid
	     end,
    if U < 16#80 ->
	    output(Us, [U | Acc], Valid1);
       U < 16#800 ->
	    <<_:5,X0:5,X1:6>> = <<U:16>>,
	    output(Us, [(2#10000000+X1),
			(2#11000000+X0)| Acc], Valid1);
       U < 16#10000 ->
	    <<X0:4,X1:6,X2:6>> = <<U:16>>,
	    output(Us, [(2#10000000+X2),
			(2#10000000+X1),
			(2#11100000+X0)	|Acc], Valid1);
       U =< ?UNI_MAX_LEGAL_UTF32 ->
	    <<_:3,X0:3,X1:6,X2:6,X3:6>> = <<U:24>>,
	    output(Us, [(2#10000000+X3),
			(2#10000000+X2),
			(2#10000000+X1),
			(2#11100000+X0) | Acc], Valid1);
       true ->
	    <<X0:4,X1:6,X2:6>> = <<?UNI_REPLACEMENT_CHAR:16>>,
	    output(Us, [(2#10000000+X2),
			(2#10000000+X1),
			(2#11100000+X0) |Acc], false)
    end;
output([], Acc, Valid) ->
    {Valid, lists:reverse(Acc)}.

	    
	    

		    
	    
	    
        


	


    



