%%% File    : makeup_utf16_be.erl
%%% Author  : Tony Rogvall <tony@iMac.local>
%%% Description : UTF-16 (Big endian) processing
%%% Created : 27 Feb 2006 by Tony Rogvall <tony@iMac.local>

-module(makeup_utf16_be).

-rcsid("$Id: makeup_utf16_be.erl,v 1.2 2006/03/06 19:47:32 tony Exp $\n").

-vsn("$Revision: 1.2 $ ").

-export([input/1, output/1]).

-include("makeup_utf.hrl").

-define(U16(X), (X):16/unsigned-big-integer).

%%
%% Convert from utf16 to unicode
%%
%% return
%%      {Valid, [code()], Continuation/1}
%%
%%
input(Bin) when binary(Bin) ->
    input(Bin, 0, [], true);
input(List) when list(List) ->
    input(list_to_binary(List), 0, [], true).

input(Bin, Offset, Acc, Valid)  ->
    case Bin of
	<<_:Offset/binary,?U16(U1),_/binary>> ->
	    if U1 >= ?UNI_SUR_HIGH_START, U1 =< ?UNI_SUR_HIGH_END ->
		    Offset2 = Offset+2,
		    case Bin of
			<<_:Offset2/binary,?U16(U2),_/binary>> ->
			    if U2 >= ?UNI_SUR_LOW_START, U2 =< ?UNI_SUR_LOW_END ->
				    U = ((U1 - ?UNI_SUR_HIGH_START) bsl 10) +
					((U2 - ?UNI_SUR_LOW_START)+16#10000),
				    input(Bin,Offset+4,[U|Acc],Valid);
			       true ->
				    input(Bin,Offset+2,[U1|Acc],false)
			    end;
			<<_:Offset2/binary,Rest/binary>> ->
			    {Valid,lists:reverse(Acc),
			     fun(Data) ->
				     input(list_to_binary([Rest,Data]),0,[],Valid)
			     end}
		    end;
	       U1 >= ?UNI_SUR_LOW_START, U1 =< ?UNI_SUR_LOW_END ->
		    input(Bin, Offset+2, [U1|Acc], false);
	       true ->
		    input(Bin, Offset+2, [U1|Acc], Valid)
	    end;
	<<_:Offset/binary>> ->
	    {Valid, lists:reverse(Acc), fun input/1};
	<<_:Offset/binary, Rest/binary>> ->
	    {Valid, lists:reverse(Acc),
	     fun(Data) ->
		     input(list_to_binary([Rest,Data]),0,[],Valid)
	     end}
    end.
    

%%
%% Convert Unicode to UTF-16
%%
output(Us) ->
    output(Us, [], true).

output([U | Us], Acc, Valid) when integer(U) ->
    if U =< ?UNI_MAX_BMP ->
	    if U >= ?UNI_SUR_HIGH_START, U =< ?UNI_SUR_LOW_END ->
		    output(Us, [<<?U16(?UNI_REPLACEMENT_CHAR)>>|Acc], false);
	       true ->
		    output(Us, [<<?U16(U)>>|Acc], Valid)
	    end;
       U > ?UNI_MAX_LEGAL_UTF32 ->
	    output(Us, [<<?U16(?UNI_REPLACEMENT_CHAR)>>|Acc], false);
       true ->
	    U1 = U - 16#10000,
	    Y  = (U1 bsr 10) + ?UNI_SUR_HIGH_START,
	    X  = (U1 band 16#3FF) + ?UNI_SUR_LOW_START,
	    output(Us,[<<?U16(Y),?U16(X)>>|Acc],Valid)
    end;
output([], Acc, Valid) ->
    {Valid, lists:reverse(Acc)}.

	    
	    
		   
