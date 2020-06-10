%%% File    : makeup_utf32_be.erl
%%% Author  : Tony Rogvall <tony@iMac.local>
%%% Description : UTF-32 (Big endian) processing
%%% Created : 27 Feb 2006 by Tony Rogvall <tony@iMac.local>

-module(makeup_utf32_be).

-export([input/1, output/1]).

-include("makeup_utf.hrl").

-define(U32(X), (X):32/unsigned-big-integer).

%%
%% Convert from utf32 to unicode
%%
%% return
%%     {Valid, [code()], Continuation/1}
%%
%%
input(Bin) when is_binary(Bin) ->
    input(Bin, 0, [], true);
input(List) when is_list(List) ->
    input(list_to_binary(List), 0, [], true).

input(Bin, Offset, Acc, Valid) ->
    case Bin of
	<<_:Offset/binary, ?U32(C), _/binary>> ->
	    if C > ?UNI_MAX_LEGAL_UTF32 ->
		    input(Bin, Offset+4, [C|Acc], false);
	       true ->
		    input(Bin, Offset+4, [C|Acc], Valid)
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
%% Convert unicode to UTF-32 
%%
output(Us) ->
    output(Us, [], true).

output([U|Us], Acc, Valid) ->
    if U > ?UNI_MAX_LEGAL_UTF32 ->
	    output(Us, [<<?U32(?UNI_REPLACEMENT_CHAR)>> | Acc], false);
       true ->
	    output(Us, [<<?U32(U)>> | Acc], Valid)
    end;
output([], Acc, Valid) ->
    {Valid, lists:reverse(Acc)}.
