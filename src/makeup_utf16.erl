%%% File    : makeup_utf16.erl
%%% Author  : Tony Rogvall <tony@iMac.local>
%%% Description : BOM process utf16
%%% Created : 27 Feb 2006 by Tony Rogvall <tony@iMac.local>

-module(makeup_utf16).

-rcsid("$Id: makeup_utf16.erl,v 1.1 2006/03/06 19:47:32 tony Exp $\n").

-vsn("$Revision: 1.1 $ ").

-export([input/1, output/1]).

input(<<16#FE,16#FF,Rest/binary>>) ->
    makeup_utf16_be:input(Rest);
input([16#FE,16#FF|Rest]) ->
    makeup_utf16_be:input(Rest);
input(<<16#FF,16#FE,Rest/binary>>) ->
    makeup_utf16_le:input(Rest);
input([16#FF,16#FE|Rest]) ->
    makeup_utf16_le:input(Rest);
input(Bin) when binary(Bin),size(Bin) < 2 ->
    {true, [], fun(Data) -> input(list_to_binary([Bin,Data])) end};
input(List) when list(List), length(List) < 2 ->
    {true, [], fun(Data) -> input(list_to_binary([List,Data])) end};
input(Data) ->
    {false, [], makeup_utf16_be:input(Data)}.
    
output(Us) ->
    case <<16#FEFF:16/native>> of
	<<16#FE,16#FF>> ->
	    makeup_utf16_be:output(Us);
	<<16#FF,16#FE>> ->
	    makeup_utf16_le:output(Us)
    end.

    
    
    

