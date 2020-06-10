%%% File    : makeup_ascii.erl
%%% Author  : Tony Rogvall <tony@iMac.local>
%%% Description : ASCII character processing
%%% Created : 27 Feb 2006 by Tony Rogvall <tony@iMac.local>

-module(makeup_ascii).

-rcsid("$Id: makeup_ascii.erl,v 1.1 2006/03/06 19:47:32 tony Exp $\n").

-vsn("$Revision: 1.1 $ ").

-export([input/1, output/1]).

%%
%% Convert from ascii to unicode
%%
%% return
%%     {Valid, [code()], Continuation/1}
%%
%%
input(Bin) when binary(Bin) ->
    {true, binary_to_list(Bin), fun input/1};
input(List) when list(List) ->
    {true, List, fun input/1}.

output(Us) ->
    output(Us, [], true).

output([U|Us], Acc, Valid) ->
    if U > 127 ->
	    output(Us, [U band 16#7f | Acc], false);
       true ->
	    output(Us, [U | Acc], Valid)
    end;
output([], Acc, Valid) ->
    {Valid, lists:reverse(Acc)}.


