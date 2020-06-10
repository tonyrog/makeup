%%% File    : makeup_iso_8859_1.erl
%%% Author  : Tony Rogvall <tony@iMac.local>
%%% Description : ISO Latin-1 processing
%%% Created : 27 Feb 2006 by Tony Rogvall <tony@iMac.local>

-module(makeup_iso_8859_1).

-export([input/1, output/1]).

%%
%% Convert from lastin-1 to unicode
%%
%% return
%%     {Valid, [code()], Continuation/1}
%%
%%
input(Bin) when is_binary(Bin) ->
    {true, binary_to_list(Bin), fun input/1};
input(List) when is_list(List) ->
    {true, List, fun input/1}.

output(Us) ->
    output(Us, [], true).

output([U|Us], Acc, Valid) ->
    if U > 255 ->
	    output(Us, [U band 16#ff | Acc], false);
       true ->
	    output(Us, [U | Acc], Valid)
    end;
output([], Acc, Valid) ->
    {Valid, lists:reverse(Acc)}.
