-module(makeup_soapenc).
-compile(export_all).

find_record(Body, ID) ->
    Rec = [{_,Attr,_}] = lists:filter(fun({_,A,_}) -> lists:member({id,ID},A) end,Body),
    {value, {_,TA}} = lists:keysearch([xsi|type],1,Attr),
    {TA,Rec}.

decode_Array(Array,Body,EnvAttrs,Ns2Mod) ->
    Dec = fun({_,Attr,Val0}) -> 
		  {TypeAttr,Val} = case lists:keysearch(href,1,Attr) of
				       {value, {_,[$#|ID]}} -> find_record(Body,ID);
				       _ ->
					   {value, {_,TA}} = lists:keysearch([xsi|type],1,Attr),
					   {TA,Val0}
				   end,
		  ValAttr = case Val of [{_,A,_}] -> A; _ -> Attr end,
		  [NsStr,Type] = string:tokens(TypeAttr,":"),
		  {value, {_,NsUri}} = lists:keysearch([xmlns|list_to_atom(NsStr)],1,ValAttr++EnvAttrs),
		  Mod = Ns2Mod(NsUri),
		  Fun = list_to_atom("decode_" ++ Type),
		  Mod:Fun(Val,Body,EnvAttrs,Ns2Mod)
	  end,
    lists:map(Dec, Array).
