%%% File    : makeup_xpath.erl
%%% Author  : Tony Rogvall <tony@pbook.synap.se>
%%% Description : XPath utilities
%%% Created : 11 May 2006 by Tony Rogvall <tony@pbook.synap.se>

-module(makeup_xpath).

-export([search/2]).
-export([match/2]).
-export([file/2, file/3]).
-export([tokens/1, location_path/1]).
-export([select/1]).
-import(lists, [reverse/1, map/2]).

-include("../include/makeup.hrl").


file(File, Expr) ->
    file(File, Expr, []).

%%
%% Expr = {path,['/', e1, e2, ...]}
%%
file(File, Expr, Opts) ->
    CtxPath =
	case filename:split(Expr) of
	    ["/" | Path] ->
		reverse(map(fun(Item) -> list_to_atom(Item) end, Path));
	    Path ->
		reverse(map(fun(Item) -> list_to_atom(Item) end, Path))
	end,
    Action =
	fun('#PCDATA', _ContextTag, Data,CtxEs={Ctx,[Bdy|Bs0]}) ->
		case lists:suffix(CtxPath, Ctx) of
		    true ->
			{Ctx,[[{'#PCDATA',Data}|Bdy] | Bs0]};
		    false ->
			CtxEs
		end;
	   ('#BEGIN', Tag, _As, {Ctx,ElemStack}) ->
		Ctx1 = [Tag|Ctx],
		case lists:suffix(CtxPath, Ctx1) of
		    true ->
			{Ctx1, [ [] | ElemStack]};
		    false ->
			{Ctx1, ElemStack}
		end;
	   ('#END', '#TOP', [], {_Ctx,ElemStack}) ->
		case ElemStack of
		    [Bdy] ->
			reverse(Bdy);
		    [Bdy,PBdy|BdyS1] -> 
			[[reverse(Bdy)|PBdy]|BdyS1]
		end;
	   ('#END', Tag, Att, {Ctx=[Tag|Ctx1],Es}) ->
		case lists:suffix(CtxPath,Ctx) of
		    true ->
			case Es of
			    [Bdy] ->   
				{Ctx1,[[{Tag,reverse(Att),reverse(Bdy)}]]};
			    [Bdy,PBdy|BdyS1] ->
				{Ctx1,[[{Tag,reverse(Att),reverse(Bdy)}|PBdy]|BdyS1]}
			end;
		    false ->
			{Ctx1, Es}
		end;
	   ('#PI', _Target, _Value, CtxEs) ->   CtxEs;
	   ('#!', _Comment, _Value, CtxEs) ->  CtxEs;
	   ('#INIT', _Tag, _Att, _ElemStack) -> {[],[]}
	end,
    case makeup:file(File,Opts++[{actionf,Action}]) of
	{ok, Doc} ->
	    io:format(makeup:format(Doc#makeup_document.content));
	Error ->
	    Error
    end.


%% @doc Finds first XmlSectionTag that matches TagName and TagConds in Xml that
%% is the output of makeup:string(XmlStr).
%%
%% @spec search(Search, Xml) -> XmlSectionTag
%%
%% XmlSectionTag = {TagName, [Attribute], [XmlSection]}
%% XmlSection = {TagName, [Attribute], [XmlSection]} | {'#PCDATA', string()}
%% Search = {tag,TagName,TagConds}
%% TagConds = {attribute,AttrName,AttrValue} | {attribute,AttrName}
%% Attribute = {AttrName, AttrValue}
%% TagName = atom()
%% AttrName = atom()
%% AttrValue = string()
search(S={tag,Tag,TagConds}, [HXml={Tag,_As,Cs}|TXml]) ->
    %%io:format("search 1\n"),
    %% Tag Matches
    case tagconds(HXml,TagConds) of
	V = {value, _} -> V;
	_ -> search_Cs(S,Cs,TXml)
    end;
search(S={tag,_Tag,_TagConds}, [{_Tag2,_As,Cs}|TXml]) ->
    %%io:format("xml_search 2\n"),
    %% no tag match
    search_Cs(S,Cs,TXml);
search(S={tag,_Tag,_TagConds}, [_HXml|TXml]) ->
    %%io:format("xml_search 2b\n"),
    %% '#PCDATA' for instance
    search(S,TXml);
search(_S,[]) ->
    %%io:format("xml_search 3\n"),
    false.

search_Cs(S,Cs,TXml) ->
    %%io:format("xml_search_Cs ~p\n",[Cs]),
    case search(S,Cs) of
	V={value,_} -> V;
	_ -> search(S,TXml)
    end.

tagconds(Xml={_Tag,As,_Cs},[{attribute,Name,Value}|TTagConds]) ->
    %%io:format("xml_tagconds 1 ~p\n",[Xml]),
    NV = {Name,Value},
    case lists:member(NV,As) of
	true -> tagconds(Xml,TTagConds);
	false -> false
    end;
tagconds(Xml={_Tag,As,_Cs},[{attribute,Name}|TTagConds]) ->
    %%io:format("xml_tagconds 2 ~p\n",[Xml]),
    case lists:keysearch(Name,1,As) of
	{value, _} -> tagconds(Xml,TTagConds);
	false -> false
    end;
tagconds(Xml,[]) ->
    %%io:format("xml_tagconds 3\n"),
    {value,Xml}.

%%
%% Match to XML trees allow:
%%
%%  '_'                      match anythin
%%  {var}                    match with "variable" var
%%  {Tag,As,Cs}
%%    where 
%%      Tag = atom() | {var} | '_'
%%      As = '_' | [] | [A|As]
%%      A = {Attr,Value}
%%      Attr = atom() | {var} | '_'
%%      Value = string() | {var} | '_'
%%      Cs = [] | [{Tag,As,Cs}]
%%
%%  Return value:
%%      on success a binding list is returned
%%      on failure exit(bad_match) is generated
%%
match(Pattern, XML) ->
    dict:to_list(match(Pattern, XML, dict:new())).

%% match tag
match('_', _Xml, Bind) -> 
    Bind;
match({Var}, Xml, Bind) -> 
    match_var(Var,Xml,Bind);
match({Tag1,As1,Cs1}, {Tag2,As2,Cs2}, Bind) ->
    Bind1 = match_pat(Tag1,Tag2,Bind),
    Bind2 = match_attr_list(As1,As2,Bind1),
    match_cs(Cs1,Cs2,Bind2);
match({Tag1,Data1},{Tag2,Data2},Bind) ->
    Bind1 = match_pat(Tag1,Tag2,Bind),
    match_pat(Data1,Data2,Bind1).
%%
%% match sections
%% FIXME: make it possible to match repetitive sections etc.
%%
match_cs('_', _Cs, Bind) ->
    Bind;
match_cs({Var}, Cs, Bind) -> 
    match_var(Var, Cs, Bind);
match_cs([{Var}|Ps], [C|Cs], Bind) -> 
    Bind1=match_var(Var, C, Bind),
    match_cs(Ps, Cs, Bind1);
match_cs([P|Ps], [C|Cs], Bind) ->
    Bind1 = match(P, C, Bind),
    match_cs(Ps, Cs, Bind1);
match_cs([],[],Bind) ->
    Bind.
%%
%% match attribute lists:
%%   attributes are matched in any order
%%
match_attr_list('_', _As,Bind) -> 
    Bind;
match_attr_list({Var},As,Bind) -> 
    match_var(Var,As,Bind);
match_attr_list([{{Var},'_'}|As1],[{Attr,_Val}|As2],Bind) ->
    Bind1=match_var(Var,Attr,Bind),
    match_attr_list(As1,As2,Bind1);
match_attr_list([{{Var1},{Var2}}|As1],[{Attr,Val}|As2],Bind) ->
    Bind1=match_var(Var1,Attr,Bind),    
    Bind2=match_var(Var2,Val,Bind1),
    match_attr_list(As1,As2,Bind2);
match_attr_list([{{Var},Value}|As1], As2,Bind) ->
    case lists:keysearch(Value, 2, As2) of
	false -> exit(bad_match);
	{value,Elem={Attr,_}} ->
	    Bind1 = match_var(Var,Attr,Bind),
	    match_attr_list(As1, As2--[Elem], Bind1)
    end;
match_attr_list([{Attr,{Var}}|As1], As2,Bind) ->
    case lists:keysearch(Attr, 1, As2) of
	false -> exit(bad_match);
	{value,E={_,Val}} ->
	    Bind1 = match_var(Var,Val,Bind),
	    match_attr_list(As1, As2--[E], Bind1)
    end;
match_attr_list([E={_Attr,_Val}|As1], As2,Bind) ->
    case lists:member(E,As2) of
	false -> exit(bad_match);
	true ->
	    match_attr_list(As1, As2--[E], Bind)
    end;
match_attr_list([], [], Bind) ->
    Bind;
match_attr_list(_, _, _Bind) ->
    exit(bad_match).

match_pat('_', _Val, Bind) -> Bind;
match_pat(Val, Val, Bind) -> Bind;
match_pat({Var},Val,Bind) -> match_var(Var,Val,Bind);
match_pat(_Pat, _Val, _Bind) ->
    exit(bad_match).
	    
match_var(Var,Val,Bind) ->
    case dict:find(Var, Bind) of
	{ok,Val} -> Bind;
	error -> dict:store(Var,Val,Bind);
	_ -> exit(bad_match)
    end.

select(String) ->
    {Expr,[]} = expr(tokens(String)),
    Expr.
    
%%
%% XPath expression
%%
location_path(Ts) ->
    case Ts of
	['/'] -> 
	    {{xpath,[{root,node,[]}]}, []};
	['/'|Ts1] ->
	    {Path,Ts2} = relative_location_path(Ts1),
	    {{xpath,[{child,'/',[]}|Path]},Ts2};
	['//'|Ts1] ->
	    {Path,Ts2} = relative_location_path(Ts1),
	    {{xpath,[{'descendant-or-self','/',[]}|Path]},Ts2};
	_ ->
	    {Path,Ts2} = relative_location_path(Ts),
	    {{xpath,Path},Ts2}
    end.

relative_location_path(Ts) ->  
    relative_location_path(Ts,[]).
	    
relative_location_path(Ts, Steps) ->
    case step(Ts) of
	{Step, ['/'|Ts1]} ->
	    relative_location_path(Ts1, [Step|Steps]);
	{Step, ['//'|Ts1]} ->
	    relative_location_path(Ts1, [{'descendant-or-self', node, []},
					 Step|Steps]);
	{Step, Ts1} ->
	    {reverse([Step|Steps]), Ts1}
    end.

step(Ts) ->
    case Ts of
	['@'|Ts1] ->
	    step('attribute', Ts1);
	['..'|Ts1] ->
	    {{'parent',{test,node},[]}, Ts1};
	['.'|Ts1] ->
	    {{'self',{test,node},[]},Ts1};
	[{word,"ancestor"},'::'|Ts1] -> 
	    step('ancestor', Ts1);
	[{word,"ancestor-or-self"},'::'|Ts1] ->
	    step('ancestor-or-self', Ts1);
	[{word,"attribute"},'::'|Ts1] ->
	    step('attribute', Ts1);
	[{word,"child"},'::'|Ts1] ->
	    step('child', Ts1);
	[{word,"descendant"},'::'|Ts1] ->
	    step('descendant', Ts1);
	[{word,"descendant-or-self"},'::'|Ts1] ->
	    step('descendant-or-self', Ts1);
	[{word,"following"},'::'|Ts1] ->
	    step('following', Ts1);
	[{word,"following-sibling"},'::'|Ts1] ->
	    step('following-sibling', Ts1);
	[{word,"namespace"},'::'|Ts1] ->
	    step('namespace', Ts1);
	[{word,"parent"},'::'|Ts1] ->
	    step('parent', Ts1);
	[{word,"preceding"},'::'|Ts1] ->
	    step('preceding', Ts1);
	[{word,"preceding-sibling"},'::'|Ts1] ->
	    step('preceding-sibling', Ts1);
	[{word,"self"}, '::'|Ts1] ->
	    step('self', Ts1);
	_ ->
	    step('child', Ts)
    end.


step(Axis, Ts) ->
    case Ts of
	['*'|Ts1] ->
	    step(Axis,'*',[],Ts1);
	[{word,"comment"},'(', ')' | Ts1] ->
	    step(Axis, {test,comment},[],Ts1);
	[{word,"text"},'(', ')' | Ts1] ->
	    step(Axis, {test,text}, [], Ts1);
	[{word,"processing-instruction"},'(',{string,Target}, ')' | Ts1] ->
	    step(Axis, {test,{processing_instruction,Target}},[],Ts1);
	[{word,"node"},'(',')' | Ts1] ->
	    step(Axis, {test,node}, [], Ts1);
	[{word,NCName},':','*'|Ts1] ->
	    step(Axis,[list_to_atom(NCName)|'*'],[],Ts1);
	[{word,Prefix},':',{word,Name} | Ts1] ->
	    step(Axis,[list_to_atom(Prefix)|list_to_atom(Name)],[],Ts1);
	[{word,NCName} | Ts1] ->
	    step(Axis,list_to_atom(NCName),[],Ts1)
    end.

step(Axis, NodeTest, Ps, ['['|Ts]) ->
    case predicate(Ts) of
	{P, [']'|Ts1]} ->
	    step(Axis,NodeTest,[P|Ps],Ts1);
	{P, Ts1} ->
	    {{Axis,NodeTest,reverse([P|Ps])},Ts1}
    end;
step(Axis, NodeTest, Ps, Ts) ->
    {{Axis,NodeTest,reverse(Ps)},Ts}.

    
predicate(Ts) -> expr(Ts).

expr(Ts) -> exprf(fun and_expr/1, ['or'], Ts).
and_expr(Ts) -> exprf(fun eq_expr/1, ['and'], Ts).
eq_expr(Ts) ->  exprf(fun rel_expr/1, ['=', '!='], Ts).
rel_expr(Ts) -> exprf(fun add_expr/1, ['<','<=', '>', '>='], Ts).
add_expr(Ts) -> exprf(fun mul_expr/1, ['+','-'], Ts). 
mul_expr(Ts) -> exprf(fun unary_expr/1, ['*','div','mod'], Ts). 

unary_expr(['-'|Ts1]) ->
    {E, Ts2} = unary_expr(Ts1),
    {{'-',E}, Ts2};
unary_expr(Ts) ->
    union_expr(Ts).

expr_list(Ts) ->
    expr_list(Ts,[]).

expr_list(Ts=[')'|_], List) ->
    {reverse(List), Ts};
expr_list(Ts, List) ->
    case expr(Ts) of
	{Expr, [','|Ts1]} ->
	    expr_list(Ts1, [Expr|List]);
	{Expr, Ts1} ->
	    {reverse([Expr|List]), Ts1}
    end.

union_expr(Ts) -> exprf(fun path_expr/1, ['|'], Ts).
    
path_expr(Ts) ->
    case Ts of 
	['\$',{word,Name} | Ts1] -> 
	    path_expr1(Ts1, [{var,Name}]);
	['(' | Ts1] ->
	    {Expr,[')'|Ts2]} = expr(Ts1),
	    path_expr1(Ts2, [Expr]);
	[{string,Lit}|Ts1] -> 
	    path_expr1(Ts1, [{string,Lit}]);
	[{number,N}|Ts1] -> 
	    path_expr1(Ts1, [{number,N}]);
	[{word,Name},'('|Ts1] ->
	    {ArgList,[')'|Ts2]} = expr_list(Ts1),
	    path_expr1(Ts2, [{function,Name,ArgList}]);
	_ ->
	    location_path(Ts)
    end.
	
path_expr1(Ts, Fs) ->
    case Ts of
	['\$',{word,Name} | Ts1] -> 
	    path_expr1(Ts1, [{var,Name}|Fs]);
	['(' | Ts1] ->
	    {Expr,[')'|Ts2]} = expr(Ts1),
	    path_expr1(Ts2, [Expr|Fs]);
	[{string,Lit}|Ts1] -> 
	    path_expr1(Ts1, [{string,Lit}|Fs]);
	[{number,N}|Ts1] -> 
	    path_expr1(Ts1, [{number,N}|Fs]);
	[{word,Name},'('|Ts1] ->
	    {ArgList,[')'|Ts2]} = expr_list(Ts1),
	    path_expr1(Ts2, [{function,Name,ArgList}|Fs]);
	['['|Ts1] ->
	    case predicate(Ts1) of
		{P,[']','/'|Ts2]} ->
		    {Path,Ts3} = relative_location_path(Ts2),
		    {{xfilter,reverse(Fs),P,
		      [{child,'/',[]}|Path]},Ts3};
		{P,[']','//'|Ts2]} ->
		    {Path,Ts3} = relative_location_path(Ts2),
		    {{xfilter,reverse(Fs),P,
		      [{'descendant-or-self','/',[]}|Path]},Ts3};
		{P,[']' | Ts2]} ->
		    {{xfilter,reverse(Fs),P,[]},Ts2}
	    end;
	['/'|Ts1] ->
	    {Path,Ts2} = relative_location_path(Ts1),
	    {{xfilter,reverse(Fs),true,[{child,'/',[]}|Path]},Ts2}; 
	['//'|Ts1] ->
	    {Path,Ts2} = relative_location_path(Ts1),
	    {{xfilter,reverse(Fs),true,[{'descendant-or-self','/',[]}|Path]},Ts2};
	_ ->
	    case Fs of
		[Expr] -> {Expr,Ts};
		_  -> {{xfilter,reverse(Fs),true,[]},Ts}
	    end
    end.

exprf(Fun, Operators, Ts) ->
    case Fun(Ts) of
	{E1, Ts0=[Op|Ts1]} ->
	    case lists:member(Op, Operators) of
		true ->
		    {E2,Ts2} = exprf(Fun,Operators,Ts1),
		    {{Op,E1,E2}, Ts2};
		false ->
		    {E1,Ts0}
	    end;
	Expr ->
	    Expr
    end.

%%
%% XPath tokens
%%
tokens(String) ->
    tokens(String, []).

tokens(Cs0, Acc) ->
    case Cs0 of
	[?NL|Cs]  -> tokens(Cs,Acc);
	[?CR|Cs]  -> tokens(Cs,Acc);
	[?SP|Cs]  -> tokens(Cs,Acc);
	[?TAB|Cs] -> tokens(Cs,Acc);
	[?LBRACKET|Cs] -> tokens(Cs,['['|Acc]);
	[?RBRACKET|Cs] -> tokens(Cs,[']'|Acc]);
	[?LPAREN|Cs]  -> tokens(Cs,['('|Acc]);
	[?RPAREN|Cs]  -> tokens(Cs,[')'|Acc]);
	[?DOT,?DOT|Cs] -> tokens(Cs,['..'|Acc]);
	[?DOT,C|Cs] when ?is_digit(C) -> 
	    token_fraction(Cs,[C,?DOT,$0],Acc);
	[?DOT|Cs]      -> tokens(Cs,['.'|Acc]);
	[$@|Cs]      -> tokens(Cs,['@'|Acc]);
	[$,|Cs]      -> tokens(Cs,[','|Acc]);
	[$:,$:|Cs]      -> tokens(Cs,['::'|Acc]);
	[$/,$/|Cs]   -> tokens(Cs,['//'|Acc]);
	[$/|Cs]      -> tokens(Cs,['/'|Acc]);
	[$||Cs]      -> tokens(Cs,['|'|Acc]);
	[$+|Cs]      -> tokens(Cs,['+'|Acc]);
	[$-|Cs]      -> tokens(Cs,['-'|Acc]);
	[$=|Cs]      -> tokens(Cs,['='|Acc]);
	[$!,$=|Cs]   -> tokens(Cs,['!='|Acc]);
	[$<,$=|Cs]   -> tokens(Cs,['<='|Acc]);
	[$<|Cs]      -> tokens(Cs,['<'|Acc]);
	[$>,$=|Cs]   -> tokens(Cs,['>='|Acc]);
	[$>|Cs]      -> tokens(Cs,['>'|Acc]);
	[$*|Cs]      -> tokens(Cs,['*'|Acc]);
	[$$|Cs]      -> tokens(Cs,['\$'|Acc]);
	[?APOS|Cs]   -> token_string(Cs,?APOS,[],Acc);
	[?QUOT|Cs]   -> token_string(Cs,?QUOT,[],Acc);
	
	[C|Cs] ->
	    if C >= $a, C =< $z -> token_name(Cs, [C], Acc);
	       C >= $A, C =< $Z -> token_name(Cs, [C], Acc);
	       ?is_digit(C)     -> token_number(Cs,[C],Acc);
	       C == $_ -> token_name(Cs, [C], Acc);
	       true -> tokens(Cs, [{char,C}|Acc])
	    end;
	[] ->
	    reverse(Acc)
    end.

%% Parse name
token_name(Cs0=[C|Cs], Ts, Acc) ->
    if C >= $a, C =< $z -> token_name(Cs, [C|Ts], Acc);
       C >= $A, C =< $Z -> token_name(Cs, [C|Ts], Acc);
       C >= $0, C =< $9 -> token_name(Cs, [C|Ts], Acc);
       C == $_ -> token_name(Cs, [C|Ts], Acc);
       C == $- -> token_name(Cs, [C|Ts], Acc);
       C == ?DOT -> token_name(Cs, [C|Ts], Acc);
       true -> tokens(Cs0,[{word,reverse(Ts)}|Acc])
    end;
token_name([], Ts, Acc) ->
    tokens([],[{word,reverse(Ts)}|Acc]).

    
%% Parse number
token_number([C|Cs],Ns,Acc) when ?is_digit(C) ->
    token_number(Cs,[C|Ns],Acc);
token_number([?DOT|Cs], Ns, Acc) ->
    token_fraction(Cs, [?DOT|Ns], Acc);
token_number(Cs,Ns,Acc) ->
    tokens(Cs, [{number,list_to_integer(reverse(Ns))}|Acc]).

token_fraction([C|Cs],Ns,Acc) when ?is_digit(C) ->
    token_fraction(Cs,[C|Ns],Acc);
token_fraction(Cs,Ns,Acc) ->
    tokens(Cs, [{number,list_to_float(reverse(Ns))}|Acc]).


%% Parse string
token_string([Q|Cs],Q,Str,Acc) ->
    tokens(Cs, [{string,reverse(Str)}|Acc]);
token_string([C|Cs],Q,Str,Acc) ->
    token_string(Cs,Q,[C|Str],Acc);
token_string([],_Q,Str,Acc) ->
    tokens([], [{string,reverse(Str)}|Acc]).
