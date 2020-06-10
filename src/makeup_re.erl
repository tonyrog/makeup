%%% File    : makeup_re.erl
%%% Author  : Tony Rogvall <tony@bulldog.synap.se>
%%% Description : Regular expression support
%%% Created :  1 Feb 2005 by Tony Rogvall <tony@bulldog.synap.se>

-module(makeup_re).

-export([match/2, format/1, format_tag/1, symbols/1]).

-export([make_dfa/1, make_dfa/2,
	 make_nfa/1, make_nfa/2,
	 afa_from_dfa/1,
	 make_dfa_new/1,
	 minimise_dfa/2]).

-include("../include/makeup.hrl").

-define(epsilon, empty).    %% epsilon
-define(pcdata,  pcdata).   %% only match pcdata
-define(any,     any).      %% any (including nothing)
-define(dot,     '_').      %% match any one


-record(cre,
	{
	  operator,
	  operand,
	  firstpos,
	  lastpos,
	  nullable
	 }).


%%
%% Pretty print rule
%%
format(Expr) ->
    %% io:format("Expr=~p\n", [Expr]),
    case Expr of
	undefined -> "Missing rule";
	{pclosure,RE} -> format_post(RE, "+");
	{closure,RE}  -> format_post(RE, "*");
	{optional,RE} -> format_post(RE, "?");
	{choice,Occure,REs} ->
	    [format_list(REs, $|),format_occure(Occure)];
	{choice,REs} ->
	    format_list(REs, $|);
	{all,Occure,REs} -> 
	    [format_list(REs, $&),format_occure(Occure)];
	{all,REs} -> 
	    format_list(REs, $&);
	{sequence,Occure,REs} -> 
	    [format_list(REs, $,),format_occure(Occure)];
	{sequence,REs} -> 
	    format_list(REs, $,);
	{element,Occure,E} ->
	    [format_tag(E), format_occure(Occure)];
	{element,E} ->
	    format_tag(E);
	{char,C} ->
	    format_char(C);
	{action,Actions} ->
	    ["{",format_actions(Actions)++"}"];
	{any,Occure} ->
	    ["ANY", format_occure(Occure)];
	any       -> "ANY";

	{pcdata,Occure} ->
	    ["#PCDATA", format_occure(Occure)];
	pcdata ->
	    "#PCDATA";
	empty     -> 
	    "EMPTY"
    end.

format_post(RE, PostOp) ->
    case need_paren(RE) of
	true -> ["(", format(RE), ")", PostOp];
	false -> [format(RE),PostOp]
    end.

need_paren({sequence,_}) -> true;	
need_paren({sequence,_,_}) -> true;
need_paren({choice,_}) -> true;	
need_paren({choice,_,_}) -> true;
need_paren({all,_}) -> true;
need_paren({all,_,_}) -> true;
need_paren({pclosure,_}) -> true;
need_paren({closure,_}) -> true;
need_paren({optional,_}) -> true;
need_paren(_) -> false.
    

format_occure({1,1}) -> "";
format_occure({0,1}) -> "?";
format_occure({0,unbounded}) -> "*";
format_occure({1,unbounded}) -> "+";
format_occure({N,unbounded}) when is_integer(N) ->
    ["{",integer_to_list(N),",}"];
format_occure({N,N}) when is_integer(N) ->
    ["{",integer_to_list(N),"}"];
format_occure({N,M}) when is_integer(N),is_integer(M), N < M ->
    ["{",integer_to_list(N),",", integer_to_list(M),"}"].
    
format_tag(Tag) when is_atom(Tag) ->
    atom_to_list(Tag);
format_tag([Ns|Tag]) when is_atom(Ns),is_atom(Tag) ->  
    atom_to_list(Ns)++":"++atom_to_list(Tag).

format_char(Char) ->
    if Char >= $0, Char =< $9 -> [Char];
       Char >= $A, Char =< $Z -> [Char];
       Char >= $a, Char =< $z -> [Char];
       %% fixme- single char esc
       Char < 16#ff   -> 
	    io_lib:format("\\x~2.16.0B", [Char]);
       Char < 16#ffff -> 
	    io_lib:format("\\x~4.16.0B", [Char]);
       Char < 16#ffffffff ->
	    io_lib:format("\\x~8.16.0B", [Char])
    end.

format_list(REs,Op) ->
    [$(, lists:foldr(fun(RE,"")  -> format(RE);
			(RE,Acc) -> [format(RE),Op,Acc]
		     end, "", REs), $)].

format_actions([H]) -> 
    format_action(H);
format_actions([H|T]) -> 
    format_action(H)++","++format_actions(T);
format_actions([]) -> "".

format_action(nil) -> "nil";
format_action(cons) -> "cons";
format_action(list) -> "list";
format_action(string) -> "string";
format_action(push) -> "push";
format_action(undefined) -> "undefined";
format_action(defined) -> "defined";
format_action({push,T}) -> io_lib:format("push(~p)",[T]);
format_action({tuple,K}) -> 
    "tuple("++integer_to_list(K)++")";
format_action({record,N,K}) -> 
    "#"++atom_to_list(N)++"("++integer_to_list(K)++")";
format_action({mfa,M,F,A}) -> 
    atom_to_list(M)++":"++atom_to_list(F)++"("++
	io_lib:format("~p",[A])++")".

%% <re> ::= <branch> ('|' <branch> ) *
%% <branch> := <piece> *
%% <piece>  := <atom> <quantifier>?
%% <quantifier> := [?*+] | ( '{' <quantify> '}' )
%% <quantify>   := <quantRange> | <quantMin> | <QuantExact>
%% <quantRange> := <QuantExact>','<QuantExact>
%% <quantMin>   := <QuantExact>','
%% <QuantExact> := [0-9]+
%% <atom>       := <char> | <charClass> | '(' <re> ')'
%% <char>       := [^.\?*+()|#x5B#x5D]
%% <charClass>  := <charClassEsc> | <charClassExpr> | <WildcardEsc>
%% <charClassExpr> ::= '[' <charGroup> ']'
%% <charGroup>     ::= <posCharGroup> | <negCharGroup> | <charClassSub>
%% <posCharGroup>  ::= ( <charRange> | <charClassEsc> )+
%% <negCharGroup>  ::= '^' <posCharGroup>
%% <charClassSub>  ::= (<posCharGroup>|<negCharGroup>) '-' <charClassExpr>
%% <charRange>     ::= <seRange> | <XmlCharIncDash>
%% <seRange>       ::= <charOrEsc> '-' <charOrEsc>
%% <charOrEsc>     ::= <XmlChar> | <SingleCharEsc>
%% <XmlChar>       ::= [^\#x2D#x5B#x5D]
%% <XmlCharIncDash> ::= [^\#x5B#x5D]
%% <charClassEsc>   ::= ( <SingleCharEsc> | <MultiCharEsc> | 
%%                        <catEsc> | <complEsc> )
%% <SingleCharEsc>  ::= '\' [nrt\|.?*+(){}#x2D#x5B#x5D#x5E]
%% <catEsc>         ::= '\p{' <charProp> '}'
%% <complEsc>       ::= '\P{' <charProp> '}'
%% <charProp>       ::= <IsCategory> | <IsBlock>
%% <IsCategory>     ::= <Letters> | <Marks> | <Numbers> | <Punctuation>
%%                      <Separtors> | <Symbols> | <Others>
%% <Letters>        ::= 'L' [ultmo]?
%% <Marks>          ::= 'M' [nce]?
%% <Numbers>        ::= 'N' [nce]?
%% <Punctuation>    ::= 'P' [cdseifo]?
%% <Separators>     ::= 'Z' [slp]?
%% <Symbols>        ::= 'S' [mcko]?
%% <Others>         ::= 'C' [cfon]?
%% <IsBlock>        ::= 'Is' [a-zA-Z0-9#x2D]+
%%
%% <MultiCharEsc>   ::= '\' [sSiIcCdDwW]
%% <WildcardEsc>    ::= '.'
%%

%%
%% Check rule R with a list of elements
%% Special case accept empty PCDATA/ANY/#PCDATA sections as ""
%%
match(pcdata, []) -> match;
match(any, []) -> match;
match(RE, L) ->
    RE1 = translate(RE),
    case re_apply(L, 1, {sequence,['^',RE1]}) of  %'
	{match,_,[]} -> accept;
	{match,_,_}  -> reject;
	more         -> more;
	nomatch      -> reject
    end.

%%
%% Interpret rule
%%    
re_apply(S, P, RE) -> 
    re_apply(RE, [], S, P).

%% ANY/#PCDATA/CDATA Must have been translated before this call
re_apply(empty, More, S, P) ->
    re_apply_more(More, S, P);
re_apply(any, More, [_|S], P) ->
    re_apply_more(More, S, P+1);
re_apply(dot, More, [_|S], P) ->
    re_apply_more(More, S, P+1);
re_apply({element,E}, More, [E|S], P) ->
    re_apply_more(More, S, P+1);
re_apply({char,C}, More, [C|S], P) ->
    re_apply_more(More, S, P+1);
re_apply({choice,REs}, More, S, P) ->
    lists:foldl(
      fun(RE, Acc) ->
	      re_apply_or(re_apply(RE,More,S,P), Acc)
      end, nomatch, REs);
re_apply({all,REs}, More, S, P) ->
    fold_permutations(
      fun(REs1, Acc) ->
	      re_apply_or(re_apply({sequence,REs1},More,S,P), Acc)
      end, nomatch, REs);
re_apply({sequence,[RE1|REs]}, More, S0, P) ->
    re_apply(RE1, REs++More, S0, P);
re_apply({closure,CE}, More, S, P) ->
    re_apply_or(re_apply_more(More, S, P),
		re_apply(CE, [{closure,CE}|More], S, P));
re_apply('^', More, S, 1) -> re_apply_more(More, S, 1);
re_apply('$', More, [], P) -> re_apply_more(More, [], P);

re_apply(_RE, _More, _S, _P) -> nomatch.

re_apply_more([],  [], P) -> {match,P,[]};
re_apply_more([{closure,_}],  [], P) -> {match,P,[]};
re_apply_more(_REs, [], _P) -> more;
re_apply_more([RE|More], S, P) -> re_apply(RE, More, S, P);
re_apply_more([], S, P) -> {match,P,S}.

re_apply_or({match,P1,S1},   {match,P2,_S2}) when P1 >= P2 -> {match,P1,S1};
re_apply_or({match,_P1,_S1}, {match,P2,S2}) -> {match,P2,S2};
re_apply_or(more, {match,P2,S2}) -> {match,P2,S2};
re_apply_or({match,P1,S1}, more) -> {match,P1,S1};
re_apply_or(nomatch, R2) -> R2;
re_apply_or(R1, nomatch) -> R1.

%% Generate all permutations of Ls and
fold_permutations(Fun, Acc, L) ->
    fold_perm(L,L,[],Fun,Acc).

fold_perm(_, [], X, Fun,Acc) ->
    Fun(X,Acc);
fold_perm([H|T], L, X, Fun, Acc) ->
    L1 = L -- [H],
    Acc1 = fold_perm(L1, L1, [H|X], Fun, Acc),
    fold_perm(T, L, X, Fun,Acc1);
fold_perm([], _L, _X, _Fun, Acc) ->
    Acc.
%%
%% Translate expression into interal form
%%
translate(RE) ->
    %% io:format("RE=~p\n", [RE]),
    RE1 = rewrite(RE),
    %% io:format("RE'=~p\n", [RE1]),
    RE2 = expand(RE1),
    %% io:format("RE''=~p\n", [RE2]),
    RE2.


%%
%% Rewrite non-standard operators into standard ones,
%%
rewrite('ANY')     -> any;
rewrite(any)       -> any;
rewrite(dot)       -> ?dot;
rewrite('CDATA')   -> ?pcdata;
rewrite('#PCDATA') -> ?pcdata;
rewrite(pcdata)    -> ?pcdata;
rewrite('EMPTY')   -> ?epsilon;
rewrite(empty)     -> ?epsilon;
rewrite({closure,RE}) ->
    Occure = {0,unbounded},
    case rewrite(RE) of
	{element,E}      -> {element,Occure,E};
	{choice,E}       -> {choice,Occure,E};
	{sequence,E}     -> {sequence,Occure,E};
	{all,E}          -> {all,Occure,E};
	{action,A}       -> {action,A};
	?any             -> {?any,Occure};
	?dot             -> {?dot,Occure};
	?pcdata          -> {?pcdata,Occure};
	?epsilon         -> ?epsilon
    end;
rewrite({pclosure,RE}) ->
    Occure = {1,unbounded},
    case rewrite(RE) of
	{element,E}    -> {element,Occure,E};
	{choice,E}     -> {choice,Occure,E};
	{sequence,E}   -> {sequence,Occure,E};
	{all,E}        -> {all,Occure,E};
	{action,A}     -> {action,A};
	?any           -> {?any,Occure};
	?dot           -> {?dot,Occure};
	?pcdata        -> {?pcdata,Occure};
	?epsilon       -> ?epsilon
    end;
rewrite({optional,RE}) ->
    Occure = {0,1},
    case rewrite(RE) of
	{element,E}    -> {element,Occure,E};
	{choice,E}     -> {choice,Occure,E};
	{sequence,E}   -> {sequence,Occure,E};
	{all,E}        -> {all,Occure,E};
	{action,A}     -> {action,A};
	?any           -> {?any,Occure};
	?dot           -> {?dot,Occure};
	?pcdata        -> {?pcdata,Occure};
 	?epsilon       -> ?epsilon
    end;
rewrite({sequence,REs}) ->
    case lists:map(fun(RE) -> rewrite(RE) end, REs) of
	[] -> empty;
	[RE] -> RE;
	RE1 -> {sequence,RE1}
    end;
rewrite({choice,REs}) ->
    case lists:map(fun(RE) -> rewrite(RE) end, REs) of
	[] -> empty;
	[RE] -> RE;
	REs1 -> {choice,REs1}
    end;
rewrite({all,REs}) ->
    case lists:map(fun(RE) -> rewrite(RE) end, REs) of
	[] -> empty;
	[RE] -> RE;
	REs1 -> {all,REs1}
    end;
rewrite({'+',RE}) ->  rewrite({pclosure,RE});
rewrite({'*',RE}) ->  rewrite({closure,RE});
rewrite({'?',RE}) ->  rewrite({optional,RE});
rewrite({'&',REs}) -> rewrite({all,REs});
rewrite({'|',REs}) -> rewrite({choice,REs});
rewrite({',',REs}) -> rewrite({sequence,REs});
rewrite(Element) when ?is_tag(Element) -> {element,Element};
rewrite(Char) when is_integer(Char) -> {char,Char};
rewrite(E) -> E.

%%
%% Translate rewritten expressions into internal form
%%
%% Expand: {all,[a,b]} => (a,b) | (b,a)
%% Expand: {x,{1,2},E} => E | EE
%%
%%expand(any)         -> expand({any,{0,unbounded}});
expand(?any)        -> expand({?dot,{0,unbounded}});
expand(?dot)        -> ?dot;
expand(?pcdata)     -> expand({?pcdata,{0,unbounded}});
expand(?epsilon)    -> ?epsilon;
expand({element,E}) -> {element,E};
expand({char,C})    -> {char,C};
expand({action,As}) -> {action,As};
expand({choice,Cs}) -> 
    case lists:map(fun(C) -> expand(C) end, Cs) of
	[] -> empty;
	[C] -> C;
	Cs1 -> {choice,Cs1}
    end;
expand({sequence,Cs}) -> 
    case lists:map(fun(C) -> expand(C) end, Cs) of
	[] -> empty;
	[C] -> C;
	Cs1 -> {sequence,Cs1}
    end;
expand({all,Cs}) ->
    case fold_permutations(fun(Cs1, Acc)->[{sequence,Cs1}|Acc] end,[],Cs) of
	[]  -> empty;
	[C] -> expand(C);
	Cs1 -> expand({choice,Cs1})
    end;
expand({all,{N,M},Cs}) ->
    case expand({all,Cs}) of
	?epsilon -> ?epsilon;
	{sequence,Cs1} -> expand({sequence,{N,M},Cs1});
	{choice,Cs1} -> expand({choice,{N,M},Cs1})
    end;
expand({F,{N,M},Cs}) when F==choice; F==sequence ->
    E = {F,lists:map(fun(C) -> expand(C) end, Cs)},
    expand(N, M,E);
expand({F,{N,M},E}) when F==element; F==char ->
    expand(N, M,{F,E});
expand({F,{N,M}}) when F==?dot; F==?any; F==?pcdata ->
    expand(N, M, F).

expand(N,unbounded,E) when is_integer(N), N>=0 ->
    if N == 0 -> {closure,E};
       true ->   {sequence,lists:duplicate(N,E)++[{closure,E}]}
    end;
expand(N,M,E) when is_integer(N), N>=0, is_integer(M), N=<M ->
    {choice, lists:map(fun (0) -> empty;
		     (1) -> E;
		     (K) -> {sequence,lists:duplicate(K,E)} end,
		 lists:seq(N,M))}.

%%
%% DFA from RE expression (NOTE! not working yet)
%%
make_dfa_new(RE) ->
    RE1 = {sequence,[translate(RE),{element,'#'}]},
    io:format("make_dfa_new: ~s\n", [format(RE1)]),
    {CRE, _,Position0} = cre(RE1, 1),
    Follow = lists:keysort(1, follow_cre(CRE,[])),
    Position = list_to_tuple(lists:map(fun({_I,Sym}) -> Sym end, Position0)),
    Symbols = symbols(RE1),
    io:format("position=~p\n", [Position]),
    io:format("symbols=~p\n", [Symbols]),
    io:format("follow=~p\n", [Follow]),
    St0 = CRE#cre.firstpos,
    io:format("St0=~p\n", [St0]),
    DTran = dtran([St0], Follow, Position, Symbols),
    %% create a grouped dfa
    DFA1 = group_dfa(DTran, St0, St0, [], [], size(Position)),
    remap_fa(DFA1).

group_dfa([#fa_edge {id={St1,Sym},target=St2}|Dtran],St1,St0,Tr, DFA, Final) ->
    Tr1 = if St2 == [] -> Tr;
	     true -> [#fa_edge{id=Sym,target=St2,data=St1}|Tr]
	  end,
    group_dfa(Dtran,St1,St0,Tr1,DFA,Final);
group_dfa([#fa_edge {id={St1,Sym},target=St2}|Dtran],St3,St0,Tr,DFA,Final) ->
    Tr0 = if St2 == [] -> [];
	     true -> [#fa_edge{id=Sym,target=St2,data=St2}]
	  end,
    group_dfa(Dtran, St1, St0, Tr0,
	      [#fa_state { id=St3,
			   edges=Tr,
			   accept=lists:member(Final, St3)}|DFA],
	      Final);
group_dfa([], St1, St0, Tr, DFA, Final) ->
    Accept = lists:member(Final, St1),
    #fa { type = dfa,
	  init = St0,
	  states = [#fa_state { id=St1, edges=Tr, accept=Accept}|DFA]
	 }.
    
dtran(Us, Follow, Positions, Symbols) ->
    dtran(Us,Symbols, [],Follow,Positions,[]).

dtran([T|Us],Symbols, Ms, Follow,Positions,Dtran) ->
    dtran(Symbols,Symbols,T,Us,[T|Ms],Follow,Positions,Dtran);
dtran([], _Symbols, _Ms, _Follow, _Posisions, Dtran) ->
    lists:reverse(Dtran).


dtran([A|As],Symbols,T, Us, Ms, Follow,Positions,Dtran) ->
    U = 
	lists:foldl(
	  fun(P,U0) ->
		  case element(P, Positions) of
		      {element,A} -> 
			  orders:union(U0,followpos(Follow,P));
		      {char,A} -> 
			  orders:union(U0,followpos(Follow,P));
		      ?dot ->
			  orders:union(U0,followpos(Follow,P));
		      ?any ->
			  orders:union(U0,followpos(Follow,P));
		      _ ->
			  U0
		  end
	  end, ordsets:new(), ordsets:to_list(T)),
    Dtran1 = [#fa_edge { id={T,A}, target=U} | Dtran],
    if U == [] ->
	    dtran(As, Symbols, T, Us, Ms, Follow,Positions,Dtran1);
       true ->
	    case lists:member(U, Us) orelse lists:member(U, Ms) of
		true ->
		    dtran(As, Symbols,T,Us,Ms,Follow,Positions,Dtran1);
		false ->
		    dtran(As, Symbols,T,[U|Us],Ms,Follow,Positions,Dtran1)
	    end
    end;
dtran([], Symbols, _T, Us, Ms, Follow,Positions,Dtran) ->
    dtran(Us, Symbols, Ms, Follow,Positions, Dtran).


follow_cre(CRE, F0) ->
    case CRE#cre.operator of
	choice -> 
	    lists:foldl(fun(CRE1,F) -> follow_cre(CRE1,F) end, F0, CRE#cre.operand);
	sequence -> 
	    follow_cat(CRE#cre.operand, F0);
	closure -> 
	    F1 = follow_cre(CRE#cre.operand,F0),
	    lists:foldl(fun(I,F) ->
			  update_followpos(F, I, CRE#cre.firstpos)
		  end, F1, CRE#cre.lastpos);
	leaf ->
	    case CRE#cre.firstpos of
		[I] ->
		    update_followpos(F0, I, ordsets:new());
		[] ->
		    F0
	    end
    end.
	    
follow_cat([A], F0) ->
    follow_cre(A, F0);
follow_cat([A,B|As], F0) ->
    F1 = follow_cre(A, F0),
    F2 = follow_cat([B|As],F1),
    lists:foldl(fun(I, F) ->
		  update_followpos(F, I, B#cre.firstpos)
	  end, F2, A#cre.lastpos).

followpos(F, I) ->
    case lists:keysearch(I, 1, F) of
	{value,{_,Follow}} -> 
	    Follow;
	_ ->
	    undefined
    end.

update_followpos(F, I, Set) ->
    case followpos(F, I) of
	undefined ->
	    [{I,Set} | F];
	Follow ->
	    lists:keyreplace(I, 1, F, {I,ordsets:union(Set, Follow)})
    end.

    
%% #PCDATA and CDATA must have been expanded before
cre(empty,N) ->
    {#cre { operator = leaf, 
	    operand = empty,
	    firstpos = [N],
	    lastpos = [N],
	    nullable = true },N,[]};
cre(any,N) ->
    {#cre { operator = leaf,
	    operand = any,
	    firstpos = [N],
	    lastpos = [N],
	    nullable = true }, N+1,[{N,any}]};
cre(dot,N) ->
    {#cre { operator = leaf,
	    operand =  dot,
	    firstpos = [N],
	    lastpos = [N],
	    nullable = false }, N+1,[{N,dot}]};
cre({element,E},N) ->
    {#cre { operator = leaf,
	    operand = E,
	    firstpos = [N],
	    lastpos = [N],
	    nullable = false }, N+1,[{N,{element,E}}]};
cre({char,C},N) ->
    {#cre { operator = leaf,
	    operand = C,
	    firstpos = [N],
	    lastpos = [N],
	    nullable = false }, N+1,[{N,{char,C}}]};
cre({action,Action},N) ->
    {#cre { operator = leaf,
	    operand = {'!',Action},
	    firstpos = [N],
	    lastpos = [N],
	    nullable = false }, N+1,[{N,{'!',Action}}]};

cre({choice,REs},N) ->
    {CREs,CN,Pos} = cre_list(REs, N),
    Nullable = lists:any(fun(CRE) -> CRE#cre.nullable end, CREs),
    FirstPos = lists:foldl(fun(CRE,Pos1) ->
			     ordsets:union(Pos1,CRE#cre.firstpos)
		     end, ordsets:new(), CREs),
    LastPos = lists:foldl(fun(CRE,Pos1) ->
			    ordsets:union(Pos1,CRE#cre.lastpos)
		    end, ordsets:new(), CREs),
    { #cre { operator = choice,
	     operand  = CREs,
	     firstpos = FirstPos,
	     lastpos  = LastPos,
	     nullable = Nullable }, CN, Pos};

cre({sequence,REs},N) ->
    {CREs,CN,Pos} = cre_list(REs, N),
    Nullable = lists:all(fun(CRE) -> CRE#cre.nullable end, CREs),
    %% Note buildin from right to left!
    FirstPos = lists:foldr(fun(CRE,Pos1) ->
			     case CRE#cre.nullable of
				 true ->
				     ordsets:union(Pos1,CRE#cre.firstpos);
				 false ->
				     CRE#cre.firstpos
			     end
		     end, ordsets:new(), CREs),
    %% Note buildin from left to right!
    LastPos = lists:foldl(fun(CRE,Pos1) ->
			    case CRE#cre.nullable of
				true ->
				    ordsets:union(Pos1,CRE#cre.lastpos);
				false ->
				    CRE#cre.lastpos
			    end
		    end, ordsets:new(), CREs),
    { #cre { operator = sequence,
	     operand  = CREs,
	     firstpos = FirstPos,
	     lastpos  = LastPos,
	     nullable = Nullable }, CN, Pos};

cre({closure,RE},N) ->
    {CRE,CN,Pos} = cre(RE,N),
    { #cre { operator = closure,
	     operand = CRE,
	     firstpos = CRE#cre.firstpos,
	     lastpos = CRE#cre.lastpos,
	     nullable = true }, CN, Pos}.


cre_list(REs, N) ->
    cre_list(REs, N, [], []).
    
cre_list([RE|REs], N, CREs, Pos0) ->
    {CRE,N1,Pos1} = cre(RE, N),
    cre_list(REs, N1, [CRE|CREs],Pos0++Pos1);
cre_list([], N, CREs, Pos) ->
    {lists:reverse(CREs), N, Pos}.

%%
%% Generate DFA for DTD rule
%%
%% Symbols must be a list of uniq atoms
%%
make_dfa(RE) ->
    make_dfa(RE, []).

make_dfa(RE, Action) ->
    RE1 = if Action == [] -> RE;
	     true -> {sequence,[RE,{'!',Action}]}
	  end,
    RE2 = translate(RE1),
    ?dbg("make_dfa: ~s\n", [format(RE2)]),
    Symbols =  symbols(RE2),
    ?dbg("symbols=~p\n", [Symbols]),
    build_dfa(RE2, Symbols).
    

build_dfa(RE, Symbols) ->
    NFA = build_nfa(RE, 1),
    ?dbg("NFA contains ~w states,i=~p\n~p\n", 
	 [length(NFA#fa.states), NFA#fa.init,NFA#fa.states]),
    NFA_Array = list_to_tuple(lists:keysort(#fa_state.id, NFA#fa.states)),
    DFA = build_dfa(NFA_Array, NFA#fa.init, Symbols),
    ?dbg("DFA contains ~w states i=~p\n~p\n", 
	 [length(DFA#fa.states),DFA#fa.init,DFA#fa.states]),
    DFA1 = minimise_dfa(DFA),
    ?dbg("minimised to ~w states i=~p\n~p\n", 
	 [length(DFA1#fa.states),DFA1#fa.init,DFA1#fa.states]),
    DFA1.

symbols(RE) ->
    ordsets:to_list(symbols(RE, ordsets:new())).

symbols({choice,REs}, Symbols) ->
    lists:foldl(fun(RE,S) -> symbols(RE,S) end, Symbols, REs);
symbols({all,REs}, Symbols) ->
    lists:foldl(fun(RE,S) -> symbols(RE,S) end, Symbols, REs);
symbols({sequence,REs}, Symbols) ->
    lists:foldl(fun(RE,S) -> symbols(RE,S) end, Symbols, REs);
symbols({closure,RE}, Symbols) ->
    symbols(RE,Symbols);
symbols({pclosure,RE}, Symbols) ->
    symbols(RE,Symbols);
symbols({optional,RE}, Symbols) ->
    symbols(RE,Symbols);
symbols(?epsilon, Symbols) ->  Symbols;
symbols(?any, Symbols) ->    Symbols;
symbols(?dot, Symbols) ->
    ordsets:add_element(?dot,Symbols); %% Is this it?
symbols({action,Action},Symbols) -> 
    ordsets:add_element({action,Action}, Symbols);
symbols(?pcdata, Symbols) ->
    ordsets:add_element(?pcdata, Symbols);
symbols({element,E}, Symbols) ->
    ordsets:add_element(E, Symbols);
symbols({char,C}, Symbols) ->
    ordsets:add_element(C, Symbols).

%% Create and NFA from a RegExp
make_nfa(RE) ->
    make_nfa(RE, []).

make_nfa(RE,Action) ->
    RE1 = if Action == [] -> RE;
	     true -> {sequence,[RE,{'!',Action}]}
	  end,
    RE2 = translate(RE1),
    build_nfa(RE2, 1).

%% {NFA,NextFreeState,FirstState} = build_nfa(RegExp, FreeState, Action)
%%  When building the NFA states for a ??? we don't build the end
%%  state, just allocate a State for it and return this state
%%  number. This allows us to avoid building unnecessary states for
%%  concatenation which would then have to be removed by overwriting
%%  an existing state.

build_nfa(RE, N0) ->
    {States,_N1,Es} = build_nfa(RE, N0+1, N0, []),
    #fa { type = nfa,
	  init = N0,
	  states = [#fa_state{id=Es,accept=true}|States] }.


%% build_nfa(RegExp, NextState, FirstState, NFA) -> {NFA,NextState,EndState}.
%%  The NFA is a list of fa_state is no predefined order. The state
%%  number of the returned EndState is already allocated!

build_nfa(RE, N0, Fs, NFA0) ->
    case RE of
	{choice,REs} ->
	    build_or_nfa(REs, N0, Fs, NFA0);
	{all,REs} ->
	    build_all_nfa(REs, N0, Fs, NFA0);
	{sequence,REs} ->
	    build_concat_nfa(REs, N0, Fs, NFA0);
	{closure,RE1} ->
	    {NFA1,N1,Es1} = build_nfa(RE1, N0+1, N0, NFA0),
	    Es = N1,
	    {[#fa_state{id=Fs,
			edges=[#fa_edge{id=?epsilon,target=N0},
			       #fa_edge{id=?epsilon,target=Es}]},
	      #fa_state{id=Es1,
			edges=[#fa_edge{id=?epsilon,target=N0},
			       #fa_edge{id=?epsilon,target=Es}]}|
	      NFA1],
	     N1+1,Es};
	?epsilon ->
	    {[#fa_state{id=Fs,edges=
			[#fa_edge{id=?epsilon,target=N0}]}|NFA0], 
	     N0+1, N0};
	?any ->
	    {[#fa_state{id=Fs,edges=
			[#fa_edge{id=?any,target=N0},
			 #fa_edge{id=?epsilon,target=N0}]}|
	      NFA0],
	     N0+1,N0};
	?dot ->
	    {[#fa_state{id=Fs,edges=
			[#fa_edge{id=?dot,target=N0}]}|
	      NFA0],
	     N0+1,N0};
	?pcdata ->
	    {[#fa_state{id=Fs,edges=
			[#fa_edge{id=?pcdata,target=N0},
			 #fa_edge{id=?epsilon,target=N0}]}|
	      NFA0],
	     N0+1,N0};
	{action,Action} ->
	    {[#fa_state{id=Fs,edges=
			[#fa_edge{id={'!',Action},target=N0}]}
	      |NFA0],N0+1,N0};
	{char,Char} ->
	    {[#fa_state{id=Fs,edges=
			[#fa_edge{id={char,Char},target=N0}]}|NFA0],
	     N0+1,N0};
	{element,E} ->
	    {[#fa_state{id=Fs,edges=
			[#fa_edge{id={element,E},target=N0}]}|NFA0],
	     N0+1,N0}
    end.


%% r1 r2 .. rn
build_concat_nfa([RE|REs], N0, Fs, NFA0) ->
    {NFA1,N1,Es1} = build_nfa(RE,N0,Fs,NFA0),
    build_concat_nfa(REs,N1,Es1,NFA1);
build_concat_nfa([], N0, Fs, NFA0) ->
    {NFA0,N0,Fs}.

%% r1|r2|..|rn
build_or_nfa([], N0, Fs, NFA0) ->
    {NFA0,N0,Fs};
build_or_nfa(REs, N0, Fs, NFA0) ->
    build_or_nfa(REs, N0, Fs, NFA0, [], []).
    
build_or_nfa([RE|REs], N0, Fs, NFA0, FsList, EsList) ->
    {NFA1,N1,Es1} = build_nfa(RE, N0+1, N0, NFA0),
    build_or_nfa(REs, N1, Fs, NFA1, [N0|FsList], [Es1|EsList]);
build_or_nfa([], Es, Fs, NFA0, FsList, EsList) ->
    Es1 = lists:map(fun(Fi) -> 
			    #fa_edge{id=?epsilon,target=Fi} 
		    end,
		    FsList),
    {[#fa_state { id=Fs, edges=Es1} |
      lists:map(fun(Ei) ->
			#fa_state { id=Ei, edges=
					[#fa_edge{id=?epsilon,target=Es}]}
		end, EsList)]++NFA0,
     Es+1, Es}.
		 
%% r1&r2&..&rn (any order)
%% (r1 r2 .. ) | (r2 r1 ..) ....
%%
build_all_nfa(REs, N0, Fs, NFA0) ->
    RePerm = 
	fold_permutations(fun(REs1, Acc) -> [{sequence,REs1}|Acc] end, [], REs),
    build_or_nfa(RePerm, N0, Fs, NFA0).


%% build_dfa(NFA_Array, NfaFirstState, Symbols) -> DFA
%%  Build a DFA from an NFA using "subset construction". The major
%%  difference from the book is that we keep the marked and unmarked
%%  DFA states in seperate lists. New DFA states are added to the
%%  unmarked list and states are marked by moving them to the marked
%%  list. We assume that the NFA accepting state numbers are in
%%  ascending order for the rules and use ordsets to keep this order.

build_dfa(NFA_Array, Nf, Symbols) ->
    Ec = eclosure([Nf], NFA_Array),
    D = #fa_state{id=1},
    #fa { type=dfa,
	  init=1,
	  states=build_dfa([{Ec,D}], 2, [], NFA_Array, Symbols)
	 }.

%% build_dfa([UnMarked], NextState, [Marked], NFA) -> DFA.
%%  Traverse the unmarked states. Temporarily add the current unmarked
%%  state to the marked list before calculating translation, this is
%%  to avoid adding too many duplicate states. Add it properly to the
%%  marked list afterwards with correct translations.

build_dfa([U0={Uk,U}|Us0], N0, Ms, NFA_Array, Symbols) ->
    {Es,Us1,N1} = build_dfa(Symbols,Uk,Us0,N0,[],[U0|Ms],NFA_Array),
    ?dbg("build_nfa: n1=~p, length=~p\n", [N1,length(Us0)]),
    M = U#fa_state{edges=Es,
		   accept=accept(Uk, NFA_Array)
		  },
    build_dfa(Us1, N1, [{Uk,M}|Ms], NFA_Array, Symbols);
build_dfa([], _N, Ms, _NFA_Array, _) -> 
    lists:map(fun({_,M}) -> M end, Ms).

%% build_dfa(Symbols, [NfaState], [Unmarked], NextState, [Transition],
%%           [Marked], NFA) ->
%%	[Marked].
%%  Foreach NFA state set calculate the legal translations. N.B. must
%%  search *BOTH* the unmarked and marked lists to check if DFA state
%%  already exists. By test characters downwards and prepending
%%  transitions we get the transition lists in ascending order.

build_dfa([Sym|Symbols], Set, Us, N, Es, Ms, NFA_Array) ->
    ?dbg("build_dfa: Sym: ~p\n", [Sym]),
    case eclosure(move(Set, Sym, NFA_Array), NFA_Array) of
	S when S /= [] ->
	    case lists:keysearch(S, 1, Us) of
		{value,{_,#fa_state{id=T}}} ->
		    build_dfa(Symbols, Set, Us, N,
			      [#fa_edge{id=Sym,target=T}|Es], Ms, 
			      NFA_Array);
		false ->
		    case lists:keysearch(S, 1, Ms) of
			{value,{_,#fa_state{id=T}}} ->
			    build_dfa(Symbols,Set,Us,N,
				      [#fa_edge{id=Sym,target=T}|Es],Ms,
				      NFA_Array);
			false ->
			    U = #fa_state{id=N},
			    build_dfa(Symbols,Set,[{S,U}|Us],N+1,
				      [#fa_edge{id=Sym,target=N}|Es],Ms,
				      NFA_Array)
		    end
	    end;
	[] ->
	    build_dfa(Symbols, Set, Us, N, Es, Ms, NFA_Array)
    end;
build_dfa([], _Set, Us, N, Es, _Ms, _NFA) ->
    {Es,Us,N}.

%% eclosure([State], NFA) -> [State]
%% move([State], Sym, NFA) -> [State].
%%  These are straight out of the book. As eclosure uses ordsets then
%%  the generated state sets are in ascending order.

eclosure(Sts, NFA) -> 
    eclosure(Sts, NFA, []).

eclosure([St|Sts], NFA, Ec) ->
    #fa_state{edges=Es} = element(St, NFA),
    eclosure([ N || #fa_edge{id=?epsilon,target=N} <- Es,
		    not ordsets:is_element(N, Ec) ] ++ Sts,
	     NFA, ordsets:add_element(St, Ec));
eclosure([], _NFA, Ec)-> Ec.

%% Given a list of states and a symbol C,
%% calculate the list of end states and actions
move(Sts, C, NFA) ->
    [St || 
	N <- Sts, 
	#fa_edge{id=C1,target=St} <- (element(N, NFA))#fa_state.edges,
	match_id(C,C1)].

match_id(_C, ?any)       -> true;
match_id(_C, ?dot)       -> true;
match_id(_C, ?epsilon)   -> false;
match_id('#PCDATA', ?pcdata) -> true;
match_id(pcdata, ?pcdata) -> true;
match_id(C, {element,C}) -> true;
match_id(C, {char,C})    -> true;
match_id(_, _)           -> false.

%% accept([State], NFA) -> true | false
%%  Return a list of accept state actions
%%  CHANGE: collect all accepting actions
%%           [] -> no accept
%%           [...|accept] -> accept and permform actions
%%
accept([St|Sts], NFA) ->
    case element(St, NFA) of
	#fa_state{accept=true} -> true;
	#fa_state{accept=false} -> accept(Sts,NFA)
    end;
accept([], _NFA) -> false.

%%
%% Build an action FA
%%  1. transform 
%%         edge on form id={'!',Action}, target=Sj
%%                   to id='!',action=Action,target=Sj
%%
afa_from_dfa(DFA) ->
    Es = lists:flatmap(
	   fun(S) ->
		   Si = S#fa_state.id,
		   lists:map(
		     fun(E = #fa_edge { id={'!',Action}}) ->
			     E#fa_edge { source=Si, id='!', action=Action};
			(E) ->
			     E#fa_edge { source=Si }
		     end, S#fa_state.edges)
	   end, DFA#fa.states),
    %% Require sorted DFA states 1...N
    INFO1 = init_afa_info(DFA#fa.states, Es),
    io:format("INFO1 = ~p\n", [INFO1]),
    {Es1,INFO2} = afa_merge(Es,INFO1),
    io:format("INFO2 = ~p\n", [INFO2]),
    io:format("Es1 = ~p\n", [Es1]),
    %% reconstruct the DFA again
    GEs3 = key_group(Es1, #fa_edge.source),
    States =
	lists:foldl(
	  fun(S,SAcc) ->
		  Sj = S#fa_state.id,
		  case element(Sj,INFO2) of
		      {0,0,_} -> SAcc;
		      {_In,_Out,Accept} ->
			  case lists:keysearch(Sj,1,GEs3) of
			      false ->
				  [S#fa_state { edges=[],accept=Accept} | SAcc];
			      {value,{_,SEs}} ->
				  [S#fa_state { edges=SEs,accept=Accept} | SAcc]
			  end
		  end
	  end, [], DFA#fa.states),
    DFA#fa { states = States }.

	      
%%
%% Merge action edges:
%%
%% A Rule:
%%   A -> ?!x -> B,  B -> _!y -> C
%%      =>  A -> ?!xy  -> C  
%%
afa_merge(Es0, I0) ->
    {Es1,I1} = afa_merge_a(Es0, I0, []),
    {Es2,I2} = afa_merge_b(Es1, I1, []),
    {Es2,I2}.
    
afa_merge_a([E=#fa_edge { source=_A,target=B,action=EAs}|Es1],I0,Es2) ->
    {Bin,Bout,_Baccept} = element(B, I0),
    if
	Bout==1 ->
	    %% Bypass B and replace it with a new straight edge
	    [F=#fa_edge{id=Fid,action=FAs,target=C}] = out_edges(B, Es1, Es2),
	    if Fid == '!' ->
		    if Bin == 1 ->
			    I1 = del_edge_info(E, I0),
			    I2 = del_edge_info(F, I1),
			    G = E#fa_edge { action=EAs++FAs,target=C},
			    I3 = add_edge_info(G, I2),
			    io:format("RULE a: ~s[DEL] , ~s[DEL]  => ~s\n",
				      [fmt_edge(E),fmt_edge(F),fmt_edge(G)]),
			    afa_merge_a(((Es1++Es2)--[F])++[G],I3,[]);
		       true ->
			    I1 = I0,
			    I2 = del_edge_info(E, I1),
			    G = E#fa_edge { action=EAs++FAs,target=C},
			    I3 = add_edge_info(G, I2),
			    io:format("RULE a: ~s[DEL], ~s  => ~s\n",
				      [fmt_edge(E),fmt_edge(F),fmt_edge(G)]),
			    afa_merge_a((Es1++Es2)++[G],I3,[])
		    end;
	       true ->
		    afa_merge_a(Es1,I0,[E|Es2])
	    end;
       true ->
	    afa_merge_a(Es1,I0,[E|Es2])
    end;
afa_merge_a([],I0,Es2) ->
    {Es2, I0}.

%%
%% B Rule:
%%   A -> !y -> B,  B -> a!z -> Ci
%%                  B ->  !z -> Cj
%%
%%      =>  A -> a!save,y,restore,z -> Ci
%%          A ->  !yz -> Cj
%%
%% Exception Ain == 0  (for initial state etc)
%%

afa_merge_b([E=#fa_edge { id='!',source=A,target=B,action=EAs}|Es1],I0,Es2) ->
    {Ain,_Aout,_Aaccept} = element(A,I0),
    if Ain == 0 ->
	    io:format("RULE b0: ~s [KEEP]\n", [fmt_edge(E)]),
	    afa_merge_b(Es1,I0,[E|Es2]);
       true ->
	    Fs0 = out_edges(B, Es1, Es2),
	    {Gs,Fs,I1} =
		lists:foldl(
		  fun(F=#fa_edge{id=Fid,action=FAs,target=C},{Gs,Fs,I1}) ->
			  GAs = if Fid == '!' -> EAs ++ FAs;
				   true -> [save|EAs]++[restore|FAs]
				end,
			  G = F#fa_edge { source=A,target=C,action=GAs},
			  case element(B, I1) of
			      {1,_,_} -> %% E is the only input edge
				  io:format("RULE b1: ~s , ~s[DEL]  => ~s\n",
					    [fmt_edge(E),
					     fmt_edge(F),
					     fmt_edge(G)]),
				  I2 = add_edge_info(G, I1),
				  I3 = del_edge_info(F, I2),
				  {[G|Gs],[F|Fs],I3};
			      {_,_,_} ->
				  io:format("RULE b1: ~s , ~s  => ~s\n",
					    [fmt_edge(E),
					     fmt_edge(F),
					     fmt_edge(G)]),
				  I2 = add_edge_info(G, I1),
				  {[G|Gs],Fs,I2}
			  end
		  end,{[],[],I0},Fs0),
	    if length(Fs0) == 0 ->
		    afa_merge_b(Es1,I1,[E|Es2]);
	       true ->
		    io:format("RULE b2: ~s[DEL]\n",[fmt_edge(E)]),
		    I2 = del_edge_info(E, I1),
		    afa_merge_b(((Es1++Es2)--Fs)++Gs,I2,[])
	    end
    end;
afa_merge_b([E|Es1],I0,Es2) ->
    afa_merge_b(Es1,I0,[E|Es2]);
afa_merge_b([],I0,Es2) ->
    {Es2, I0}.

fmt_edge(#fa_edge{id=ID, action=As,source=S, target=T}) ->
    io_lib:format("(~w -> ~s~w -> ~w)", [S,ID,As,T]).

%% Create degree info
init_afa_info(States, Es) ->
    INFO0 = list_to_tuple(lists:map(fun(S) -> {0,0,S#fa_state.accept} end, States)),
    update_afa_info(Es, INFO0).

%% Update degree info for all edges
update_afa_info([E|Es], I0) ->
    update_afa_info(Es, add_edge_info(E,I0));
update_afa_info([], I0) ->
    I0.

add_edge_info(#fa_edge { source=A, target=A }, I0) ->
    {Ain,Aout,Aaccept} = element(A, I0),
    setelement(A,I0,{Ain+1,Aout+1,Aaccept});
add_edge_info(#fa_edge { source=A, target=B }, I0) ->
    {Ain,Aout,Aaccept} = element(A, I0),
    {Bin,Bout,Baccept} = element(B, I0),
    I1 = setelement(A,I0,{Ain,Aout+1,Aaccept}),
    setelement(B,I1,{Bin+1,Bout,Baccept}).

del_edge_info(#fa_edge { source=A, target=A }, I0) ->
    {Ain,Aout,Aaccept} = element(A, I0),
    setelement(A,I0,{Ain-1,Aout-1,Aaccept});
del_edge_info(#fa_edge { source=A, target=B }, I0) ->
    {Ain,Aout,Aaccept} = element(A, I0),
    {Bin,Bout,Baccept} = element(B, I0),
    I1 = setelement(A,I0,{Ain,Aout-1,Aaccept}),
    setelement(B,I1,{Bin-1,Bout,Baccept}).

%% in_edges(S, Es1, Es2) ->
%%    in_edges(S, Es1) ++ in_edges(S, Es2).

%% in_edges(S, Es) ->
%%    lists:foldl(fun(E,SEs) ->
%%		  if E#fa_edge.target == S -> [E|SEs];
%%		     true -> SEs
%%		  end
%%	  end, [], Es).

out_edges(S, Es1, Es2) ->
    out_edges(S, Es1) ++ out_edges(S, Es2).

out_edges(S, Es) ->
    lists:foldl(fun(E,SEs) ->
		  if E#fa_edge.source == S -> [E|SEs];
		     true -> SEs
		  end
	  end, [], Es).


%% Group edges given Source = Si
key_group(Edges, Key) ->
    case lists:keysort(Key, Edges) of
	Es = [E|_] ->
	    key_group(Es, element(Key,E), Key, [], []);
	[] ->
	    []
    end.

key_group([E|Es], Val, Key, ValEs, Acc) ->
    case element(Key,E) of
	Val ->
	    key_group(Es, Val, Key, [E|ValEs], Acc);
	NewVal ->
	    key_group(Es, NewVal, Key, [E], [{Val,ValEs}|Acc])
    end;
key_group([], Val, _Key, ValEs, Acc) ->
    [{Val,ValEs} | Acc].

	    



%% minimise_dfa(DFA) -> DFA'
%%
%%  Minimise the DFA by removing equivalent states. We consider a
%%  state if both the transitions and the their accept state is the
%%  same.  First repeatedly run throught the DFA state list removing
%%  equivalent states and updating remaining transitions with
%%  remaining equivalent state numbers. When no more reductions are
%%  possible then pack the remaining state numbers to get consecutive
%%  states.
%% 
minimise_dfa(#fa {type=dfa, init=Init, states=States}) ->
    minimise_dfa(States, Init).

minimise_dfa(States, Init) ->
    case min_dfa(States) of
	{States1,[]} ->
	    {States2,Map} = renumber_states(States1),
	    #fa {type=dfa,
		 init=remap_state(Init, Map),
		 states=remap_fa(States2, Map)};
	{States1,Map} ->
	    minimise_dfa(remap_fa(States1, Map), remap_state(Init, Map))
    end.

min_dfa(DFA) ->
    min_dfa(DFA, [], []).

min_dfa([D|DFA0], Rs0, MDFA) ->
    {DFA1,Rs1} = min_delete(DFA0, D#fa_state.edges, 
			    D#fa_state.accept,
			    D#fa_state.id, Rs0, []),
    min_dfa(DFA1, Rs1, [D|MDFA]);
min_dfa([], Rs, MDFA) -> {MDFA,Rs}.

min_delete([#fa_state{id=N,edges=T,accept=A}|DFA], T, A, NewN, Rs, MDFA) ->
    min_delete(DFA, T, A, NewN, [{N,NewN}|Rs], MDFA);
min_delete([D|DFA], T, A, NewN, Rs, MDFA) ->
    min_delete(DFA, T, A, NewN, Rs, [D|MDFA]);
min_delete([], _T, _A, _NewN, Rs, MDFA) -> {MDFA,Rs}.

%% renumber and remap all states to 1..N
remap_fa(FA) ->
    {States,Map}   = renumber_states(FA#fa.states),
    FA#fa { init   = remap_state(FA#fa.init, Map),
	    states = remap_fa(States, Map) }.
    
%% remap all edges for all states 
remap_fa(States, Map) ->
    lists:map(
      fun(D = #fa_state { edges = Edges}) ->
	      D#fa_state { edges = remap_edges(Edges, Map) }
      end, States).

%% Renumber the State's from 1..N
%% and return the list of states in that order
renumber_states(States) ->
    renumber_states(States, 1, [], []).

renumber_states([D|States], NewN, Map, PFA) ->
    renumber_states(States, NewN+1,[{D#fa_state.id,NewN}|Map],
		    [D#fa_state{id=NewN}|PFA]);
renumber_states([], _NewN, Map, PFA) -> 
    {lists:reverse(PFA),Map}.

remap_edges([E=#fa_edge{target=Sj}|Es], Map) ->
    [E#fa_edge { target=remap_state(Sj, Map) } | remap_edges(Es,Map)];
remap_edges([],_Map) ->
    [].

remap_state(Si, Map) ->
    case lists:keysearch(Si, 1, Map) of
	false -> Si;
	{value,{_,Sk}} -> Sk
    end.


