%%% File    : makeup_dtd.erl
%%% Author  : Tony Rogvall <tony@bulldog.synap.se>
%%% Description : DTD scanner/parser
%%% Created :  1 Feb 2005 by Tony Rogvall <tony@bulldog.synap.se>

-module(makeup_dtd).

-include("../include/makeup.hrl").
 
-import(lists, [member/2,reverse/1,seq/2,append/1]).
-import(lists, [foreach/2,map/2,foldl/3,foldr/3, filter/2]).
-import(lists, [keymember/3,keysearch/3,keysort/2]).
-import(ordsets, [new/0,is_element/2,add_element/2,union/2,subtract/2]).

-compile(export_all).
-export([expand_rule/3]).

%% makeup callback module functions
-export([init/1, final/3, 
	 text/3, tag_begin/3, tag_end/3,
	 attribute/4, attribute/5, processing/4,
	 declaration/3, dparam/4, dcomment/4, 
	 section/4, comment/3,
	 charref/3]).

-ifdef(debug).
-define(dbg(Fmt,As), io:format((Fmt),(As))).
-else.
-define(dbg(Fmt,As), ok).
-endif.


-record(cstate,
	{
	  opts,               %% stored options
	  icase,              %% ignore case
	  ns=false,           %% name space encode as {Ns,Tag}
	  astack = [],
	  error_list = [],
	  entity_list = [],
	  element_list = []
	 }).

file(File) ->
    file(File,[],undefined).
file(File, Opts) ->
    file(File,Opts,undefined).
file(File, Opts, Tab) ->
    case makeup:file(File,?MODULE,[{icase,true},
				   {space,normalize},
				   {charset,"iso-8859-1"},
				   {xopts,Opts}]) of
	{ok,{Elems,Ents}} ->
	    {ok, {mk_elems(Elems,Tab), mk_ents(Ents,Tab)}};
	Error ->
	    Error
    end.

string(Data) ->
    string(Data,[],undefined).
string(Data,Opts) ->
    string(Data,Opts,undefined).
string(Data,Opts,Tab) ->
    case makeup:string(Data,?MODULE,[{icase,true},
				     {space,normalize},
				     {charset,"iso-8859-1"},
				     {xopts,Opts}]) of
	{ok,{Elems,Ents}} ->
	    {ok, {mk_elems(Elems,Tab), mk_ents(Ents,Tab)}};
	Error ->
	    Error
    end.

%% Extract char entities
mk_ents(Ents,Tab) ->
    filter(
      fun(E) ->
	      if E#makeup_entity.type == cdata ->
		      insert_entry(Tab,{{charref,E#makeup_entity.name},
					E#makeup_entity.def}),
		      true;
		 true -> false
	      end
      end, Ents).

mk_elems(Es, Tab) ->
    map(fun(E) -> 
		insert_elem(Tab, E, Es),
		E
	end, Es).

%%
%% Load ets table with DTD defintion
%% 
%% FIXME: We should only load the first definition !!!
%%
insert_elem(undefined, _, _Es) ->
     ok;
insert_elem(Tab, E, Es) ->
    Name = E#makeup_element.name,
    ets:insert(Tab, {{rule,Name}, E#makeup_element.rule}),
    Rule = expand_rule(E#makeup_element.rule, Es, [Name]),
    DFA = makeup_re:make_dfa(Rule),
    ets:insert(Tab, {{state_init,Name}, DFA#fa.init}),
    foreach(
      fun(#fa_state {id=Si,edges=Edges,accept=Accept}) ->
	      foreach(
		fun (#fa_edge{id=Sym,target=Sj,action=[]}) ->
			ets:insert(Tab,{{state,Name,Si,Sym},Sj});
		    (#fa_edge{id=Sym,target=Sj,action=Action}) ->
			ets:insert(Tab,{{state,Name,Si,Sym},Sj}),
			ets:insert(Tab,{{action,Si,Sym,Sj},Action})
		end, Edges),
	      if Accept == true ->
		      ets:insert(Tab,{{state_accept,Name,Si},true});
		 Accept == false ->
		      ets:insert(Tab,{{state_accept,Name,Si},false})
	      end
      end, DFA#fa.states),

    if E#makeup_element.start == optional ->
	    ets:insert(Tab, {{start_tag_optional,E}, true});
       true -> ok
    end,
    if E#makeup_element.stop == optional ->
	    ets:insert(Tab, {{stop_tag_optional,E}, true});
       true -> ok
    end,
    if E#makeup_element.include =/= [] ->
	    ets:insert(Tab, {{include,Name}, E#makeup_element.include});
       true -> ok
    end,
    if E#makeup_element.exclude =/= [] ->    
	    ets:insert(Tab, {{exclude,Name}, E#makeup_element.exclude});
       true -> ok
    end,
    ets:insert(Tab, {{attributes,Name},
		     map(fun(A) -> A#makeup_attribute.name end,
			 E#makeup_element.attr)}),
    foreach(
      fun(A) ->
	      ets:insert(Tab,{{attribute,Name,A#makeup_attribute.name},
			      {A#makeup_attribute.type,
			       A#makeup_attribute.status,
			       A#makeup_attribute.value}})
      end, E#makeup_element.attr).


insert_entry(undefined, _Entry) ->
    ok;
insert_entry(Tab, Entry) ->
    ets:insert(Tab, Entry).

%% Expand rule by checking all element in the rule
%% and add the rule definition for elements with optional start tags
%% as   r -> e | f
%%      e -> a,b
%% => r -> (e | (a,b)) | f  (if e has optional start tag)
%%     

expand_rule(empty, _Es, _Vs)  -> empty;
expand_rule(pcdata,_Es, _Vs)  -> pcdata;
expand_rule(any,_Es, _Vs)     -> any;
expand_rule({element,En}, Es, Vs) ->
    case lists:keysearch(En, #makeup_element.name, Es) of
	false ->
	    {element,En};
	{value,E} ->
	    if E#makeup_element.start == optional ->
		    case member(En, Vs) of
			true ->
			    {element,En};
			false ->
			    Rx=expand_rule(E#makeup_element.rule, Es, [En|Vs]),
			    {choice,[{element,En},Rx]}
		    end;
	       true ->
		    {element,En}
	    end
    end;
expand_rule({pclosure,E}, Es,Vs) -> {pclosure,expand_rule(E, Es,Vs)};
expand_rule({closure,E}, Es,Vs) ->  {closure,expand_rule(E, Es,Vs)};
expand_rule({optional,E}, Es,Vs) -> {optional,expand_rule(E, Es,Vs)};
expand_rule({sequence,As},Es,Vs) ->
    {sequence, map(fun(A) -> expand_rule(A,Es,Vs) end, As)};
expand_rule({choice,As},Es,Vs) ->
    {choice, map(fun(A) -> expand_rule(A,Es,Vs) end, As)};
expand_rule({all,As},Es,Vs) ->
    {all, map(fun(A) -> expand_rule(A,Es,Vs) end, As)}.


init(Opts) ->
    XOpts = getopt(xopts, Opts, []),
    ?dbg("INIT: opts=~p, xopts=~p\n", [Opts,XOpts]),
    ICase = getopt(icase, XOpts, false),
    Ns    = getopt(ns, XOpts, false),
    Opts1 = XOpts ++ delopt(xopts, Opts),
    #cstate { icase = ICase, 
	      ns = Ns,
	      opts = Opts1 }.

final(_Buf,_Line,St) ->
    ?dbg("FINAL:~w: ~s\n", [_Line,_Buf]),
    St1 = cross_ref(St),
    case St1#cstate.error_list of
	[] ->
	    {ok,{St1#cstate.element_list,
		 St1#cstate.entity_list}};
	ErrList ->
	    {error, ErrList}
    end.

text(Data, Line, St) ->
    dtext(Data,Line,St).

tag_begin(_Tag, _Line, St) ->
    ?dbg("<~s>:~w\n", [_Tag,_Line]),
    St.

tag_end(_Tag, _Line, St) ->
    ?dbg("</~s>:~w\n", [_Tag,_Line]),
    St.

attribute(_Tag, Attr, _Line, St) ->
    ?dbg("ATTRIBUTE:~w ~s ~s\n", [_Line,_Tag,Attr]),
    As = [Attr|St#cstate.astack],
    St#cstate { astack = As }.

attribute(_Tag, Attr, Value, _Line, St) ->
    ?dbg("ATTRIBUTE:~w ~s ~s=~s\n",
	 [_Line,_Tag,Attr,Value]),
    As = [{Attr,Value}|St#cstate.astack],
    St#cstate { astack = As }.

processing(xml, Value, _Line, St) ->
    KeyVal = makeup:keyval(Value),
    case lists:keysearch(encoding,1,KeyVal) of
	false -> St#cstate { astack = []};
	{value,{_,Charset}} ->
	    %% Extension?
	    ?dbg("Charset: ~p\n", [Charset]),
	    case makeup_charset:from_mime_name(Charset) of
		0 ->
		    io:format("Warning: unknown charset: ~s\n",[Charset]),
		    St#cstate { astack = []};
		N ->
		    Charset1 = makeup_charset:to_mime_name(N),
		    St1 = makeup:ioptions([{charset,Charset1}],St),
		    St1#cstate { astack = [] }
	    end
    end;
processing(_PI,_Value,_Line,St) ->
    ?dbg("PROCESSING::~w ~s\n", [_Line,_PI]),
    St#cstate { astack = [] }.

%% <!ENTITY parm*>
declaration('ENTITY', _Line, St) ->
    Ts = tokens(cat(reverse(St#cstate.astack), " ")),
    ?dbg("ENTITY:~w ~p\n", [_Line,Ts]),
    E=case Ts of
	  [{word,"%"}, {word,Name},{word,"PUBLIC"},
	   {string,PUBID}] ->
	      #makeup_entity { name = Name,
			       type = public,
			       def  = {PUBID,undefined} };
	  [{word,"%"}, {word,Name},{word,"PUBLIC"},{string,PUBID},{string,Url}] ->
	      #makeup_entity { name = Name,
			       type = public,
			       def  = {PUBID,Url} };
	  [{word,"%"}, {word,Name},{word,"SYSTEM"},{string,File}] ->
	      #makeup_entity { name = Name,
			       type = system,
			       def  = File };
	  [{word,"%"}, {word,Name},{string,Def}] ->
	      #makeup_entity { name = Name,
			       type = inline,
			       def  = Def };
	  [{word,Name},{word,"CDATA"},{string,Def}] ->
	      #makeup_entity { name = Name,
			       type = cdata,
			       def  = Def };
	  [{word,Name},{string,Def}] ->
	      #makeup_entity { name = Name,
			       type = cdata,
			       def  = Def }
      end,
    %% Only the first definition is used
    Es = St#cstate.entity_list,
    case lists:keysearch(Name, #makeup_entity.name, Es) of
	false ->
	    St#cstate { entity_list = [E|Es], astack = []};
	{value,_} ->
	    %% debug only since this is defined behaviour
	    ?dbg("entity ~s already defined\n", [Name]),
	    St#cstate { astack = [] }
    end;
declaration('ELEMENT', Line, CSt) ->
    Data = cat(reverse(CSt#cstate.astack), " "),
    ?dbg("ELEMENT:~w ~s\n", [Line,Data]),
    Ts0 = tokens(Data),
    case catch expand_tokens(Ts0, CSt#cstate.entity_list) of
	{error, Reason} ->
	    error(Line, Reason, CSt);
	Ts ->
	    ?dbg("ELEMENT tokens: ~p\n", [Ts]),
	    case idlist(Ts, CSt) of
		{error,_} ->
		    error(Line, {syntax_error, Data}, CSt);
		{IDList,Ts1} ->
		    ?dbg("ELEMENT idlist: ~p tokens: ~p\n", [IDList,Ts1]),
		    case catch parse_rule(Ts1,CSt) of
			{'EXIT',_Reason} ->
			    ?dbg("Crash: reason ~p\n", [_Reason]),
			    error(Line, {syntax_error,Data}, CSt);
			Error={syntax_error,_} ->
			    error(Line, Error, CSt);
			Error={error,_} ->
			    error(Line, Error, CSt);
			{Start,Stop,{Expr,Exception}} ->
			    {Extension,Restriction}=split_exception(Exception),
			    foldl(
			      fun(ID,CSt1) ->
				      add_element(Line,ID,Start,Stop,
						  Expr,Extension,Restriction,
						  CSt1)
			      end, CSt, IDList)
		    end
	    end
    end;

declaration('ATTLIST', Line, CSt) ->
    Data = cat(reverse(CSt#cstate.astack), " "),
    ?dbg("ATTLIST:~w ~s\n", [Line,Data]),
    Ts0 = tokens(Data),
    case catch expand_tokens(Ts0, CSt#cstate.entity_list) of
	{error,Reason} ->
	    error(Line, Reason, CSt);
	{'EXIT', Reason} ->
	    ?dbg("Crash: reason ~p\n", [Reason]),
	    error(Line, Reason, CSt);
	Ts ->
	    ?dbg("tokens: ~p\n", [Ts]),
	    case idlist(Ts, CSt) of
		{error,_} ->
		    error(Line, {syntax_error, Data}, CSt);
		{IDList,Ts1} ->
		    ?dbg("tokens: ~p\n", [Ts1]),
		    case catch parse_attr(Ts1, CSt) of
			Error={syntax_error,_} ->
			    error(Line, Error, CSt);			    
			Error={error,_} ->
			    error(Line, Error, CSt);
			{'EXIT',Reason} ->
			    error(Line, Reason, CSt);
			As when is_list(As) ->
			    foldl(
			      fun(ID,CSt1) ->
				      add_attributes(Line, ID, As, CSt1)
			      end,CSt,IDList)
		    end
	    end
    end;
declaration(_Tag, _Line, St) ->
    ?dbg("DIRECTIVE:~w ~s\n", [_Line,_Tag]),
    St#cstate { astack = []}.

dparam(_Tag, Param, _Line, St) ->
    ?dbg("DPARAM:~w ~s ~p\n", [_Line,_Tag,Param]),
    As = [Param|St#cstate.astack],
    St#cstate { astack = As }.

dcomment(_Tag,_Comment,_Line,St) ->
    ?dbg("COMMENT:~w ~s ~s\n", [_Line,_Tag,_Comment]),
    St.

comment(_Comment,_Line,St) ->
    ?dbg("COMMENT:~w ~s\n", [_Line,_Comment]),
    St.

%%
%% Handle INCLUDE | IGNORE
%%
section(Name, Data, Line, St) ->
    ?dbg("SECTION:~w: ~s ~s\n", [Line,Name,Data]),
    Ts0 = tokens(Name), 
    {St1,Section} =
	case catch expand_tokens(Ts0, St#cstate.entity_list) of
	    {error,Reason} ->
		{error(Line,Reason,St), "ERROR"};
	    [{word,[$\%|EName]},';'] ->
		case lists:keysearch(EName, #makeup_entity.name, 
				     St#cstate.entity_list) of
		    false ->
			{error(Line,{entity_not_found,EName},St), "ERROR"};
		    {value,E} ->
			if E#makeup_entity.type == inline ->
				{St, E#makeup_entity.def};
			   true ->
				{error(Line,{entity_not_inline,EName},St),
				 "ERROR"}
			end
		end;
	    [{word,[$\%|EName]}] ->
		case lists:keysearch(EName, #makeup_entity.name, 
				     St#cstate.entity_list) of
		    false ->
			{error(Line,{entity_not_found,EName},St), "ERROR"};
		    {value,E} ->
			if E#makeup_entity.type == inline ->
				{St, E#makeup_entity.def};
			   true ->
				{error(Line,{entity_not_inline,EName},St),
				 "ERROR"}
			end
		end;
	    [{word,Nm}|_] ->
		{St, Nm};
	    _ ->
		{St, "IGNORE"}
	end,
    case Section of
	"IGNORE" ->
	    St1#cstate { astack=[]};
	"INCLUDE" ->
	    %% Push back data section to stream
	    makeup:istring(Data, St1#cstate { astack = [] });
	"ERROR" ->
	    io:format("section: ERROR ~s\n",[Name]),
	    St1#cstate { astack=[]};
	Def ->
	    io:format("section: IGNORE ~p ~s\n",[Def,Name]),
	    St1#cstate { astack=[]}
    end.


charref(_Char, _Line, _St) ->
    ?dbg("CHARREF:~w: ~w\n", [_Line,_Char]),
    undefined.

%%
%% Add element
%% 
add_element(Line,Name,Start,Stop,Rule,Extension,Restriction,CSt) ->
    Es0 = CSt#cstate.element_list,
    case lists:keysearch(Name, #makeup_element.name, Es0) of
	false ->
	    E = #makeup_element { name  = Name,
				  start = Start,
				  stop  = Stop,
				  rule  = Rule,
				  include = Extension,
				  exclude = Restriction },
	    Es = [E|Es0],
	    CSt#cstate { element_list=Es, astack=[]};
	{value,E0} when E0#makeup_element.rule == undefined ->
	    E = E0#makeup_element { start = Start, 
				    stop  = Stop,
				    rule  = Rule,
				    include = Extension,
				    exclude = Restriction },
	    Es = lists:keyreplace(name,#makeup_element.name,Es0,E),
	    CSt#cstate { element_list=Es, astack=[] };
	{value,E0} ->
	    CSt1 = error(Line,{element_multiply_defined,Name},CSt),
	    E = E0#makeup_element { start = Start, stop  = Stop,
				    rule  = Rule,
				    include = Extension,
				    exclude = Restriction },
	    Es = lists:keyreplace(name,#makeup_element.name,Es0,E),
	    CSt1#cstate { element_list=Es, astack=[]}
    end.

add_attributes(Line, Name, Attributes, CSt) ->
    Es0 = CSt#cstate.element_list,
    case lists:keysearch(Name, #makeup_element.name, Es0) of
	false ->
	    E = #makeup_element { name  = Name },
	    {E1,CSt1} = add_attribute_list(Line, Attributes, E, CSt),
	    Es = [E1|Es0],
	    CSt1#cstate { element_list=Es, astack=[]};
	{value,E0} ->
	    {E1,CSt1} = add_attribute_list(Line, Attributes, E0, CSt),
	    Es1 = lists:keyreplace(Name, #makeup_element.name, Es0, E1),
	    CSt1#cstate { element_list=Es1, astack=[]}
    end.
	    
add_attribute_list(Line, [A|As], E, CSt) ->
    EAs = E#makeup_element.attr,
    AName = A#makeup_attribute.name,
    case lists:keysearch(AName, #makeup_attribute.name, EAs) of
	false ->
	    EAs1 = [A | EAs],
	    add_attribute_list(Line, As, E#makeup_element { attr=EAs1}, CSt);
	{value,A} ->
	    add_attribute_list(Line, As, E, CSt);
	{value,A1} ->
	    CSt1 = error(Line,{attribute_multiply_defined,AName},CSt),
	    EAs1 = [A1 | EAs],
	    add_attribute_list(Line, As, E#makeup_element { attr=EAs1}, CSt1)
    end;
add_attribute_list(_Line, [], E, CSt) ->
    {E, CSt}.
    
    


split_exception(Es) ->
    split_exception(Es, [], []).

split_exception([{extension,{choice,As}}|Es], Include, Exclude) ->
    split_exception(Es, As++Include, Exclude);
split_exception([{extension,A}|Es], Include, Exclude) ->
    split_exception(Es, [A|Include], Exclude);
split_exception([{restriction,{'|',As}}|Es], Include, Exclude) ->
    split_exception(Es, Include, As++Exclude);
split_exception([{restriction,A}|Es], Include, Exclude) ->
    split_exception(Es, Include, [A|Exclude]);
split_exception([], Include, Exclude) ->
    {Include, Exclude}.

%%
%% Expand entity references in the text.
%%
dtext(Data,Line,St) ->
    ?dbg("TEXT:~w: ~s\n", [Line,Data]),
    case dtext_split(Data) of
	false -> 
	    St;
	{_Before,Name,After} ->
	    case lists:keysearch(Name,#makeup_entity.name,St#cstate.entity_list) of
		false ->
		    io:format("Error: missing entity ~s\n",[Name]),
		    dtext(After, Line, St);
		{value,E} ->
		    case E#makeup_entity.type of
			system ->
			    ID = E#makeup_entity.def,
			    case makeup_dtd_srv:system_file(ID) of
				{error,Reason} ->
				    St1 = error(Line,{system_url_not_found,ID,Reason},St),
				    dtext(After, Line, St1);
				{ok,File} ->
				    St1 = makeup:ifile(File, St),
				    dtext(After, Line, St1)
			    end;
			public ->
			    {PUBID, _Url} = E#makeup_entity.def,
			    case makeup_dtd_srv:public_file(PUBID) of
				{error,Reason} ->
				    St1 = error(Line,{public_id_not_found,PUBID,Reason},St),
				    dtext(After, Line, St1);
				{ok,File} ->
				    St1 = makeup:ifile(File, St),
				    dtext(After, Line, St1)
			    end;
			inline ->
			    St1 = makeup:istring(E#makeup_entity.def, St),
			    dtext(After, Line, St1);
			Type ->
			    St1 = error(Line,{bad_entity_type,Name,Type},St),
			    dtext(After, Line, St1)
		    end
	    end
    end.

%%
%% Find entity references in string and
%% i.e    "abc%t;foo" -> {"abc", "t", "foo"}
%%
%% return {BeforeText, Ref, AfterText}
%%     or false
%%
dtext_split(Cs) ->
    dtext_split(Cs,[]).

dtext_split([$\%|Cs], Before) ->
    dtext_split(Cs, [], Before);
dtext_split([C|Cs], Before) ->
    dtext_split(Cs, [C|Before]);
dtext_split([], _Before) ->
    false.

dtext_split([$;|After],Name,Before) ->
    {reverse(Before), reverse(Name), After};
dtext_split([C|Cs], Name, Before) ->
    if C >= $a, C=<$z ->
	    dtext_split(Cs,[C|Name],Before);
       C >= $A, C=<$Z ->
	    dtext_split(Cs,[C|Name],Before);
       C >= $0, C=<$9 ->
	    dtext_split(Cs,[C|Name],Before);
       C == $.; C==$_; C==$- ->
	    dtext_split(Cs,[C|Name],Before);
       true ->
	    dtext_split([C|Cs],Name++[$\%|Before])
    end;
dtext_split([], _Name, _Before) ->
    %% FIXME: Possibly use the name here (for string inline)
    false.


%% Insert errors in error list
error(Line,{error,Reason},CSt) ->
    io:format("~w: error=~p\n", [Line,Reason]),
    erlang:error(stop),
    EL = CSt#cstate.error_list,
    CSt#cstate { error_list = [{Line, Reason}|EL], astack = [] };
error(Line,Reason,CSt) ->
    io:format("~w: error=~p\n", [Line,Reason]),
    erlang:error(stop),
    EL = CSt#cstate.error_list,
    CSt#cstate { error_list = [{Line,Reason}|EL], astack = [] }.

    


delopt(Opt, Opts) ->
    lists:keydelete(Opt, 1, Opts).

getopt(Opt, Opts, Default) ->
    case lists:keysearch(Opt, 1, Opts) of
	false -> Default;
	{value,{_,Value}} -> Value
    end.

%% Expand parameter entity references found in tokens

expand_tokens([{word,[$\%|Name]}|Ts], Params) ->
    case lists:keysearch(Name, #makeup_entity.name, Params) of
	false ->
	    throw({error, {missing_entity, Name}});
	{value,E} ->
	    Expansion = expand_string_df(E#makeup_entity.def, Params),
	    case tokens(Expansion) of
		Error={error,_} ->
		    throw(Error);
		Ts1 ->
		    %% io:format("Expand:word:~p => ~p\n", [Nm,Ts1]),
		    case Ts of 
			[';' | Ts2] ->
			    expand_tokens(Ts1++Ts2, Params);
			_ ->
			    expand_tokens(Ts1++Ts, Params)
		    end
	    end
    end;
expand_tokens([{string,String}|Ts], Params) ->
    Expansion = expand_string_df(String, Params),
    [{string,Expansion} | expand_tokens(Ts, Params)];
expand_tokens([T|Ts], Params) ->
    [T | expand_tokens(Ts, Params)];
expand_tokens([],_) ->
    [].

%% Fully expand string replace refs to %x; with expanstion
%% Expand depth first
%% Throws {error, Reason}

expand_string_df(String, Params) ->
    %% io:format("Expand:df: ~s\n", [String]),
    case dtext_split(String) of
	false -> String;
	{Before,Name,After} ->
	    case lists:keysearch(Name, #makeup_entity.name, Params) of 
		false ->
		    throw({error, {missing_entity, Name}});
		{value,E} ->
		    Expansion = E#makeup_entity.def,
		    %% io:format("Expand:name: ~s => ~s\n", [Name, Expansion]),
		    Before ++ 
			expand_string_df(Expansion ++ After,Params)
	    end
    end.



idlist(Ts, St) ->
    case catch parse_expr(Ts, St) of
	{{choice,IDs},Ts1} -> 
	    {map(fun({element,E}) -> E; (E) -> E end, IDs), Ts1};
	{'EXIT', Reason} -> {error,Reason};
	{{element,ID}, Ts1} when ?is_tag(ID) -> {[ID], Ts1};
	Other -> {error, {syntax_error,Other}}
    end.


%% Parse DTD element syntax description
%%
%% expr:
%%     | 'EMPTY'
%%     | 'CDATA'
%%     | 'ANY'                     -- whatever (#PCDATA or tagged?)
%%     | '#PCDATA'
%%     | <id>                       - must occure, one time
%%     | [<ns>|<id>]                - as id but with namespace
%%     | <expr> '|' <expr>          - either but not both
%%     | <expr> '&' <expr>          - both in any order
%%     | <expr> ',' <expr>          - followed by
%%     | <expr> '?'                 - zero or one 
%%     | <expr> '*'                 - zero or more
%%     | <expr> '+'                 - one or more
%%     | '+' '(' <expr> ')'         - may occure
%%     | '-' '(' <expr> ')'         - must not occure
%%     | '(' <expr> ')'
%%
%% element: '<' '!ELEMENT' <names> [<start-stop>] <expr> '>'
%% 
%% start-stop: - -     -- mandatory start and stop
%%             - O     -- end tag optional
%%             O O     -- optional tag?
%%
%% <!ELEMENT <name> - 0 EMPTY>  => end tag MUST be omitted
%%
%%

parse_rule(['-', '-' | Ts], St) ->
    {mandatory, mandatory, parse_expression(Ts,St)};
parse_rule(['-', {word,"O"} | Ts],St) ->
    {mandatory, optional, parse_expression(Ts,St)};
parse_rule([{word,"O"},{word,"O"} | Ts], St) ->
    {optional, optional, parse_expression(Ts,St)};
parse_rule(Ts0=[{word,"O"}, '-' | _],_St) ->
    {syntax_error,Ts0};
parse_rule(Ts,St) ->
    {mandatory, mandatory, parse_expression(Ts,St)}.

parse_expression(Ts, St) ->
    {E,Ts1} = parse_expr(Ts,St),
    {E,parse_exception(Ts1,[],St)}.

parse_exception(['-'|Ts], ExList,St) ->
    case parse_expr(['-'|Ts],St) of
	{E, []} ->
	    reverse([E|ExList]);
	{E, Ts1} ->
	    parse_exception(Ts1, [E|ExList],St)
    end;
parse_exception(['++'|Ts], ExList, St) ->
    case parse_expr(['++'|Ts], St) of
	{E, []} ->
	    reverse([E|ExList]);
	{E, Ts1} ->
	    parse_exception(Ts1, [E|ExList],St)
    end;
parse_exception([], ExList, _St) ->
    reverse(ExList);
parse_exception(Ts1,_, _) ->
    throw({syntax_error, Ts1}).


parse_expr(Ts,St) ->
    parse_expr(Ts, '--',St).

parse_expr(Ts,StopToken,St) ->
    case parse_expr1(Ts,St) of
	{E, [StopToken|Ts1]} -> {E,[StopToken|Ts1]};
	{E, ['|'|Ts1]} -> parse_expr_list(Ts1, [E], '|', choice, St);
	{E, ['&'|Ts1]} -> parse_expr_list(Ts1, [E], '&', all, St);
	{E, [','|Ts1]} -> parse_expr_list(Ts1, [E], ',', sequence, St);
	ETs -> ETs
    end.

parse_expr1(Ts, St) ->
    case parse_expr0(Ts, St) of
	{E, ['+'|Ts1]}  -> { {pclosure, E}, Ts1};
	{E, ['*'|Ts1]}  -> { {closure, E}, Ts1};
	{E, ['?'|Ts1]}  -> { {optional, E}, Ts1};
	ETs -> ETs
    end.

parse_expr0([{word,"EMPTY"}|Ts], _St) ->    {empty, Ts};
parse_expr0([{word,"#PCDATA"}|Ts], _St) ->  {pcdata, Ts};
parse_expr0([{word,"CDATA"}|Ts], _St) ->    {pcdata, Ts};
parse_expr0([{word,"ANY"}|Ts], _St) ->      {any, Ts};
parse_expr0([{word,ID}|Ts], St) ->
    Tag = makeup:name_ns(makeup:cvtcase(St#cstate.icase,ID),St#cstate.ns),
    {{element,Tag}, Ts};
parse_expr0([{number,N}|Ts], _St) ->
    {N, Ts};
parse_expr0(['++'|Ts], St) ->
    {E,Ts1} = parse_expr(Ts,'--', St),
    {{extension,E}, Ts1};
parse_expr0(['-'|Ts], St) ->
    {E,Ts1} = parse_expr(Ts, St),
    {{restriction,E}, Ts1};
parse_expr0(['('|Ts], St) ->
    case parse_expr(Ts, St) of
	{E,[')'|Ts1]} -> {E, Ts1};
	_ -> throw({synax_error, Ts})
    end.

parse_expr_list(Ts, Es, Tok, Op, St) ->
    case parse_expr(Ts,Tok, St) of
	{E, [Tok|Ts1]} ->
	    parse_expr_list(Ts1, [E|Es], Tok, Op, St);
	{E, Ts1} ->
	    { {Op,reverse([E|Es])}, Ts1}
    end.
%%
%% Attribte declaration:
%%  ( <name> <def> <status> )*
%%
%% 
%%
parse_attr(Ts, St) ->
    parse_attr(Ts, [], St).

parse_attr([{word,Name}|Ts],Acc, St) ->
    AttrName = makeup:name_ns(makeup:cvtcase(St#cstate.icase,Name),
			      St#cstate.ns),
    case parse_expr(Ts,St) of
	{{choice,Enums}, Ts1} ->
	    EnumStr = mk_enums(Enums),
	    parse_attr_status(Ts1, AttrName, {enum,EnumStr}, Acc,St);
	{{element,Def}, Ts1} when is_atom(Def) ->
	    parse_attr_status(Ts1, AttrName, atom_to_list(Def), Acc,St);
	{Def,Ts1} when is_list(Def) ->
	    parse_attr_status(Ts1, AttrName, Def, Acc,St);
	{pcdata, Ts1} -> %% Hmm fixme
	    parse_attr_status(Ts1, AttrName, "CDATA", Acc,St);
	_ ->
	    throw({syntax_error,Ts})
    end;
parse_attr([], Acc, _St) ->
    reverse(Acc).

mk_enums([{element,E}|Es]) when is_atom(E) ->
    [atom_to_list(E) | mk_enums(Es)];
mk_enums([E|Es])  ->
    [E | mk_enums(Es)];
mk_enums([]) ->
    [].


parse_attr_status([{word,"#IMPLIED"}|Ts],Name,Type,Acc0,St) ->
    A = #makeup_attribute { name   = Name,
			    type   = Type,
			    status = optional },
    parse_attr(Ts, [A|Acc0],St);
parse_attr_status([{word,"#implied"}|Ts],Name,Type,Acc0,St) ->
    A = #makeup_attribute { name = Name,
			    type = Type,
			    status = optional },
    parse_attr(Ts, [A|Acc0],St);
parse_attr_status([{word,"#REQUIRED"}|Ts],Name,Type,Acc0,St) ->
    A = #makeup_attribute { name   = Name,
			    type   = Type,
			    status = required },
    parse_attr(Ts, [A|Acc0],St);
parse_attr_status([{word,"#FIXED"},{word,Value}|Ts],Name,Type,Acc0,St) ->
    A = #makeup_attribute { name   = Name,
			    type   = Type,
			    status = fixed,
			    value  = Value },
    parse_attr(Ts, [A|Acc0],St);
parse_attr_status([{word,"#FIXED"},{number,Value}|Ts],Name,Type,Acc0,St) ->
    A = #makeup_attribute { name   = Name,
			    type   = Type,
			    status = fixed,
			    value  = Value },
    parse_attr(Ts, [A|Acc0],St);
parse_attr_status([{word,"#FIXED"},{string,Value}|Ts],Name,Type,Acc0,St) ->
    A = #makeup_attribute { name   = Name,
			    type   = Type,
			    status = fixed,
			    value  = Value },
    parse_attr(Ts, [A|Acc0],St);
parse_attr_status([{word,Value}|Ts],Name,Type,Acc0,St) ->
    A = #makeup_attribute { name   = Name,
			    type   = Type,
			    status = default,
			    value  = Value
			   },
    parse_attr(Ts, [A|Acc0],St);
parse_attr_status([{number,Value}|Ts],Name,Type,Acc0,St) ->
    A = #makeup_attribute { name  = Name,
			    type  = Type,
			    status = default,
			    value  = Value
			   },
    parse_attr(Ts, [A|Acc0],St);
parse_attr_status([{string,Value}|Ts],Name,Type,Acc0,St) ->
    A = #makeup_attribute { name   = Name,
			    type   = Type,
			    status = default,
			    value  = Value
			   },
    parse_attr(Ts, [A|Acc0],St).

%% Verify that elements refered to from rules exists
cross_ref(St) ->
    Elems = St#cstate.element_list,
    foldl(
      fun(E,St1) ->
	      foldl(
		fun(pcdata,St2) -> St2;
		   (any,St2) -> St2;
		   (Name,St2) ->
			case makeup:ilevel() of
			    0 ->
				case lists:keysearch(Name,#makeup_element.name,
						     Elems) of
				    false ->
					error(0,{element_not_defined,Name},St2);
				    _ -> St2
				end;
			    _ -> St2
			end
		end, St1, makeup_re:symbols(E#makeup_element.rule))
      end, St, Elems).

%% Concatinate a string parts with optional separator
cat(Es) ->
    cat(Es, "").

cat([], _Sep) -> "";
cat(Es, Sep) ->
    append(cat1(Es,Sep)).

cat1([E], _Sep) -> [E];
cat1([E|Es],Sep) -> [E,Sep | cat1(Es,Sep)].
    

tokens(String) ->
    tokens(String, []).

tokens([$\s|Cs],Ts)   -> tokens(Cs,Ts);
tokens([$\t|Cs],Ts)   -> tokens(Cs,Ts);
tokens([$\r|Cs],Ts)   -> tokens(Cs,Ts);
tokens([$\n|Cs],Ts)   -> tokens(Cs,Ts);
tokens([$:|Cs],Ts)    -> tokens(Cs,[':'|Ts]);
tokens([$;|Cs],Ts)    -> tokens(Cs,[';'|Ts]);
tokens([$,|Cs],Ts)    -> tokens(Cs,[','|Ts]);
tokens([$-,$-|Cs],Ts) -> tcomment(Cs,Ts);
tokens([$-|Cs],Ts)    -> tokens(Cs,['-'|Ts]);
tokens([$+,$(|Cs],Ts)    -> tokens(Cs,['(','++'|Ts]);
tokens([$+|Cs],Ts)    -> tokens(Cs,['+'|Ts]);
tokens([$||Cs],Ts)    -> tokens(Cs,['|'|Ts]);
tokens([$&|Cs],Ts)    -> tokens(Cs,['&'|Ts]);
tokens([$*|Cs],Ts)    -> tokens(Cs,['*'|Ts]);
tokens([$?|Cs],Ts)    -> tokens(Cs,['?'|Ts]);
tokens([$(|Cs],Ts)    -> tokens(Cs,['('|Ts]);
tokens([$)|Cs],Ts)    -> tokens(Cs,[')'|Ts]);
tokens([$\"|Cs],Ts)    -> tstring(Cs,$\",[],Ts);
tokens([$\'|Cs],Ts)    -> tstring(Cs,$\',[],Ts);
tokens([],Ts)         -> reverse(Ts);
tokens([C|Cs],Ts) when C >= $0, C =< $9 -> token(Cs,[C],true,Ts);
tokens([C|Cs],Ts) when C >= $a, C =< $z -> token(Cs,[C],false,Ts);
tokens([C|Cs],Ts) when C >= $A, C =< $Z -> token(Cs,[C],false,Ts);
tokens([$_|Cs],Ts) -> token(Cs,[$_],false,Ts);
tokens([$#|Cs],Ts) -> token(Cs,[$#],false,Ts);
tokens([$%|Cs],Ts) -> token(Cs,[$%],false,Ts);
tokens([C|Cs],Ts) -> tokens(Cs,[{char,C}|Ts]).


token([C|Cs],T,N,Ts) when C >= $0, C =< $9 -> token(Cs,[C|T],N,Ts);
token([C|Cs],T,_,Ts) when C >= $a, C =< $z -> token(Cs,[C|T],false,Ts);
token([C|Cs],T,_,Ts) when C >= $A, C =< $Z -> token(Cs,[C|T],false,Ts);
token([$_|Cs],T,_N,Ts) -> token(Cs,[$_|T],false,Ts);
token([$.|Cs],T,_N,Ts) -> token(Cs,[$.|T],false,Ts);
token([$-|Cs],T,_N,Ts) -> token(Cs,[$-|T],false,Ts);
token([$:|Cs],T,_N,Ts) -> token(Cs,[$:|T],false,Ts);
token([$/|Cs],T,_N,Ts) -> token(Cs,[$/|T],false,Ts);
token(Cs,T,N,Ts) ->
    if
	N == true ->
	    tokens(Cs,[{number,reverse(T)}|Ts]);
       true ->
	    tokens(Cs,[{word,reverse(T)}|Ts])
    end.

tcomment([$-,$- | Cs], Ts) ->
    tokens(Cs, Ts);
tcomment([_|Cs], Ts) ->
    tcomment(Cs, Ts);
tcomment([], Ts) ->
    Ts.


tstring([$\\,C1,C2,C3|Cs],Q, Acc, Ts)
  when C1 >=$0, C1 =< $7, C2 >=$0, C2 =< $7, C3 >=$0, C3 =< $7 ->
    N = (C1-$0)*64 + (C2-$0)*8 + (C3-$0),
    tstring(Cs,Q,[N|Acc],Ts);
tstring([$\\,C1,C2|Cs],Q, Acc, Ts)
  when C1 >=$0, C1 =< $7, C2 >=$0, C2 =< $7 ->
    N = (C1-$0)*8 + (C2-$0),
    tstring(Cs,Q,[N|Acc],Ts);
tstring([$\\,C1|Cs],Q, Acc, Ts) 
  when C1 >=$0, C1 =< $7 ->
    N = (C1-$0),
    tstring(Cs,Q,[N|Acc],Ts);
tstring([$\\,C|Cs],Q,Acc,Ts) ->
    case C of
	$n -> tstring(Cs,Q,[$\n|Acc],Ts);
	$r -> tstring(Cs,Q,[$\r|Acc],Ts);
	$b -> tstring(Cs,Q,[$\b|Acc],Ts);
	$t -> tstring(Cs,Q,[$\t|Acc],Ts);
	$s -> tstring(Cs,Q,[$\s|Acc],Ts);
	$f -> tstring(Cs,Q,[$\f|Acc],Ts);
	_ -> tstring(Cs,Q,[C|Acc],Ts)
    end;
tstring([Q|Cs],Q,Acc,Ts) ->
    tokens(Cs, [{string,reverse(Acc)}|Ts]);
tstring([C|Cs],Q,Acc,Ts) ->
    tstring(Cs,Q,[C|Acc],Ts);
tstring([],_,_Acc,_Ts) ->
    {error, unterminated_string}.


