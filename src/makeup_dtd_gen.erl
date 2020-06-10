%%% File    : makeup_dtd_gen.erl
%%% Author  : Tony Rogvall <tony@bix.hemma.se>
%%% Description : DTD generator
%%% Created : 19 Feb 2004 by Tony Rogvall <tony@bix.hemma.se>

-module(makeup_dtd_gen).

-include("../include/makeup.hrl").

-ifdef(debug).
-define(dbg(Fmt,As), io:format((Fmt),(As))).
-else.
-define(dbg(Fmt,As), ok).
-endif.

-define(toupper(C), if (C) >= $a, (C) =< $z -> ((C)-$a)+$A; true -> (C) end).

-export([file/1, file/2]).
-export([compile/1, compile/2]).

compile(File) ->
    compile(File,[]).

compile(File,Opts) ->
    stop(File, catch file(File,Opts)).

stop(_File, {'EXIT',Reason}) -> 
    io:format("dtd_compile: crash: ~p\n", [Reason]),
    halt(1);
stop(File, {error,ReasonList}) when is_list(ReasonList) -> 
    lists:foreach(
      fun({Line,{function_clause,Crash}}) when is_integer(Line) ->
	      io:format("~s:~w: crash ~999p\n",
			[File, Line, Crash]);
	 ({Line,{syntax_error,Tokens}}) when is_integer(Line) ->
	      io:format("~s:~w: syntax error ~999p\n",
			[File, Line, Tokens]);

	 ({Line,{element_multiply_defined,E}}) when is_integer(Line) ->
	      io:format("~s:~w: ELEMENT ~s multiply defined\n",
			[File, Line, E]);
	 ({Line,Reason}) when is_integer(Line) ->
	      io:format("~s:~w: error ~999p\n",
			[File, Line, Reason]);
	 (Error) ->
	      io:format("~s: error ~999p\n",
			[File, Error])
      end, lists:reverse(ReasonList)),
    halt(1);
stop(_File, {error,Reason}) ->     
    io:format("dtd_compile: error: ~p\n", [Reason]),
    halt(1);
stop(_File, ok) -> 
    halt(0).


file(File) ->
    file(File, []).

file(I_File,Opts) ->
    case filename:extension(I_File) of
	".dtd" ->
	    BaseName = filename:basename(I_File,".dtd"),
	    O_Mod    = getopt(mod, Opts, BaseName++"_dtd"),
	    O_Dir    = getopt(outdir, Opts, "./"),
	    O_ErlFile   = case getopt(out, Opts, undefined) of
			      undefined ->
				  filename:join(O_Dir,O_Mod++".erl");
			      OutFile1 -> OutFile1
			  end,
	    O_HrlFile = case getopt(outh, Opts, undefined) of
			    undefined ->
				filename:join(O_Dir,O_Mod++".hrl");
			    HrlFile -> HrlFile
			end,
	    file(I_File,O_Mod, O_ErlFile, O_HrlFile, Opts);
	Ext ->
	    {error, {bad_suffix, Ext, I_File, Opts}}
    end.

file(I_File, O_Mod, O_ErlFile, O_HrlFile, Opts) ->
    case makeup_dtd:file(I_File, Opts) of
	Error={error,_} ->
	    Error;
	{ok,{ElemList,Ents}} ->
	    case emit_erl(O_ErlFile, O_Mod, ElemList, Ents, Opts) of
		ok ->
		    case getopt(record_style, Opts, false) of
			false -> 
			    ok;
			true ->
			    Prefix = getopt(prefix, Opts, ""),
			    emit_hrl(O_HrlFile, Prefix, ElemList, Opts)
		    end
	    end
    end.

%%
%% Emit erlang state machine
%%
emit_erl(O_ErlFile, O_Mod, ElemList0, Ents, Opts) ->
    case file:open(O_ErlFile, [write]) of
	{ok,Fd} ->
	    emit_erl_header(Fd),
	    emit_decls(Fd, O_Mod),
	    ElemList = expand_rules(ElemList0),
	    emit_pubid(Fd, Opts),
	    emit_rule(Fd, ElemList),
	    emit_state(Fd, ElemList, Opts),
	    emit_exception(Fd, ElemList),
	    emit_start_tag(Fd, ElemList),
	    emit_stop_tag(Fd, ElemList),
	    emit_attributes(Fd, ElemList),
	    emit_attribute(Fd, ElemList),
	    emit_charrefs(Fd, Ents),
	    file:close(Fd),
	    ok;
	Error ->
	    Error
    end.

%%
%% Emit erlang record style hrl file
%%    

emit_hrl(O_HrlFile, Prefix, ElemList, Opts) ->
    case file:open(O_HrlFile, [write]) of
	{ok,Fd} ->
	    emit_hrl_header(Fd),
	    emit_records(Fd, ElemList, Prefix, Opts),
	    file:close(Fd);
	Error ->
	    Error
    end.

emit_hrl_header(Fd) ->
    io:format(Fd, "%% DTD record module generate by: ~s\n"
	          "%% Version: ~s\n"
	          "%%    Date: ~s\n",
	      [?MODULE, 
	       makeup:vesion(),
	       format_date(calendar:universal_time())]).

emit_erl_header(Fd) ->
    io:format(Fd, "%% DTD state module generate by: ~s\n"
	          "%% Version: ~s\n"
	          "%%    Date: ~s\n",
	      [?MODULE, 
	       makeup:version(),
	       format_date(calendar:universal_time())]).

emit_decls(Fd, O_Mod) ->
    io:format(Fd,"-module('~s').\n", [O_Mod]),
    io:format(Fd,"-export([pubid/0]).\n", []),
    io:format(Fd,"-export([rule/1]).\n", []),
    io:format(Fd,"-export([state_init/1]).\n", []),
    io:format(Fd,"-export([state_accept/2]).\n", []),
    io:format(Fd,"-export([state/3]).\n", []),
    io:format(Fd,"-export([include/1]).\n", []),
    io:format(Fd,"-export([exclude/1]).\n", []),
    io:format(Fd,"-export([start_tag_optional/1]).\n",[]),
    io:format(Fd,"-export([stop_tag_optional/1]).\n",[]),
    io:format(Fd,"-export([attribute/2]).\n", []),
    io:format(Fd,"-export([attributes/1]).\n", []),
    io:format(Fd,"-export([charref/1]).\n\n", []).


emit_pubid(Fd, Opts) ->
    PubID = getopt(pubid, Opts, ""),
    io:format(Fd, "pubid() -> \"~s\".\n\n", [PubID]).


%% Emit record definitions for each Element

emit_records(Fd, Es, Pfx, _Opts) ->
    Indent = "        ",
    lists:foreach(
      fun(E) ->
	      RName = list_to_atom(Pfx ++ 
				   makeup_re:format_tag(E#makeup_element.name)),
	      Rule = E#makeup_element.rule,
	      case Rule of
		  empty  -> ignore;
		  pcdata -> ignore;
		  _ ->
		      Attributes = format_auto(RName,"attributes",1),
		      io:format(Fd, 
				"\n-record(~p,\n~s{\n"
				"~s~p,    %% Attributes\n"
				"~s"
				"~s}).\n",
				[RName, Indent,
				 Indent, list_to_atom(Attributes),
				 format_fields(Rule, Es, Indent, RName),
				 Indent
				])
	      end
      end, Es).

name(Name) ->
    lists:flatten(io_lib:format("~p", [Name])).    

%% Generate a tag name where prefix is not used. 
%% ex: f([_|tag]) ->
tname([Xs|Name]) when is_atom(Xs), is_atom(Name) ->
    [H|Hs] = atom_to_list(Xs),
    "["++[$_,?toupper(H)]++Hs++"|"++name(Name)++"]";
tname(Name) when is_atom(Name) ->
    name(Name).
    
%% Generate a match name for name-space dtd's like XMLSchema.dtd
%% [xs|schema] => [_Xs|schema]  FIXME: handle bad namespace name
%% ex: state([Xs|foo], 1, [Xs|bar]) -> 2
mname([Xs|Name]) when is_atom(Xs), is_atom(Name) ->
    [H|Hs] = atom_to_list(Xs),
    "["++[?toupper(H)]++Hs++"|"++name(Name)++"]";
mname(Name) when is_atom(Name) ->
    name(Name).

%% Generate a valid atom literal foo => foo, FOO => 'FOO' etc
aname(Xs,[Xs|Name]) ->
    mname([Xs|Name]);
aname(Xs,[xmlns|Xs]) ->
    [H|Hs] = atom_to_list(Xs),
    "[xmlns|"++[?toupper(H)]++Hs++"]";
aname(_Xs,Name) ->
    name(Name).

ns([Xs|_]) ->
    Xs;
ns(_) ->
    ''.

%% mlist([]) -> "";
%% mlist([Name]) -> mname(Name);
%% mlist([Name|Ns]) ->
%%    [mname(Name),",",mlist(Ns)].

alist(_Xs,[]) -> "";
alist(Xs, [Name]) -> aname(Xs,Name);
alist(Xs,[Name|Ns]) ->
    [aname(Xs,Name),",",alist(Xs,Ns)].




format_fields(Rule,Es,Indent, RName) ->
    FList = format_fields(Rule,Es,RName),
    format_field_list(FList,Indent).

format_field_list([[Name,Comment]],Indent) ->
    AName = io_lib:format("~p",[list_to_atom(Name)]),
    [Indent,AName,"\t","%% ", Comment, "\n"];
format_field_list([[Name,Comment] | Fs],Indent) ->
    AName = io_lib:format("~p",[list_to_atom(Name)]),
    [Indent,AName,",","\t","%% ", Comment, "\n" |
     format_field_list(Fs,Indent)];
format_field_list([],_Indent) ->
    [].
    
format_fields({sequence,Es}, Elements, RName) ->
    format_elements(Es, 1, Elements, RName);
format_fields(Rule, Elements, RName) ->
    [_I,Name,Comment] = format_elem(Rule, 1, Elements, RName),
    [[Name,Comment]].

format_elements([E|Es], I, Elements, RName) ->
    [I1,Name,Comment] = format_elem(E, I, Elements, RName),
    [[Name,Comment] | format_elements(Es,I1,Elements, RName)];
format_elements([], _I, _Elements,_RName) ->
    [].

format_elem(A={element,E}, I, Es, _RName)  ->
    [I,makeup_re:format_tag(E),
     "Mandatory: "++comment_rule(A,Es)];
format_elem({closure,A={element,E}},I, Es, _RName) ->
    [I,makeup_re:format_tag(E), 
     "Optional List: ("++comment_rule(A,Es)++")*"];
format_elem({pclosure,A={element,E}}, I, Es, _RName) ->
    [I,makeup_re:format_tag(E),
     "Mandatory List: ("++comment_rule(A,Es)++")+"];
format_elem({optional,A={element,E}}, I, Es, _RName) ->
    [I,makeup_re:format_tag(E),
     "Optional: "++comment_rule(A,Es)++"?"];
format_elem({closure,A={choice,_}}, I, Es, RName) ->
    [I+1,format_auto(RName,"elements",I),
     "Optional List: ("++comment_rule(A,Es)++")*"];
format_elem({pclosure,A={choice,_}}, I, Es, RName) ->
    [I+1,format_auto(RName,"elements",I),
     "Mandatory List: ("++comment_rule(A,Es)++")+"];
format_elem(RE, I, _Es, RName) ->
    [I+1,format_auto(RName,"field",I), makeup_re:format(RE)].

format_auto(Prefix, Name, 1) ->
    atom_to_list(Prefix)++"@"++Name;
format_auto(Prefix, Name, I) ->
    atom_to_list(Prefix)++"@"++Name++"_"++integer_to_list(I).


comment_rule(Name, Elements) ->
    case lists:keysearch(Name, #makeup_element.name, Elements) of
	false -> "";
	{value, E} ->
	    makeup_re:format(E#makeup_element.rule)
    end.


getopt(Opt, Opts, Default) ->
    case lists:keysearch(Opt, 1, Opts) of
	false -> Default;
	{value,{_,Value}} -> Value
    end.

expand_rules(ElemList) ->
    expand_rules(ElemList, ElemList).

expand_rules([E|Es], ElementList) ->
    Name = E#makeup_element.name,
    Rule0 = E#makeup_element.rule,
    Rule = makeup_dtd:expand_rule(Rule0, ElementList,[Name]),
    if Rule == Rule0 ->
	    [E|expand_rules(Es, ElementList)];
       true ->
	    ?dbg("Expanded rule ~s : ~s  =>  ~s\n",
		 [makeup_re:format_tag(Name), 
		  makeup_re:format(Rule0),makeup_re:format(Rule)]),
	    [E#makeup_element { rule = Rule} | expand_rules(Es, ElementList)]
    end;
expand_rules([], _ElementList) ->
    [].
    
%% FIX namespace translation in rule([Xs|tag]) -> ... {[Xs|tugg]..    
emit_rule(Fd, [E|Es]) ->
    io:format(Fd, "rule(~s) -> ~999p;\n",
	      [tname(E#makeup_element.name), 
	       E#makeup_element.rule]), 
    emit_rule(Fd,Es);
emit_rule(Fd, []) ->
    io:format(Fd, "rule(_) -> undefined.\n\n", []).

emit_state(Fd, Es, _Opts) ->
    DFAs = 
	lists:map(
	  fun(E) -> 
		  Name = E#makeup_element.name,
		  Rule = E#makeup_element.rule,
		  ?dbg("RULE ~p=~p\n", [Name,Rule]),
		  DFA = makeup_re:make_dfa(Rule),
		  {Name, DFA, Rule}
	  end, Es),
    ?dbg("DFAs=~p\n", [DFAs]),
    lists:foreach(fun({Name,DFA,Rule}) ->
		    ?dbg("RULE ~p=~p\n", [Name,Rule]),
		    io:format(Fd, "%% ~s : ~s\n", 
			      [makeup_re:format_tag(Name),
			       makeup_re:format(Rule)]),
		    io:format(Fd, "state_init(~s) -> ~w;\n", 
			      [tname(Name),DFA#fa.init])
	    end, DFAs),
    io:format(Fd, "state_init(_) -> undefined.\n\n",[]),

    lists:foreach(
      fun({Name,DFA,_}) ->
	      lists:foreach(
		fun(#fa_state { id=Si,edges=Edges}) ->
			lists:foreach(
			  fun (#fa_edge{id='_',action=[],target=Sj}) ->
				  io:format(Fd, "state(~s,~w, _) -> ~w;\n",
					    [tname(Name), Si, Sj]);
			      (#fa_edge{id=Sym,action=[],target=Sj}) ->
				  io:format(Fd, "state(~s,~w,~s) -> ~w;\n",
					    [mname(Name), Si, mname(Sym), Sj]);
			      (#fa_edge{id='_',action=Action,target=Sj}) ->
				  io:format(Fd, "state(~s,~w, _) -> {~w,~p};\n",
					    [tname(Name), Si, Sj, Action]);
			      (#fa_edge{id=Sym,action=Action,target=Sj}) ->
				  io:format(Fd, "state(~s,~w,~s) -> {~w,~p};\n",
					    [mname(Name), Si, mname(Sym), Sj,Action])
			  end, sort_edges(Edges))
		end, DFA#fa.states)
      end, DFAs),

    io:format(Fd, "state(_, _, _) -> undefined.\n\n", []),

    lists:foreach(
      fun({Name,DFA,_}) ->
	      lists:foreach(
		fun(#fa_state { id=N, accept=Accept}) ->
			io:format(Fd, "state_accept(~s, ~w) -> ~p;\n",
				  [tname(Name),N,Accept])
		end, DFA#fa.states)
      end, DFAs),
    io:format(Fd, "state_accept(_, _) -> undefined.\n\n", []).

    

%% Put edge matching any thing last
sort_edges(Edges) ->
    case lists:keysearch('_', #fa_edge.id, Edges) of
	{value, E} ->
	    lists:keydelete('_', #fa_edge.id, Edges) ++ [E];
	false ->
	    Edges
    end.

emit_exception(Fd, Es) ->
    emit_include(Fd, Es),
    emit_exclude(Fd, Es).
    
emit_include(Fd, [E|Es]) ->
    if E#makeup_element.include == undefined;
       E#makeup_element.include == [] -> ok;
       true ->
	    io:format(Fd, "include(~s) -> ~999p;\n",
		      [tname(E#makeup_element.name),
		       E#makeup_element.include])
    end,
    emit_include(Fd,Es);
emit_include(Fd, []) ->
    io:format(Fd, "include(_) -> undefined.\n\n", []).

emit_exclude(Fd, [E|Es]) ->
    if E#makeup_element.exclude == undefined;
       E#makeup_element.exclude == [] -> ok;
       true ->
	    io:format(Fd, "exclude(~s) -> ~999p;\n",
		      [tname(E#makeup_element.name),
		       E#makeup_element.exclude])
    end,
    emit_exclude(Fd,Es);
emit_exclude(Fd, []) ->
    io:format(Fd, "exclude(_) -> undefined.\n\n", []).


%% emit_stop_tag.
emit_stop_tag(Fd, [E|Es]) ->
    if E#makeup_element.stop == optional ->
	    io:format(Fd, "stop_tag_optional(~s) -> true;\n",
		      [tname(E#makeup_element.name)]);
       true ->
	    ok
    end,
    emit_stop_tag(Fd,Es);
emit_stop_tag(Fd, []) ->
    io:format(Fd, "stop_tag_optional(_) -> undefined.\n\n",[]).


emit_start_tag(Fd, [E|Es]) ->
    if E#makeup_element.start == optional ->
	    io:format(Fd, "start_tag_optional(~s) -> true;\n",
		      [tname(E#makeup_element.name)]);
       true ->
	    ok
    end,
    emit_start_tag(Fd,Es);
emit_start_tag(Fd, []) ->
    io:format(Fd, "start_tag_optional(_) -> undefined.\n\n",[]).

emit_attributes(Fd, [E|Es]) ->
    As = lists:map(fun(A) -> A#makeup_attribute.name end, E#makeup_element.attr),
    Ns = ns(E#makeup_element.name),
    UseTName = lists:foldl(fun([Xs|_],_Bool) when Xs==Ns -> false;
			      ([xmlns|Xs],_Bool) when Xs==Ns -> false;
			      (_Name,Bool) -> Bool and true
			   end, true, As),
    if UseTName ->
	    Ns = ns(E#makeup_element.name),
	    io:format(Fd,"attributes(~s) -> [~s];\n",
		      [tname(E#makeup_element.name),
		       alist(Ns,As)]);
       true ->
	    Ns = ns(E#makeup_element.name),
	    io:format(Fd,"attributes(~s) -> [~s];\n",
		      [mname(E#makeup_element.name),
		       alist(Ns,As)])
    end,
    emit_attributes(Fd, Es);
emit_attributes(Fd, []) ->
    io:format(Fd,"attributes(_) -> undefined.\n", []).


emit_attribute(Fd, [E|Es]) ->
    lists:foreach(
      fun(A) ->
	      Xs = ns(E#makeup_element.name),
	      NameF = case A#makeup_attribute.name of
			  [Xs|_] -> fun mname/1;
			  [xmlns|Xs] -> fun mname/1;
			  _ -> fun tname/1
		      end,
	      io:format(Fd,"attribute(~s,~s) -> ~999p;\n",
			[NameF(E#makeup_element.name),
			 aname(Xs,A#makeup_attribute.name),
			 {A#makeup_attribute.type,
			  A#makeup_attribute.status,
			  A#makeup_attribute.value}])
      end, E#makeup_element.attr),
    emit_attribute(Fd,Es);
emit_attribute(Fd, _) ->
    io:format(Fd, "attribute(_,_) -> undefined.\n\n", []).

emit_charrefs(Fd, [E|Es]) ->
    if E#makeup_entity.type == cdata ->
	    io:format(Fd,"charref(\"~s\") -> ~999p;\n",
		      [E#makeup_entity.name,
		       E#makeup_entity.def]),
	    emit_charrefs(Fd, Es);
       true ->
	    emit_charrefs(Fd, Es)
    end;
emit_charrefs(Fd, []) ->
    io:format(Fd,"charref(_) -> undefined.\n",[]).


format_date({{Y,M,D},{TH,TM,TS}}) ->
    WkDay = case calendar:day_of_the_week({Y,M,D}) of
                1 -> "Mon";
                2 -> "Tue";
                3 -> "Wed";
                4 -> "Thu";
                5 -> "Fri";
                6 -> "Sat";
                7 -> "Sun"
            end,
    lists:flatten(io_lib:format("~s, ~2..0w ~s ~4..0w "
                                "~2..0w:~2..0w:~2..0w GMT",
                                [WkDay, D, fmonth(M), Y, TH, TM, TS])).

%% decode lowercase month
fmonth(1) -> "Jan";
fmonth(2) -> "Feb";
fmonth(3) -> "Mar";
fmonth(4) -> "Apr";
fmonth(5) -> "May";
fmonth(6) -> "Jun";
fmonth(7) -> "Jul";
fmonth(8) -> "Aug";
fmonth(9) -> "Sep";
fmonth(10) -> "Oct";
fmonth(11) -> "Nov";
fmonth(12) -> "Dec".
