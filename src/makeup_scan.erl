%%% File    : makeup_scan.erl
%%% Author  : Tony Rogvall <tony@localhost.localdomain>
%%% Description : Reetrant tag /attribute scanner
%%% Created : 16 Feb 2004 by Tony Rogvall <tony@localhost.localdomain>

-module(makeup_scan).

-rcsid("$Id: makeup_scan.erl,v 1.4 2007/02/04 09:21:36 tony Exp $\n").

-vsn("$Revision: 1.4 $ ").

-export([file/1, file/2, file/3]).
-export([ifile/2, istring/2, ioptions/2, ilevel/0]).
-export([string/1, string/2, string/3]).
-export([etags/1, etags/2]).

-import(lists, [reverse/1,map/2, foldl/3, foreach/2, member/2,append/1]).

-include("../include/makeup.hrl").

-define(dbgi(Fmt,As), io:format((Fmt),(As))).

-ifdef(debug).
-define(dbg(Fmt,As), io:format((Fmt),(As))).
-compile(export_all).
-else.
-define(dbg(Fmt,As), ok).
-endif.

-define(MAKEUP_STACK, '$makeup_stack').

-define(DEFAULT_CHUNK_SIZE, 512).

%% This is the main scanner state
-record(state,
	{
	  cstate,                %% client state
	  callback,              %% callback module
	  icase = false,         %% ignore case in tag names
	  ins = false,           %% do not strip name space
	  ns = false,            %% code name space as {Ns,Name}
	  encoding,              %% default to utf-8 charset
	  line  = 1,             %% line number
	  file  = "*stdin*",     %% file name
	  fdchunk = 512,         %% input chunk size
	  ibf_valid = true,      %% ibf valid flag
	  ibf  = fun ibf_auto/1  %% char scan function
	 }).

-define(toupper(C), if (C) >= $a, (C) =< $z -> ((C)-$a)+$A; true -> (C) end).
-define(tolower(C), if (C) >= $A, (C) =< $Z -> ((C)-$A)+$a; true -> (C) end).
-define(isblank(C), ((C)==?SP); ((C)==?TAB); ((C)==?CR)).

-define(CASE(St,C),
	if (St)#state.icase == true ->
		?tolower(C);
	   true ->
		(C)
	end).

%%
%% Scan File
%%
file(File) ->
    file(File,makeup_tags,[]).

file(File, CbMod) when is_atom(CbMod) ->
    file(File,CbMod,[]);
file(File, Opts) when is_list(Opts) ->
    file(File,makeup_tags,Opts).

file(File, CbMod, Opts) ->
    case file:open(File,[raw,read,binary]) of
	{ok, Fd} ->
	    St0 = #state { callback=CbMod, fdchunk=?DEFAULT_CHUNK_SIZE },
	    {XOpts, St1} = opts([{file,File}|Opts], [], St0),
 	    ?dbg("XOpts=~p\n", [XOpts]),
	    XOpts1 = [{icase,St1#state.icase},
		      {ins,St1#state.ins},
		      {ns,St1#state.ns},
		      {encoding,St1#state.encoding},
		      {file,File}|XOpts],
	    Res = (catch fd_start(Fd, St1#state.fdchunk, init("", XOpts1, St1))),
	    file:close(Fd),
	    case Res of
		{ok,Result,_St2} ->
		    Result;
		Error ->
		    Error
	    end;
	Error ->
	    Error
    end.

ifile(File, CState) ->
    ?dbg("IFILE: ~p\n", [File]),
    {St,Continue} = c_pop(),
    St1 = iifile(File, St#state { cstate=CState}),
    c_push(St1,Continue),
    St1#state.cstate.


iifile(File, St) ->
    case file:open(File,[raw,read,binary]) of
	{ok, Fd} ->
	    St1 = St#state { file=File, line=1 },
	    Res = (catch fd_start(Fd, St#state.fdchunk, data([],[],1,St1))),
	    file:close(Fd),
	    case Res of
		{ok, _Result, St2} ->
		    St2#state { line = St#state.line,file = St#state.file };
		Error ->
		    io:format("include ~s error ~p\n", [File, Error]),
		    St
	    end;
	Error ->
	    io:format("include error ~p\n", [Error]),
	    St
    end.

%% Scan string
string(Data) ->
    string(Data,makeup_tags,[]).

string(Data, CbMod) when is_atom(CbMod) ->
    string(Data,CbMod,[]);
string(Data, Opts) when is_list(Opts) ->
    string(Data,makeup_tags,Opts).

string(Data,CbMod,Opts) ->
    St0 = #state { callback = CbMod },
    {XOpts,St1} = opts(Opts, [], St0),
    XOpts1 = [{icase,St1#state.icase},
	      {ins,St1#state.ins},
	      {ns,St1#state.ns} |
	      XOpts],
    Res = if is_binary(Data) ->
		  bin_start(Data,St1#state.fdchunk,init("", XOpts1, St1));
	     true ->
		  Cont = init(Data,XOpts1,St1),
		  Cont(eof)
	  end,
    case Res of
	{ok,Result, _St2} ->
	    Result;
	Error ->
	    Error
    end.

%% Read the first lines of data
fd_start(Fd, N, Cont) ->
    fd(Fd, N, Cont).

fd(Fd, N, Cont) ->
    case file:read(Fd, N) of
	{ok,Data} ->
	    fd(Fd, N, Cont(Data));
	eof ->
	    Cont(eof);
	Error ->
	    Error
    end.

bin_start(Bin, N, Cont) ->
    bin(Bin, N, Cont).

bin(Bin,N,Cont) ->
    bin(Bin,0,size(Bin),N,Cont).

bin(Bin,Offset,Size,N,Cont) ->
    case Bin of
	<<_:Offset/binary, Data:N/binary, _/binary>> ->
	    bin(Bin,Offset+N,Size,N,Cont(binary_to_list(Data)));
	<<_:Offset/binary>> ->
	    Cont(eof);
	<<_:Offset/binary, Data/binary>> ->
	    bin(Bin,Offset+size(Data),Size,N,Cont(binary_to_list(Data)))
    end.

    
%% Called with client state.
istring(String, CState) ->
    ?dbg("ISTRING: ~p\n", [String]),
    {St,Continue} = c_pop(),
    St1 = iistring(String, St#state { cstate=CState}),
    c_push(St1,Continue),
    St1#state.cstate.

iistring(String, St) ->
    Cont = data(String,[],St#state.line,St),
    case Cont(eof) of
	{ok, _Result, St2} ->
	    St2#state { line = St#state.line, file = St#state.file };
	Error ->
	    io:format("include error ~p\n", [Error]),
	    St
    end.


%% Called with client state, update options
ioptions(Opts, CState) ->
    ?dbg("IOPTIONS: ~p\n", [Opts]),
    {St,Continue} = c_pop(),
    {_, St1} = opts(Opts, [], St#state { cstate=CState}),
    c_push(St1,Continue),
    St1#state.cstate.

ilevel() ->
    case get(?MAKEUP_STACK) of
	undefined -> 0;
	[] -> 0;
	Stack -> (length(Stack) div 2)
    end.


%% Process erlang coded tag format
%% May be used for validation etc
etags(Es) ->
    etags(Es,makeup_tags,[]).

etags(Es, CbMod) when is_atom(CbMod) ->
    etags(Es,CbMod,[]);
etags(Es, Opts) when is_list(Opts) ->
    etags(Es,makeup_tags,Opts).

etags(Es,CbMod,Opts) ->
    St0 = #state { callback = CbMod },
    {XOpts,St1} = opts(Opts, [], St0),
    c_init(XOpts,St1,
	   fun(St2) ->
		   St3 = etags1(Es, St2),
		   c_final([],0,St3)
	   end).

etags1([E|Es], St) ->
    etags(Es,etags1(E,St));
etags1([],St) ->
    St;
etags1({'#PCDATA',Text},St) ->
    call(text,Text,0,St,fun(St1) -> St1 end);
etags1({Tag,As,Cs},St0) ->
    St1 = etag_as(As,Tag,St0),
    call(tag_begin,Tag,0,St1,
	 fun(St2) ->
		 St3 = etags1(Cs,St2),
		 call(tag_end,Tag,0,St3,
		      fun(St4) -> St4 end)
	 end).

etag_as([{A,V}|As],Tag,St) ->
    call(attribute,Tag,A,V,0,St,
	 fun(St1) -> etag_as(As,Tag,St1) end);
etag_as([A|As],Tag,St) ->
    call(attribute,Tag,A,0,St,
	 fun(St1) -> etag_as(As,Tag,St1) end);
etag_as([],_Tag,St) ->
    St.
    
%%
%% Options
%%
opts([{Opt,Value} | Opts], XOpts, St) ->
    case Opt of
	chunk ->
	    if is_integer(Value), Value > 0 ->
		    opts(Opts, XOpts, St#state { fdchunk=Value })
	    end;
	icase ->
	    if Value==true; Value==false ->
		    opts(Opts, XOpts, St#state { icase = Value })
	    end;
	ins ->
	    if Value==true; Value==false ->
		    opts(Opts, XOpts, St#state { ins = Value })
	    end;
	ns ->
	    if Value==true; Value==false ->
		    opts(Opts, XOpts, St#state { ns = Value })
	    end;
	line  ->
	    if is_integer(Value) ->
		    opts(Opts, XOpts, St#state { line = Value })
	    end;
	file -> 
	   if is_list(Value) ->
		    opts(Opts, XOpts, St#state { file = Value })
	    end;
	charset ->  %% charset or encoding or both?
	    if is_list(Value) ->
		    opts(Opts, [{Opt,Value}|XOpts], set_encoding(Value, St))
	    end;
	encoding ->
	    if is_list(Value) ->
		    opts(Opts, XOpts, set_encoding(Value, St))
	    end;
	_ ->
	    opts(Opts, [{Opt,Value}|XOpts], St)
    end;
opts([], XOpts, St) ->
    {XOpts, St}.


tolower([C|Cs]) ->
    if (C) >= $A, (C) =< $Z -> 
	    [((C)-$A)+$a | tolower(Cs)];
        true -> 
	    [(C) | tolower(Cs)]
    end;
tolower([]) ->
    [].


%%
%% init state
%%
init(Cs,XOpts,St) ->
    c_init(XOpts,St,
	   fun(St1) ->
		   data0(input(Cs,St1),[],St1#state.line,St1)
	   end).

%%
%% Neutral data state
%% collecting CDATA text and looking for <
%% also translates entity encodings
%%

%% ignore white space between and before start tag
data0(Cs,Text,Ln,St) ->
    case Cs of
	[?LT|Cs1] ->
	    tdata(Cs1,[],Ln,St);
	[?AMP|Cs1] ->
	    char(Cs1,Ln,St,fun(Ref,Cs2,Ln1,St2) -> 
				   data(Cs2,Ref++Text,Ln1,St2)
			   end);
	[?CR] ->
	    fun(Data) -> data0(input(Data,[?CR],St),Text,Ln,St) end;
	[?CR,?NL|Cs1] -> %% normalize
	    data0(Cs1,[?NL|Text],Ln+1,St);
	[?CR|Cs1] ->      %% normalize
	    data0(Cs1,[?NL|Text],Ln+1,St);
	[?NL|Cs1] ->
	    data0(Cs1,[?NL|Text],Ln+1,St);
	[C|Cs1] ->
	    data(Cs1,[C|Text],Ln,St);
	[] ->
	    fun(Data) -> data0(input(Data,St),Text,Ln,St) end;
	{Cs1,St1} ->
	    data0(Cs1,Text,Ln,St1);
	eof ->
	    c_final("",Ln,St)
    end.
    
data(Cs,Text,Ln,St) ->
    case Cs of
	[?LT|Cs1] ->
	    tdata(Cs1,Text,Ln,St);
	[?AMP|Cs1] ->
	    char(Cs1,Ln,St,fun(Ref,Cs2,Ln1,St2) -> 
				   data(Cs2,Ref++Text,Ln1,St2) 
			   end);
	[?CR] ->
	    fun(Data) -> data(input(Data,[?CR],St),Text,Ln,St) end;
	[?CR,?NL|Cs1] -> %% normalize
	    data(Cs1,[?NL|Text],Ln+1,St);
	[?CR|Cs1] ->      %% normalize
	    data(Cs1,[?NL|Text],Ln+1,St);
	[?NL|Cs1] ->
	    data(Cs1,[?NL|Text],Ln+1,St);
	[C|Cs1] ->
	    data(Cs1,[C|Text],Ln,St);
	[] ->
	    fun(Data) -> data(input(Data,St),Text,Ln,St) end;
	{Cs1,St1} ->
	    data(Cs1,Text,Ln,St1);
	eof ->
	    c_text(Text,Ln,St,
		 fun(St1) -> c_final("",Ln,St1) end)
    end.

%% scan when < seen, PCDATA is data seen sofar
tdata(Cs,Text,Ln,St) ->
    case Cs of
	[$!,$-,$-|Cs1] ->
	    comment(Cs1,[],Text,Ln,St);
	[$!,$-] ->
	    fun(Data) -> tdata(input(Data,[$!,$-],St),Text,Ln,St) end;
	[$!,?LBRACKET|Cs1] ->
	    section(Cs1,[?LBRACKET],Text,Ln,St);
	[$!] ->
	    fun(Data) -> tdata(input(Data,[$!],St),Text,Ln,St) end;
	[$!|Cs1] ->
	    c_text(Text,Ln,St,
		 fun(St1) -> declaration(Cs1,[],Ln,St1) end);
	[$/|Cs1] ->
	    c_text(Text,Ln,St,
		 fun(St1) -> t_end(Cs1,[],Ln,St1) end);
	[$?|Cs1] ->
	    c_text(Text,Ln,St,
		 fun(St1) -> processing(Cs1,[],Ln,St1) end);
	[$\\|Cs1] -> 
	    %% not valid? - hack escape in scripts found
	    data(Cs1, [$\\,?LT|Text], Ln, St);
	[C|Cs1] ->
	    c_text(Text,Ln,St,
		 fun(St1) -> tag(Cs1,[?CASE(St,C)],Ln,St1) end);
	[] ->
	    fun(Data) -> tdata(input(Data,St),Text,Ln,St) end;
	{Cs1,St1} ->
	    tdata(Cs1,Text,Ln,St1);
	eof ->
	    c_text(Text,Ln,St,
		 fun(St1) -> c_final("",Ln,St1) end)
    end.

%%
%% Comment processing
%%  '<!--' <comment> '--' [<wsp>] '>'
%%
%% Should be extended to!
%%  '<!--' <comment> '--' [<data> '--' <comment>]* '--' <wsp> '>'
%%
comment(Cs,Acc,Text,Ln,St) ->
    case Cs of
	[$-,$-,?GT|Cs1] ->
	    %% fixme data | data0
	    call(comment,reverse(Acc),Ln,St,
		 fun(St1) -> data(Cs1,Text,Ln,St1) end);
	[$-,$-] ->
	    fun(Data) -> comment(input(Data,[$-,$-],St),Acc,Text,Ln,St) end;
	[$-] ->
	    fun(Data) -> comment(input(Data,[$-],St),Acc,Text,Ln,St) end;
	[?NL|Cs1] ->
	    comment(Cs1,[?NL|Acc],Text,Ln+1,St);
	[C|Cs1] ->
	    comment(Cs1,[C|Acc],Text,Ln,St);
	[] ->
	    fun(Data) -> comment(input(Data,St),Acc,Text,Ln,St) end;
	{Cs1,St1} ->
	    comment(Cs1,Acc,Text,Ln,St1);
	eof ->
	    c_text(Text,Ln,St,
		 fun(St1) -> c_final("<!--"++reverse(Acc),Ln,St1) end)
    end.


%%
%% tag end '<' <tag-name> <space> <attributes>  ['/'] '>'
%%
%% FIXME: allow initial space?
%%
tag(Cs,Acc,Ln,St) ->
    case Cs of
	[?GT|Cs1] ->
	    Tag = list_to_tag_name(reverse(Acc),St),
	    call(tag_begin,Tag,Ln,St,
		 fun(St1) -> data(Cs1, [], Ln, St1) end);
	[$/,?GT|Cs1] ->
	    Tag = list_to_tag_name(reverse(Acc),St),
	    call(tag_begin,Tag,Ln,St,
		 fun(St1) ->
			 call(tag_end,Tag,Ln,St1,
			      fun(St2) ->
				      data0(Cs1,[],Ln,St2)
			      end)
		 end);
	[$/] ->
	    fun(Data) -> tag(input(Data,[$/],St),Acc,Ln,St) end;

	[?NL|Cs1] ->
	    Tag = list_to_tag_name(reverse(Acc),St),
	    tag_attrs(Cs1,Tag,Ln+1,St);

	[C|Cs1] when ?isblank(C) ->
	    Tag = list_to_tag_name(reverse(Acc),St),
	    tag_attrs(Cs1,Tag,Ln,St);
	
	[C|Cs1] ->
	    tag(Cs1,[?CASE(St,C)|Acc],Ln,St);
	[] ->
	    fun(Data) -> tag(input(Data,St),Acc,Ln,St) end;
	{Cs1,St1} ->
	    tag(Cs1,Acc,Ln,St1);
	eof ->
	    c_final([?LT|reverse(Acc)],Ln,St)
    end.

%%
%% tag end '<' '/' [<tag-name>] '>'
%%
t_end(Cs,Acc,Ln,St) ->
    case Cs of
	[?GT|Cs1] ->
	    Tag = list_to_attribute_name(reverse(Acc),St),
	    call(tag_end,Tag,Ln,St,
		 fun(St1) -> data0(Cs1,[],Ln,St1) end);
	[?NL|Cs1] ->
	    t_end(Cs1,[?NL|Acc],Ln+1,St);
	[C|Cs1] ->
	    t_end(Cs1,[?CASE(St,C)|Acc],Ln,St);
	[] ->
	    fun(Data) -> t_end(input(Data,St),Acc,Ln,St) end;
	{Cs1,St1} ->
	    t_end(Cs1,Acc,Ln,St1);
	eof ->
	    c_final([?LT,$/|reverse(Acc)],Ln,St)
    end.


%%
%% tag attributes
%%
%%  <sp> <name> <sp> ['=' <sp> <value> ]
%%
tag_attrs(Cs,Tag,Ln,St) ->
    case Cs of
	[?GT|Cs1] ->
	    call(tag_begin,Tag,Ln,St,
		 fun(St1) -> data(Cs1,[],Ln,St1) end);
	[$/,?GT|Cs1] ->
	    %% XML standard
	    call(tag_begin,Tag,Ln,St,
		 fun(St1) ->
			 call(tag_end,Tag,Ln,St1,
			      fun(St2) ->
				      data0(Cs1,[],Ln,St2)
			      end)
		 end);
	[$/] ->
	    fun(Data) -> tag_attrs(input(Data,[$/],St),Tag,Ln,St) end;
	[?NL|Cs1] ->
	    tag_attrs(Cs1,Tag,Ln+1,St);
	[C|Cs1] when ?isblank(C) ->
	    tag_attrs(Cs1,Tag,Ln,St);
	[C|Cs1] ->
	    tag_attr(Cs1,Tag,[?CASE(St,C)],Ln,St);
	[] ->
	    fun(Data) -> tag_attrs(input(Data,St),Tag,Ln,St) end;
	{Cs1,St1} ->
	    tag_attrs(Cs1,Tag,Ln,St1);
	eof ->
	    c_final("",Ln,St)
    end.

%%
%% phrase <chars> [<sp>] '='
%%
tag_attr(Cs,Tag,Acc,Ln,St) ->
    case Cs of
	[?NL|Cs1] ->
	    Attr = list_to_attribute_name(reverse(Acc),St),
	    tag_valq(Cs1,Tag,Attr,Ln+1,St);
	[C|Cs1] when ?isblank(C) ->
	    Attr = list_to_attribute_name(reverse(Acc),St),
	    tag_valq(Cs1,Tag,Attr,Ln,St);
	[?GT|Cs1] ->
	    Attr = list_to_attribute_name(reverse(Acc),St),
	    call(attribute,Tag,Attr,Ln,St,
		 fun(St1) ->
			 call(tag_begin,Tag,Ln,St1,
			      fun(St2) -> data(Cs1,[],Ln,St2) end)
		 end);
	[$=|Cs1] ->
	    Attr = list_to_attribute_name(reverse(Acc),St),
	    tag_val(Cs1,Tag,Attr,Ln,St);
	[C|Cs1] ->
	    tag_attr(Cs1,Tag,[?CASE(St,C)|Acc],Ln,St);
	[] ->
	    fun(Data) -> tag_attr(input(Data,St),Tag,Acc,Ln,St) end;
	{Cs1,St1} ->
	    tag_attr(Cs1,Tag,Acc,Ln,St1);
	eof ->
	    c_final(reverse(Acc),Ln,St)
    end.
%%
%% Look for attribute value 
%%
tag_valq(Cs,Tag,Attr,Ln,St) ->
    case Cs of
	[$=|Cs1] ->
	    tag_val(Cs1,Tag,Attr,Ln,St);
	[?NL|Cs1] ->
	    tag_valq(Cs1,Tag,Attr,Ln+1,St);
	[C|Cs1] when ?isblank(C) ->
	    tag_valq(Cs1,Tag,Attr,Ln,St);
	[] ->
	    fun(Data) -> tag_valq(input(Data,St),Tag,Attr,Ln,St) end;
	{Cs1,St1} ->
	    tag_valq(Cs1,Tag,Attr,Ln,St1);
	eof ->
	    c_final("",Ln,St);
	_ ->
	    call(attribute,Tag,Attr,Ln,St,
		 fun(St1) -> tag_attrs(Cs,Tag,Ln,St1) end)
    end.

%%
%% Look for value
%%
tag_val(Cs,Tag,Attr,Ln,St) ->
    case Cs of
	[?NL|Cs1] ->
	    tag_val(Cs1,Tag,Attr,Ln+1,St);
	[C|Cs1] when ?isblank(C) ->
	    tag_val(Cs1,Tag,Attr,Ln,St);
	[?QUOT|Cs1] ->
	    tag_qvalue(Cs1,Tag,Attr,[],?QUOT,Ln,St);
	[?APOS|Cs1] ->
	    tag_qvalue(Cs1,Tag,Attr,[],?APOS,Ln,St);
	[C|Cs1] ->
	    tag_value(Cs1,Tag,Attr,[C],Ln,St);
	[] ->
	    fun(Data) -> tag_val(input(Data,St),Tag,Attr,Ln,St) end;
	{Cs1,St1} ->
	    tag_val(Cs1,Tag,Attr,Ln,St1);
	eof ->
	    c_final("",Ln,St)
    end.
%%
%% Unquoted tag value MUST be in range 
%%
tag_value(Cs,Tag,Attr,Acc,Ln,St) ->
    case Cs of
	[?NL|Cs1] ->
	    Value = reverse(Acc),
	    call(attribute,Tag,Attr,Value,Ln,St,
		 fun(St1) -> tag_attrs(Cs1,Tag,Ln+1,St1) end);
	[C|Cs1] when ?isblank(C) ->
	    Value = reverse(Acc),
	    call(attribute,Tag,Attr,Value,Ln,St,
		 fun(St1) -> tag_attrs(Cs1,Tag,Ln,St1) end);
	[?GT|_Cs1] ->
	    Value = reverse(Acc),
	    call(attribute,Tag,Attr,Value,Ln,St,
		 fun(St1) -> tag_attrs(Cs,Tag,Ln,St1) end);
	[C|Cs1] ->
	    tag_value(Cs1,Tag,Attr,[C|Acc],Ln,St);
	[] ->
	    fun(Data) -> tag_value(input(Data,St),Tag,Attr,Acc,Ln,St) end;
	{Cs1,St1} ->
	    tag_value(Cs1,Tag,Attr,Acc,Ln,St1);
	eof ->
	    c_final(reverse(Acc),Ln,St);
	Cs1 ->
	    Value = reverse(Acc),
	    call(attribute,Tag,Attr,Value,Ln,St,
		 fun(St1) -> tag_attrs(Cs1,Tag,Ln,St1) end)
    end.
%%
%% Quoted value either ' or "
%%
tag_qvalue(Cs,Tag,Attr,Acc,Q,Ln,St) ->
    case Cs of
	[Q|Cs1] ->
	    Value = reverse(Acc),
	    call(attribute,Tag,Attr,Value,Ln,St,
		 fun(St1) -> tag_attrs(Cs1,Tag,Ln,St1) end);
	[?NL|Cs1] ->
	    tag_qvalue(Cs1,Tag,Attr,[?NL|Acc],Q,Ln+1,St);
	[?AMP|Cs1] ->
	    char(Cs1,Ln,St,
		 fun(Ref,Cs2,Ln1,St2) ->
			 tag_qvalue(Cs2,Tag,Attr,Ref++Acc,Q,Ln1,St2) 
		 end);
	[C|Cs1] ->
	    tag_qvalue(Cs1,Tag,Attr,[C|Acc],Q,Ln,St);
	[] ->
	    fun(Data) -> tag_qvalue(input(Data,St),Tag,Attr,Acc,Q,Ln,St) end;
	{Cs1,St1} ->
	    tag_qvalue(Cs1,Tag,Attr,Acc,Q,Ln,St1);
	eof ->
	    c_final(reverse(Acc),Ln,St)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% <?<target> <chars> ?>
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
processing(Cs,Acc,Ln,St) ->
    case Cs of
	[$?,?GT|Cs1] ->
	    Target = list_to_atom(reverse(Acc)),
	    call(processing,Target,"",Ln,St,
		 fun(St1) -> data(Cs1,[],Ln,St1) end);
	[?NL|Cs1] ->
	    Target = list_to_atom(reverse(Acc)),
	    proc_value(Cs1,Target,[?NL],Ln+1,St);
	[C|Cs1] when ?isblank(C) ->
	    Target = list_to_atom(reverse(Acc)),
	    proc_value(Cs1,Target,[C],Ln,St);
	[C|Cs1] ->
	    processing(Cs1,[?CASE(St,C)|Acc],Ln,St);
	[] ->
	    fun(Data) -> processing(input(Data,St),Acc,Ln,St) end;
	{Cs1,St1} ->
	    processing(Cs1,Acc,Ln,St1);
	eof ->
	    c_final([?LT,$?|reverse(Acc)],Ln,St)
    end.

%%
%% processing value part
%%
%%  chars* ?
%%
proc_value(Cs,Target,Acc,Ln,St) ->
    case Cs of
	[?NL|Cs1] ->
	    proc_value(Cs1,Target,[?NL|Acc],Ln+1,St);
	[C|Cs1] when ?isblank(C) ->
	    proc_value(Cs1,Target,[C|Acc],Ln,St);
	[$?,?GT|Cs1] ->
	    call(processing,Target,reverse(Acc),Ln,St,
		 fun(St1) -> data(Cs1,[],Ln,St1) end);
	[$?] ->
	    fun(Data) -> proc_value(input(Data,[$?],St),Target,Acc,Ln,St) end;
	[C|Cs1] ->
	    proc_value(Cs1,Target,[C|Acc],Ln,St);
	[] ->
	    fun(Data) -> proc_value(input(Data,St),Target,Acc,Ln,St) end;
	{Cs1,St1} ->
	    proc_value(Cs1,Target,Acc,Ln,St1);
	eof ->
	    c_final("",Ln,St)
    end.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% <![ [<sp>*<name><sp>*] [ ..... ]]>
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

section(Cs,Acc,Text,Ln,St) ->
    case Cs of
	[?LBRACKET|Cs1] ->
	    c_text(Text,Ln,St,
		 fun(St1) -> section_data(Cs1,"",[],Ln,St1) end);
	[?NL|Cs1] ->
	    section(Cs1,[?NL|Acc],Text,Ln+1,St);
	[C|Cs1] when ?isblank(C) ->
	    section(Cs1,[C|Acc],Text,Ln,St);
	[C|Cs1] ->
	    section_name(Cs1,[C],Text,Ln,St);
	[] ->
	    fun(Data) -> section(input(Data,St),Acc,Text,Ln,St) end;
	{Cs1,St1} ->
	    section(Cs1,Acc,Text,Ln,St1);
	eof ->
	    c_text(Text,Ln,St,
		 fun(St1) -> c_final([?LT|reverse(Acc)],Ln,St1) end)
    end.

section_name(Cs,Acc,Text,Ln,St) ->
    case Cs of
	[?LBRACKET|Cs1] ->
	    case reverse(Acc) of
		"CDATA" ->
		    section_cdata(Cs1,Text,Ln,St);
		_ -> %% FIXME
		    c_text(Text,Ln,St,
			 fun(St1) ->
				 section_data(Cs1,reverse(Acc),[],Ln,St1)
			 end)
	    end;
	[?NL|Cs1] ->
	    section_name_end(Cs1,reverse(Acc),[],Text,Ln+1,St);
	[C|Cs1] when ?isblank(C) ->
	    section_name_end(Cs1,reverse(Acc),[],Text,Ln,St);
	[C|Cs1] ->
	    section_name(Cs1,[C|Acc],Text,Ln,St);
	[] ->
	    fun(Data) -> section_name(input(Data,St),Acc,Text,Ln,St) end;
	{Cs1,St1} ->
	    section_name(Cs1,Acc,Text,Ln,St1);
	eof ->
	    c_text(Text,Ln,St,
		 fun(St1) ->
			 c_final([?LT,?LBRACKET|reverse(Acc)],Ln,St1)
		 end)
    end.

section_name_end(Cs,Name,Acc,Text,Ln,St) ->
    case Cs of
	[?LBRACKET|Cs1] ->
	    case Name of
		"CDATA" ->
		    section_cdata(Cs1,Text,Ln,St);
		_ ->
		    %% FIXME: can not really emit text here...
		    c_text(Text,Ln,St,
			 fun(St1) ->
				 section_data(Cs1,Name,[],Ln,St1)
			 end)
	    end;
	[?NL|Cs1] ->
	    section_name_end(Cs1,Name,[?NL|Acc],Text,Ln+1,St);
	[C|Cs1] when ?isblank(C) ->
	    section_name_end(Cs1,Name,[C|Acc],Text,Ln,St);
	[?RBRACKET,?GT | Cs1] ->
	    case Name of
		"CDATA" ->
		    data(Cs1,Text,Ln,St);
		true ->
		    c_text(Text,Ln,St,
			 fun(St1) ->
				 call(section, Name, "",Ln, St1,
				      fun(St2) -> data(Cs1,[],Ln,St2) end)
			 end)
	    end;
	[?RBRACKET] ->
	    fun(Data) -> 
		    section_name_end(input(Data,[?RBRACKET],St),
				     Name,Acc,Text,Ln,St) end;
	[C|_Cs1] ->
	    c_text(Text,Ln,St,
		 fun(St1) ->
			 c_final(reverse([C|Acc]),Ln,St1)
		 end);
	[] ->
	    fun(Data) -> 
		    section_name_end(input(Data,St),
				     Name,Acc,Text,Ln,St) end;
	{Cs1,St1} ->
	    section_name_end(Cs1,Name,Acc,Text,Ln,St1);
	eof ->
	    c_text(Text,Ln,St,
		 fun(St1) ->
			 c_final([?LT,?LBRACKET|reverse(Acc)],Ln,St1)
		 end)
    end.

section_cdata(Cs,Text,Ln,St) ->
    case Cs of
	[?RBRACKET,?RBRACKET,?GT | Cs1] ->
	    %% FIXME: data | data0
	    data(Cs1,Text,Ln,St);
	[?RBRACKET] ->
	    fun(Data) -> section_cdata(input(Data,[?RBRACKET],St),
				       Text,Ln,St) end;
	[?RBRACKET,?RBRACKET] ->
	    fun(Data) -> section_cdata(input(Data,[?RBRACKET,?RBRACKET],St),
				       Text,Ln,St) end;
	[?NL|Cs1]  ->
	    section_cdata(Cs1,[?NL|Text],Ln+1,St);
	[C|Cs1] ->
	    section_cdata(Cs1,[C|Text],Ln,St);
	[] ->
	    fun(Data) -> section_cdata(input(Data,St),Text,Ln,St) end;
	{Cs1,St1} ->
	    section_cdata(Cs1,Text,Ln,St1);
	eof ->
	    c_text(Text,Ln,St,
		 fun(St1) -> c_final("",Ln,St1) end)
    end.

section_data(Cs,Name,Acc,Ln,St) ->
    case Cs of
	[?RBRACKET,?RBRACKET,?GT | Cs1] ->
	    %% FIXME: data | data0
	    call(section, Name, reverse(Acc),Ln,St,
		 fun(St1) -> data(Cs1,[],Ln,St1) end);
	[?RBRACKET] ->
	    fun(Data) -> section_data(input(Data,[?RBRACKET],St),
				      Name,Acc,Ln,St) end;
	[?RBRACKET,?RBRACKET] ->
	    fun(Data) -> section_data(input(Data,[?RBRACKET,?RBRACKET],St),
				      Name,Acc,Ln,St) end;
	[?NL|Cs1] ->
	    section_data(Cs1,Name,[?NL|Acc],Ln+1,St);
	[C|Cs1] ->
	    section_data(Cs1,Name,[C|Acc],Ln,St);
	[] ->
	    fun(Data) -> section_data(input(Data,St),Name,Acc,Ln,St) end;
	{Cs1,St1} ->
	    section_data(Cs1,Name,Acc,Ln,St1);
	eof ->
	    c_final(reverse(Acc),Ln,St)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%    
%% <!<name> [ <value> | '--' <comment> '--' ]* [ '[' section-data ']' '>'
%%
%% Assume that name is case sensitive ???
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
declaration(Cs,Acc,Ln,St) ->
    case Cs of
	[C|Cs1] when ?isblank(C) ->
	    Tag = list_to_atom(reverse(Acc)),
	    decl_params(Cs1,Tag,Ln,St);
	
	[?NL|Cs1] ->
	    Tag = list_to_atom(reverse(Acc)),
	    decl_params(Cs1,Tag,Ln+1,St);

	[C|Cs1] ->
	    declaration(Cs1,[C|Acc],Ln,St);
	[] ->
	    fun(Data) -> declaration(input(Data,St),Acc,Ln,St) end;
	{Cs1,St1} ->
	    declaration(Cs1,Acc,Ln,St1);
	eof ->
	    c_final([?LT,$!|reverse(Acc)],Ln,St)
    end.
    
decl_params(Cs,Tag,Ln,St) ->
    case Cs of
	[?NL|Cs1] ->
	    decl_params(Cs1,Tag,Ln+1,St);
	[C|Cs1] when ?isblank(C) ->
	    decl_params(Cs1,Tag,Ln,St);
	[$-,$-|Cs1] ->
	    decl_comment(Cs1,Tag,[],Ln,St);
	[$-] ->
	    fun(Data) -> decl_params(input(Data,[$-],St),Tag,Ln,St) end;
	[?QUOT|Cs1] ->
	    decl_qvalue(Cs1,Tag,[?QUOT],?QUOT,Ln,St);
	[?APOS|Cs1] ->
	    decl_qvalue(Cs1,Tag,[?APOS],?APOS,Ln,St);
	[?LBRACKET|Cs1] ->
	    decl_qvalue(Cs1,Tag,[?LBRACKET],?RBRACKET,Ln,St);
	[?GT|Cs1] ->
	    %% FIXME: data | data0
	    call(declaration,Tag,Ln,St,
		 fun(St1) -> data(Cs1,[],Ln,St1) end);
	[C|Cs1] ->
	    decl_value(Cs1,Tag,[C],Ln,St);
	[] ->
	    fun(Data) -> decl_params(input(Data,St),Tag,Ln,St) end;
	{Cs1,St1} ->
	    decl_params(Cs1,Tag,Ln,St1);
	eof ->
	    c_final("",Ln,St)
    end.

decl_comment(Cs,Tag,Acc,Ln,St) ->
    case Cs of
	[$-,$-|Cs1] ->
	    call(dcomment,Tag,reverse(Acc),Ln,St,
		 fun(St1) -> decl_params(Cs1,Tag,Ln,St1) end);
	[$-] ->
	    fun(Data) -> decl_comment(input(Data,[$-],St),Tag,Acc,Ln,St) end;
	[?NL | Cs1] -> 
	    decl_comment(Cs1,Tag,[?NL|Acc],Ln+1,St);
	[C | Cs1] -> 
	    decl_comment(Cs1,Tag,[C|Acc],Ln,St);
	[] ->
	    fun(Data) -> decl_comment(input(Data,St),Tag,Acc,Ln,St) end;
	{Cs1,St1} ->
	    decl_comment(Cs1,Tag,Acc,Ln,St1);
	eof ->
	    c_final(reverse(Acc),Ln,St)  
    end.


decl_value(Cs,Tag,Acc,Ln,St) ->
    case Cs of
	[C|Cs1] when ?isblank(C) ->
	    call(dparam,Tag,reverse(Acc),Ln,St,
		 fun(St1) -> decl_params(Cs1,Tag,Ln,St1) end);
	[?NL|Cs1] -> 
	    call(dparam,Tag,reverse(Acc),Ln,St,
		 fun(St1) -> decl_params(Cs1, Tag,Ln+1,St1) end);
	[?GT|_Cs1] ->
	    call(dparam,Tag,reverse(Acc),Ln,St,
		 fun(St1) -> decl_params(Cs,Tag,Ln,St1) end);
	[C|Cs1] ->
	    decl_value(Cs1,Tag,[C|Acc],Ln,St);
	[] ->
	    fun(Data) -> decl_value(input(Data,St),Tag,Acc,Ln,St) end;
	{Cs1,St1} ->
	    decl_value(Cs1,Tag,Acc,Ln,St1);
	eof ->
	    c_final(reverse(Acc),Ln,St)
    end.

decl_qvalue(Cs,Tag,Acc,EndQ,Ln,St) ->
    case Cs of
	[EndQ|Cs1] ->
	    Value = reverse([EndQ|Acc]),
	    call(dparam,Tag,Value,Ln,St,
		 fun(St1) -> decl_params(Cs1,Tag,Ln,St1) end);
	[?NL|Cs1]  ->
	    decl_qvalue(Cs1,Tag,[?NL|Acc],EndQ,Ln+1,St);
	[C|Cs1] ->
	    decl_qvalue(Cs1,Tag,[C|Acc],EndQ,Ln,St);
	[] ->
	    fun(Data) -> decl_qvalue(input(Data,St),Tag,Acc,EndQ,Ln,St) end;
	{Cs1,St1} ->
	    decl_qvalue(Cs1,Tag,Acc,EndQ,Ln,St1);
	eof ->
	    c_final(reverse(Acc),Ln,St)
    end.


%%
%% Parse character reference
%%
char(Cs,Ln,St,Continue) ->
    case Cs of
	[$#,$x|Cs1] ->
	    charhex(Cs1,[],Ln,St,Continue);
	[$#,C|Cs1] when C >= $0, C =< $9 ->
	    chardec(Cs1,[C],Ln,St,Continue);
	[$#] ->
	    fun(Data) -> char(input(Data,St),Ln,St,Continue) end;

	[C|Cs1] when C >= $a, C =< $z; C >= $A, C =< $Z; 
	             C >= $0, C =< $9 ->
	    charref(Cs1,[C],Ln,St,Continue);
	{Cs1,St1} ->
	    char(Cs1,Ln,St1,Continue);
	[] ->
	    fun(Data) -> char(input(Data,St),Ln,St,Continue) end;
	_ ->
	    Continue([?AMP],Cs,Ln,St)
    end.

chardec(Cs,Acc,Ln,St,Continue) ->
    case Cs of
	[C|Cs1] when C >= $0, C =< $9 ->
	    chardec(Cs1,[C|Acc],Ln,St,Continue);
	[$;|Cs1] ->
	    Continue([erlang:list_to_integer(reverse(Acc))],Cs1,Ln,St);
	[] ->
	    fun(Data) -> chardec(input(Data,St),Acc,Ln,St,Continue) end;
	{Cs1,St1} ->
	    chardec(Cs1,Acc,Ln,St1,Continue);
	_ ->
	    Continue(Acc++[$#,?AMP],Cs,Ln,St)
    end.

charhex(Cs,Acc,Ln,St,Continue) ->
    case Cs of
	[C|Cs1] when C >= $0, C =< $9 ->
	    charhex(Cs1,[C|Acc],Ln,St,Continue);
	[C|Cs1] when C >= $a, C =< $f ->
	    charhex(Cs1,[C|Acc],Ln,St,Continue);
	[C|Cs1] when C >= $A, C =< $F ->
	    charhex(Cs1,[C|Acc],Ln,St,Continue);
	[$;|Cs1] ->
	    Continue([erlang:list_to_integer(reverse(Acc),16)],Cs1,Ln,St);
	[] ->
	    fun(Data) -> charhex(input(Data,St),Acc,Ln,St,Continue) end;
	{Cs1,St1} ->
	    charhex(Cs1,Acc,Ln,St1,Continue);
	_ ->
	    Continue(Acc++[$#,?AMP],Cs,Ln,St)
    end.

charref(Cs,Acc,Ln,St,Continue) ->
    case Cs of
	[C|Cs1] when ?is_lower(C); ?is_upper(C); ?is_digit(C); C == ?DOT ->
	    charref(Cs1,[C|Acc],Ln,St,Continue);
	[$;|Cs1] ->
	    Ref = reverse(Acc),
	    case c_charref(Ref,Ln,St) of
		undefined ->
		    Continue([$;|Acc]++[?AMP],Cs,Ln,St);
		Chars ->
		    Continue(reverse(Chars),Cs1,Ln,St)
	    end;
	[] ->
	    fun(Data) -> charref(input(Data,St),Acc,Ln,St,Continue) end;
	{Cs1,St1} ->
	    charref(Cs1,Acc,Ln,St1,Continue);
	_ ->
	    Continue(Acc++[?AMP],Cs,Ln,St)
    end.

%% push current state 
c_push(St,Continue) ->
    case get(?MAKEUP_STACK) of
	undefined ->
	    put(?MAKEUP_STACK, [Continue,St]);
	Stack ->
	    put(?MAKEUP_STACK, [Continue,St|Stack])
    end.

	     
c_pop() ->
    [Continue,St|Stack] = get(?MAKEUP_STACK),
    put(?MAKEUP_STACK, Stack),
    {St,Continue}.

c_continue(CSt) ->
    [Continue,St|Stack] = get(?MAKEUP_STACK),
    put(?MAKEUP_STACK, Stack),
    Continue(St#state { cstate=CSt }).


    

c_final(Buf,Ln,St) ->
    c_push(St,fun(St1) -> St1 end),
    Result = (St#state.callback):final(Buf,Ln,St#state.cstate),
    {St1,_Continue} = c_pop(),
    {ok, Result, St1}.

c_init(XOpts,St,Continue) ->
    c_push(St, Continue),
    CSt = (St#state.callback):init(XOpts),
    c_continue(CSt).

c_text(Text,Ln,St,Continue) ->
    call(text,reverse(Text),Ln,St,Continue).

%% Handle basic SGML character references
%% This call back will return wither undefined
%% or a list of characters
c_charref(Char,Ln,St) ->
    ?dbg("~s:~w ~p\n", [charref,Ln,Char]),
    case Char of
	"lt"   -> [?LT];
	"gt"   -> [?GT];
	"amp"  -> [?AMP];
	"apos" -> [?APOS];
	"quot" -> [?QUOT];
	_ ->
	    (St#state.callback):charref(Char,Ln,St#state.cstate)
    end.


%% text, tag_begin, tag_end, comment, processing, declaration
call(Where,What,Ln,St,Continue) ->
    c_push(St, Continue),
    CSt = (St#state.callback):Where(What,Ln,St#state.cstate),
    c_continue(CSt).

%% attribute, param, section, dparam
call(Where,What,Arg,Ln,St,Continue) ->
    c_push(St, Continue),
    CSt = (St#state.callback):Where(What, Arg, Ln, St#state.cstate),
    c_continue(CSt).

%% attribute, param, 
call(Where,What,Arg,Val,Ln,St,Continue) ->
    c_push(St, Continue),
    CSt = (St#state.callback):Where(What, Arg, Val, Ln, St#state.cstate),
    c_continue(CSt).


list_to_tag_name(Name,St) ->
    if St#state.ins == true ->
	    case name_ns(Name,true) of
		[_Ns|Nm] -> Nm;
		Nm -> Nm
	    end;
       true ->
	    name_ns(Name,St#state.ns)
    end.

list_to_attribute_name(Name,St) ->
    if St#state.ins == true ->
	    case name_ns(Name,true) of
		[_Ns|Nm] -> Nm;
		Nm -> Nm
	    end;
       true ->
	    name_ns(Name,St#state.ns)
    end.

%%
%% Encode a tag name or an attribute name
%% Convert the Tag or attributes name into either
%%  [Ns|Nm] (UseNs) (dotted pair notation)
%%  Nm      (when not UseNs)
%%
name_ns(Name,UseNs) ->
    case string:chr(Name, $:) of
	0 ->
	    list_to_atom(Name);
	I when UseNs == true ->
	    {Ns,[_|Nm]} = lists:split(I-1,Name),
	    [list_to_atom(Ns)|list_to_atom(Nm)];
	_ when UseNs == false ->
	    list_to_atom(Name)
    end.
    
%%
%% input buffer:
%%
%% return:
%%     list of characters
%% or 
%%     {list-of-characters, St'}
%% 
%% where list-of_characters is possibly wide characters
%%

input(Buf,St) -> 
    input(Buf,[],St).

input(eof,_,_St) -> 
    eof;
input(Buf,PushBack,St) ->
    {Valid, Us, Cont} = (St#state.ibf)(Buf),
    Valid1 = Valid andalso St#state.ibf_valid,
    {PushBack++Us, St#state { ibf=Cont, ibf_valid=Valid1 }}.

%%
%% Set input encoding and the ibf/2 (input function)
%%
set_encoding(CharSet, St) ->
    Encoding = tolower(CharSet),
    IBF = ibf(Encoding),
    ?dbg("set encoding to ~p\n",[Encoding]),
    St#state { encoding = Encoding, ibf = IBF }.


ibf(Charset) ->
    case Charset of
	"auto" ->      fun ibf_auto/1;
	"utf-8" ->     fun(Data) -> makeup_utf8:input(Data) end;
	"utf-16" ->    fun (Data) -> makeup_utf16:input(Data) end;
	"utf-16be" ->  fun (Data) -> makeup_utf16_be:input(Data) end;
	"utf-16le" ->   fun (Data) -> makeup_utf16_le:input(Data) end;
	"utf-32"   ->   fun (Data) -> makeup_utf32:input(Data) end;
	"utf-32be" ->   fun (Data) -> makeup_utf32_be:input(Data) end;
	"utf-32le" ->   fun (Data) -> makeup_utf32_le:input(Data) end;
	"us-ascii" ->   fun (Data) -> makeup_ascii:input(Data) end;
	"iso-8859-1" -> fun (Data) -> makeup_iso_8859_1:input(Data) end;
	"iso-8859-2" -> fun (Data) -> makeup_iso_8859_2:input(Data) end;
	"iso-8859-3" -> fun (Data) -> makeup_iso_8859_3:input(Data) end;
	"iso-8859-4" -> fun (Data) -> makeup_iso_8859_4:input(Data) end;
	"iso-8859-5" -> fun (Data) -> makeup_iso_8859_5:input(Data) end;
	"iso-8859-6" -> fun (Data) -> makeup_iso_8859_6:input(Data) end;
	"iso-8859-7" -> fun (Data) -> makeup_iso_8859_7:input(Data) end;
	"iso-8859-8" -> fun (Data) -> makeup_iso_8859_8:input(Data) end;
	"iso-8859-9" -> fun (Data) -> makeup_iso_8859_9:input(Data) end;
	_ -> fun(Data) -> makeup_utf8:input(Data) end
    end.

%%
%% BOM autodetect
%%
ibf_auto(Cs) ->
    case Cs of
	[] ->
	    ?dbg("ibf_auto: empty none selected\n", []),
	    {true, [], fun ibf_auto/1};
	[16#00,16#00,16#FE,16#FF|Cs1] -> 
	    ?dbg("ibf_auto: BOM utf32_be selected\n", []),
	    makeup_utf32_be:input(Cs1);
	[16#FF,16#FE,16#00,16#00|Cs1] ->
	    ?dbg("ibf_auto: BOM utf32_le selected\n", []),
	    makeup_utf32_le:input(Cs1);
	[16#FF,16#FE|Cs1]             ->
	    ?dbg("ibf_auto: BOM utf16_be selected\n", []),
	    makeup_utf16_be:input(Cs1);
	[16#FE,16#FF|Cs1]             ->
	    ?dbg("ibf_auto: BOM utf16_le selected\n", []),
	    makeup_utf16_le:input(Cs1);
	[16#EF,16#BB,16#BF|Cs1]       ->
	    ?dbg("ibf_auto: BOM utf8 selected\n", []),
	    makeup_utf8:input(Cs1);

	<<>> ->
	    ?dbg("ibf_auto: empty none selected\n", []),
	    {true, [], fun ibf_auto/1};
	<<16#00,16#00,16#FE,16#FF,Cs1/binary>> ->
	    ?dbg("ibf_auto: BOM utf32_be selected\n", []),
	    makeup_utf32_be:input(Cs1);
	<<16#FF,16#FE,16#00,16#00,Cs1/binary>> ->
	    ?dbg("ibf_auto: BOM utf32_le selected\n", []),
	    makeup_utf32_le:input(Cs1);
	<<16#FF,16#FE,Cs1/binary>>             ->
	    ?dbg("ibf_auto: BOM utf16_be selected\n", []),
	    makeup_utf16_be:input(Cs1);
	<<16#FE,16#FF,Cs1/binary>>             ->
	    ?dbg("ibf_auto: BOM utf16_le selected\n", []),
	    makeup_utf16_le:input(Cs1);
	<<16#EF,16#BB,16#BF,Cs1/binary>>       ->
	    ?dbg("ibf_auto: BOM utf8 selected\n", []),
	    makeup_utf8:input(Cs1);

	%% Try some common none BOM patterns
	[16#00,16#00,16#00,$<|_]      -> 
	    ?dbg("ibf_auto: TEXT utf32_be selected\n", []),
	    makeup_utf32_be:input(Cs);
	[$<,16#00,16#00,16#00|_]      ->
	    ?dbg("ibf_auto: TEXT utf32_le selected\n", []),
	    makeup_utf32_le:input(Cs);
	[16#00,$<,16#00,$?|_]         ->
	    ?dbg("ibf_auto: TEXT utf16_be selected\n", []),
	    makeup_utf16_be:input(Cs);
	[$<,16#00,$?,16#00|_]         ->
	    ?dbg("ibf_auto: TEXT utf16_le selected\n", []),
	    makeup_utf16_le:input(Cs);
	[16#4C,16#6F,16#A7,16#94|_]   ->
	    ?dbg("ibf_auto: ebcdic selected\n", []),
	    makeup_ebcdic:input(Cs);

	<<16#00,16#00,16#00,$<,_/binary>>      ->
	    ?dbg("ibf_auto: TEXT utf32_be selected\n", []),
	    makeup_utf32_be:input(Cs);
	<<$<,16#00,16#00,16#00,_/binary>>      ->
	    ?dbg("ibf_auto: TEXT utf32_le selected\n", []),
	    makeup_utf32_le:input(Cs);
	<<16#00,$<,16#00,$?,_/binary>>         ->
	    ?dbg("ibf_auto: TEXT utf16_be selected\n", []),
	    makeup_utf16_be:input(Cs);
	<<$<,16#00,$?,16#00,_/binary>>         ->
	    ?dbg("ibf_auto: TEXT utf16_le selected\n", []),
	    makeup_utf16_le:input(Cs);
	<<16#4C,16#6F,16#A7,16#94,_/binary>>   ->
	    ?dbg("ibf_auto: ebcdic selected\n", []),
	    makeup_ebcdic:input(Cs);

	_ ->
	    ?dbg("ibf_auto: DEFAULT to utf8\n", []),
	    makeup_utf8:input(Cs)
    end.
   
