%%% File    : makeup_stream.erl
%%% Author  : Tony Rogvall <tony@rogvall.se>
%%% Description : Generate Tag tokens as a reentrent stream
%%% Created : 29 Jan 2009 by Tony Rogvall <tony@rogvall.se>

-module(makeup_stream).

-include("../include/makeup.hrl").

-import(lists, [reverse/1,map/2, foldl/3, foreach/2, member/2,append/1]).
%% Parser interface
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
	  text="",    %% current text value
	  alist=[],   %% current attribute list
	  tokens=[]   %% collected tokens
	 }).

init(_Opts) ->
    ?dbg("INIT: ~p\n", [_Opts]),
    #cstate{}.

final(_Buf,_Line,CSt) ->
    ?dbg("FINAL:~w: ~s\n", [_Line,_Buf]),
    reverse(CSt#cstate.tokens).

text(Text,_Line,CSt) ->
    ?dbg("TEXT:~w: ~p\n", [_Line,Text]),
    CSt#cstate { text=CSt#cstate.text ++ Text }.    

tag_begin(Tag, Line, CSt) ->
    ?dbg("<~p>:~w\n", [Tag,_Line]),
    add_token({Tag, Line, CSt#cstate.alist}, CSt).


tag_end(Tag, Line, CSt) ->
    ?dbg("</~p>:~w\n", [Tag,Line]),
    ETag = list_to_atom([$/|atom_to_list(Tag)]),
    add_token({ETag, Line, []}, CSt).

attribute(_Tag, Attr, _Line, CSt) ->
    ?dbg("ATTRIBUTE:~w ~p ~p\n", [_Line,_Tag,Attr]),
    As = [Attr|CSt#cstate.alist],
    CSt#cstate { alist = As }.

attribute(_Tag, Attr, Value, _Line, CSt) ->
    ?dbg("ATTRIBUTE:~w ~p ~p=~p\n", [_Line,_Tag,Attr,Value]),
    As = [{Attr,Value}|CSt#cstate.alist],
    CSt#cstate { alist = As }.
  
processing(Target, Value, Line, CSt) ->
    ?dbg("PROCESSING::~w ~s\n", [Line,_Target]),
    KeyVal = makeup:keyval(Value),
    Tag = list_to_atom([$?|atom_to_list(Target)]),
    add_token({Tag, Line, KeyVal}, CSt).

declaration(Decl, Line, CSt) ->
    Tag = list_to_atom([$!|atom_to_list(Decl)]),
    add_token({Tag, Line, CSt#cstate.alist}, CSt).

dparam(_Tag, Value, Line, CSt) ->
    ?dbg("DPARAM:~w ~s ~s\n", [Line,_Tag,Value]),
    As = [{Line,Value}|CSt#cstate.alist],
    CSt#cstate { alist = As }.

dcomment(_Tag,_Comment,_Line,CSt) ->
    ?dbg("COMMENT:~w ~s ~s\n", [_Line,_Tag,_Comment]),
    CSt.

section(_Tag,_Data, _Line, CSt) ->
    ?dbg("SECTION:~w: ~s ~s\n", [_Line,_Tag,_Data]),
    CSt.

comment(_Comment, _Line,CSt) ->
    ?dbg("COMMENT:~w ~s\n", [_Line,_Comment]),
    CSt.

charref(_Ref, _Line, _St) ->
    ?dbg("CHARREF:~w ~s\n", [_Line,_Ref]),
    undefined.

%% Collect #PCDATA and add token 
add_token(Token, CSt) ->
    CSt1 = add_text(CSt, element(2,Token)),
    CSt1#cstate { tokens=[Token|CSt1#cstate.tokens] }.
	    
add_text(CSt,Line) ->
    case text_collapse(CSt#cstate.text) of
	"" ->
	    CSt#cstate { text="" };
	Text -> 
	    CSt#cstate { tokens=[{'#PCDATA',Line,Text}|CSt#cstate.tokens],
			 text="" }
    end.
			   

%% replace,collapse and reverse
text_collapse(Cs) ->
    case Cs of
	[?SP|Cs1]  -> text_collapse(Cs1);
	[?TAB|Cs1] -> text_collapse(Cs1);
	[?CR|Cs1]  -> text_collapse(Cs1);
	[?NL|Cs1]  -> text_collapse(Cs1);
	[C|Cs1]    -> text_collapse(Cs1,[C]);
	[] -> []
    end.

text_collapse(Cs,Acc) ->
    case Cs of
	[?SP|Cs1]  -> text_collapse_sp(Cs1,[?SP|Acc]);
	[?TAB|Cs1] -> text_collapse_sp(Cs1,[?SP|Acc]);
	[?CR|Cs1]  -> text_collapse_sp(Cs1,[?SP|Acc]);
	[?NL|Cs1]  -> text_collapse_sp(Cs1,[?SP|Acc]);
	[C|Cs1]    -> text_collapse(Cs1, [C|Acc]);
	[] -> reverse(Acc)
    end.

text_collapse_sp(Cs,Acc) ->
    case Cs of
	[?SP|Cs1]  -> text_collapse_sp(Cs1,Acc);
	[?TAB|Cs1] -> text_collapse_sp(Cs1,Acc);
	[?CR|Cs1]  -> text_collapse_sp(Cs1,Acc);
	[?NL|Cs1]  -> text_collapse_sp(Cs1,Acc);
	[C|Cs1] -> text_collapse(Cs1, [C|Acc]);
	[] ->
	    case Acc of
		[?SP|Acc1] ->
		    reverse(Acc1);
		_ ->
		    reverse(Acc)
	    end
    end.

    
			 
    
    
    


