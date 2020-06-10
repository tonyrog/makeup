%%% File    : makeup_tags.erl
%%% Author  : Tony Rogvall <tony@PBook.local>
%%% Description : Generic tag parser XML/HTML etc
%%% Created : 19 Apr 2006 by Tony Rogvall <tony@PBook.local>

-module(makeup_tags).

-import(lists, [reverse/1,map/2, foldl/3, foreach/2, member/2,append/1]).

-include("../include/makeup.hrl").

%% Parser interface
-export([init/1, final/3, 
	 text/3, tag_begin/3, tag_end/3,
	 attribute/4, attribute/5, processing/4,
	 declaration/3, dparam/4, dcomment/4, 
	 section/4, comment/3,
	 charref/3]).

-define(ERROR, -1).
-define(EXCLUDE, -2).

-define(DEFAULT_PREFIX, '$default').
-define(DEFAULT_NS_MAP, [{xml,'http://www.w3.org/XML/1998/namespace'}]).

%% stack element
-record(celem,
	{
	  tag,        %% tag
	  ns,         %% tag's namespace
	  alist,      %% attributes
	  flags,      %% flags
	  sta,        %% state
	  inc,        %% includes
	  exc,        %% excludes
	  lang,       %% language
	  ns_map      %% namespace map
	 }).

%% "static" part of cstate 
-record(copt,
	{
	  file,            %% input file name
	  start,           %% optional start tag
	  doctype,         %% doctype detected
	  encoding,        %% input charset encoding detected/used
	  dtd,             %% current validation module
	  dtd_tab,         %% ets table | undefined (included sections)
	  dtd_style=plain  %% plain|record
	 }).

%% makeup client state
-record(cstate,
	{
	  opt,               %% #copt {}
	  flags = 0,         %% various options (space,ns..)
	  lang,              %% current language
	  ns_map = [],       %% Current namespace mappings
	  alist = [],        %% Current attribute list
	  action_stack = [], %% Element Push/Pop Action Stack
	  stack        = []  %% stack of #celem 
	 }).

%% tag flag definitions
-define(SPACE_MASK,           16#0003).
-define(SPACE_PRESERVE,      16#0000).
-define(SPACE_REPLACE,       16#0001).
-define(SPACE_COLLAPSE,      16#0002).
-define(SPACE_NORMALIZE,     16#0003).

-define(SPACE_DEFAULT_MASK,   16#000C).

-define(FLAG_USE_XML,     16#00010).     %% use XML processing
-define(FLAG_USE_NS,      16#00020).     %% use Namespace processing
-define(FLAG_USE_STRICT,  16#00040).     %% do not try repair recover
-define(FLAG_VALIDATE,    16#00080).     %% validate the document
-define(FLAG_IGNORE_CASE, 16#00100).     %% ignore case
-define(FLAG_IGNORE_NS,   16#00200).     %% ignore namespace

-define(FLAG_IS_VALID,    16#01000).     %% document is "still" valid
-define(FLAG_IS_XML,      16#02000).     %% <?xml ...> has been seen
-define(FLAG_USE_DTD,     16#04000).     %% Either DTD or DTD_TAB has been set
-define(FLAG_USE_PI,      16#08000).     %% run pi into action command
-define(FLAG_USE_COMMENT, 16#10000).     %% run comments into action command

-define(FLAG_STICKY,      16#1000).     %% preserved flags

-define(GET_FLAG(Flags,Mask),((Flags) band (Mask))).

-define(IS_FLAG_SET(Flags,Flag),
	(?GET_FLAG((Flags),(Flag)) =:= (Flag))).

-define(ANY_FLAGS_SET(Flags,FlagMask),
	(?GET_FLAG((Flags),(FlagMask)) =/= 0)).

-define(IS_FLAG_CLR(Flags,Flag),
	(?GET_FLAG((Flags),(Flag)) =:= 0)).

-define(CLR_FLAG(Flags,Mask),
	((Flags) band (bnot (Mask)))).

-define(SET_FLAG_MASK(Flags,Mask,Flag),
	(?CLR_FLAG(Flags,Mask) bor (Flag))).

-define(SET_FLAG(Flags, Flag),
	((Flags) bor (Flag))).

-define(IFILE(Cst),       ((Cst)#cstate.opt)#copt.file).
-define(START(Cst),       ((Cst)#cstate.opt)#copt.start).
-define(DOCTYPE(Cst),     ((Cst)#cstate.opt)#copt.doctype).
-define(ENCODING(Cst),    ((Cst)#cstate.opt)#copt.encoding).
-define(DTD(Cst),         ((Cst)#cstate.opt)#copt.dtd).
-define(DTD_TAB(Cst),     ((Cst)#cstate.opt)#copt.dtd_tab).
-define(DTD_STYLE(Cst),   ((Cst)#cstate.opt)#copt.dtd_style).

-define(SPACE(Cst),
	?GET_FLAG((Cst)#cstate.flags, ?SPACE_MASK)).
-define(DEFAULT_SPACE(Cst),
	(?GET_FLAG((Cst)#cstate.flags, ?SPACE_DEFAULT_MASK) bsr 2)).

-define(VALIDATE(Cst), 
	?IS_FLAG_SET((Cst)#cstate.flags, ?FLAG_VALIDATE)).
-define(IGNORE_CASE(Cst),
	?IS_FLAG_SET((Cst)#cstate.flags, ?FLAG_IGNORE_CASE)).
-define(IGNORE_NS(Cst),
	?IS_FLAG_SET((Cst)#cstate.flags, ?FLAG_IGNORE_NS)).
-define(USE_NS(Cst),
	?IS_FLAG_SET((Cst)#cstate.flags, ?FLAG_USE_NS)).
-define(USE_XML(Cst),
	?IS_FLAG_SET((Cst)#cstate.flags, ?FLAG_USE_XML)).
-define(USE_DTD(Cst),
	?IS_FLAG_SET((Cst)#cstate.flags, ?FLAG_USE_DTD)).
-define(USE_STRICT(Cst),
	?IS_FLAG_SET((Cst)#cstate.flags, ?FLAG_USE_STRICT)).

-define(USE_PI(Cst),
	?IS_FLAG_SET((Cst)#cstate.flags, ?FLAG_USE_PI)).
-define(USE_COMMENT(Cst),
	?IS_FLAG_SET((Cst)#cstate.flags, ?FLAG_USE_COMMENT)).

-define(IS_VALID(Cst),
	?IS_FLAG_SET((Cst)#cstate.flags, ?FLAG_IS_VALID)).
-define(IS_XML(Cst),
	?IS_FLAG_SET((Cst)#cstate.flags, ?FLAG_IS_XML)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%  DEFAULT XML/HTML  validating parser 
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%
%% Local callback
%%
init(Opts) ->
    ?dbg("INIT: ~p\n", [Opts]),
    foreach(fun(Path) ->
		    makeup_dtd_srv:add_path(Path)
	    end, getopt_list(dtd_path, Opts)),
    DTD      = getopt(dtd,  Opts, undefined),
    File     = getopt(file, Opts, "*stdin*"),
    Style    = getopt(style, Opts, plain),

    Finit = ?FLAG_IS_VALID,

    Fspace0
	= case getopt(space, Opts, default) of
	      default   -> ?SPACE_COLLAPSE;
	      preserve  -> ?SPACE_PRESERVE;
	      replace   -> ?SPACE_REPLACE;
	      normalize -> ?SPACE_NORMALIZE;
	      collapse  -> ?SPACE_COLLAPSE
	  end,
    %% set current and default space 
    Fspace = Fspace0 bor (Fspace0 bsl 2),

    Fvalidate = 
	case getopt(validate, Opts, false) of
	    true  -> ?FLAG_VALIDATE;
	    false -> 0
	end,
    Fstrict = 
	case getopt(strict, Opts, true) of
	    true -> ?FLAG_USE_STRICT;
	    false -> 0
	end,
    Ficase = 
	case getopt(icase, Opts, false) of
	    true -> ?FLAG_IGNORE_CASE;
	    false -> 0
	end,

    Fins = case getopt(ins, Opts, false) of
	       true -> ?FLAG_IGNORE_NS;
	       false -> 0
	   end,

    Fns = case getopt(ns, Opts, false) of
	      true -> ?FLAG_USE_NS;
	      false -> 0
	  end,
    Fdtd = if DTD =/= undefined -> 
		   ?FLAG_USE_DTD;
	      true -> 0
	   end,
    Fcomment = case getopt(comment,Opts,false) of
		   true -> ?FLAG_USE_COMMENT;
		   false -> 0
	       end,
    Fpi = case getopt(pi,Opts,false) of
	      true -> ?FLAG_USE_PI;
	      false -> 0
	  end,
    Flags = Finit bor Fspace bor Fvalidate bor Fstrict  
	bor Ficase bor Fins bor Fns bor Fdtd bor Fcomment bor Fpi,
    ?dbg("INIT-FLAGS=~w\n", [decode_flags(Flags)]),

    Start = case getopt(start, Opts, undefined) of
		undefined -> undefined;  %% FIXME: undefined may be used!!!
		STag when is_atom(STag) ->
		    makeup:name_ns(atom_to_list(STag), Fns=/=0);
		STag ->
		    STag
	    end,

    Cst = #cstate { opt = #copt { file = File,
				  start = Start,
				  dtd = DTD,
				  dtd_style = Style
				 },
		    flags = Flags,
		    ns_map = ?DEFAULT_NS_MAP,
		    action_stack = [],
		    stack = [#celem { tag   = '#TOP',
				      ns    = unqualified,
				      alist = [],
				      flags = Flags,
				      sta   = ?ERROR,
				      inc   = [],
				      exc   = [],
				      ns_map =  ?DEFAULT_NS_MAP
				     }]
		   },
    tag_action('#INIT',undefined,undefined,Cst).


%% Run options after detecting DOCTYPE/xml-encoding/html meta http-equiv content-type 
coptions(Opts, CSt) ->
    COpt = CSt#cstate.opt,

    foreach(fun(Path) ->
		    makeup_dtd_srv:add_path(Path)
	    end, getopt_list(dtd_path, Opts)),
    DTD      = getopt(dtd,  Opts, COpt#copt.dtd),
    File     = getopt(file, Opts, COpt#copt.file),
    Style    = getopt(style, Opts, COpt#copt.dtd_style),
    StyleChanged = (Style =/= COpt#copt.dtd_style),

    Start  = getopt(start, Opts, COpt#copt.start),
    Flags0 = CSt#cstate.flags,

    %% Save stuff detected sofar
    Finit = Flags0 band (?FLAG_IS_VALID bor ?FLAG_IS_XML),

    Fspace0
	= case getopt(space, Opts, undefined) of
	      undefined -> ?SPACE(CSt);
	      default   -> ?SPACE_COLLAPSE;
	      preserve  -> ?SPACE_PRESERVE;
	      replace   -> ?SPACE_REPLACE;
	      normalize -> ?SPACE_NORMALIZE;
	      collapse  -> ?SPACE_COLLAPSE
	  end,
    %% set current and default space 
    Fspace = Fspace0 bor (Fspace0 bsl 2),

    Fvalidate = 
	case getopt(validate, Opts, undefined) of
	    undefined -> Flags0 band ?FLAG_VALIDATE;
	    true ->  ?FLAG_VALIDATE;
	    false -> Flags0 band ?FLAG_VALIDATE
	end,
    Fstrict = 
	case getopt(strict, Opts, undefined) of
	    undefined -> Flags0 band ?FLAG_USE_STRICT;
	    true -> ?FLAG_USE_STRICT;
	    false -> 0
	end,
    Ficase = 
	case getopt(icase, Opts, undefined) of
	    undefined -> Flags0 band ?FLAG_IGNORE_CASE;
	    true -> ?FLAG_IGNORE_CASE;
	    false -> 0
	end,
    Fins = case getopt(ins, Opts, undefined) of
	       undefined -> Flags0 band ?FLAG_IGNORE_NS;
	       true -> ?FLAG_IGNORE_NS;
	       false -> 0
	   end,
    Fns = case getopt(ns, Opts, undefined) of
	      undefined -> Flags0 band ?FLAG_USE_NS;
	      true -> ?FLAG_USE_NS;
	      false -> 0
	  end,
    Fdtd = if DTD =/= undefined -> 
		   ?FLAG_USE_DTD;
	      true -> 0
	   end,
    Fcomment = case getopt(comment,Opts,undefined) of
		   undefined -> Flags0 band ?FLAG_USE_COMMENT;
		   true -> ?FLAG_USE_COMMENT;
		   false -> 0
	       end,
    Fpi = case getopt(pi,Opts,undefined) of
	      undefined -> Flags0 band ?FLAG_USE_PI;
	      true -> ?FLAG_USE_PI;
	      false -> 0
	  end,
    Flags = Finit bor Fspace bor Fvalidate bor Fstrict  
	bor Ficase bor Fins bor Fns bor Fdtd bor Fcomment bor Fpi,
    ?dbg("COPTION-FLAGS=~w\n", [decode_flags(Flags)]),
    %% Only reinitialze the Action stack if CHANGED!

    if StyleChanged == true ->
	    Stack0 = CSt#cstate.stack,
	    COpt1  = COpt#copt { file = File,
				 start = Start,
				 dtd = DTD,
				 dtd_style = Style
				},
	    CSt1 = CSt#cstate { opt = COpt1,
				flags = Flags,
				ns_map = ?DEFAULT_NS_MAP,
				stack = [#celem { tag   = '#TOP',
						  ns    = unqualified,
						  alist = [],
						  flags = Flags,
						  sta   = ?ERROR,
						  inc   = [],
						  exc   = [],
						  ns_map =  ?DEFAULT_NS_MAP
						 } | Stack0]
			       },
	    tag_action('#BEGIN',undefined,undefined,CSt1);
       true ->
	    CSt#cstate { opt = COpt#copt { file = File,
					   start = Start,
					   dtd = DTD
					  },
			 flags = Flags
			}
    end.

	    

final(_Buf,Line,CSt) ->
    ?dbg("FINAL:~w: ~s\n", [Line,_Buf]),
    %% CSt1 = pop_tag('#TOP', CSt),
    CSt1 = tag_end('#TOP', Line, CSt),
    case ?DTD_TAB(CSt1) of
	undefined -> 
	    ok;
	Tab -> 
	    ?dbg("DELETE SUBSECTION Table\n",[]),
	    ets:delete(Tab)
    end,
    if ?USE_STRICT(CSt1)==true, ?IS_VALID(CSt1) == false ->
	    {error, invalid};
       ?IS_VALID(CSt1) == false ->
	    Doc = #makeup_document { is_valid = false,
				     is_xml = ?IS_XML(CSt),
				     encoding = ?ENCODING(CSt),
				     doctype  = ?DOCTYPE(CSt),
				     content = CSt1#cstate.action_stack },
	    {ok, Doc};
       true ->
	    case CSt1#cstate.stack of
		[] ->
		    Doc = #makeup_document { is_valid = true,
					     is_xml = ?IS_XML(CSt),
					     encoding = ?ENCODING(CSt),
					     doctype  = ?DOCTYPE(CSt),
					     content = CSt1#cstate.action_stack },
		    
		    {ok, Doc};
		_TagI ->
		    ?dbg("Final top tag=~p\n", [_TagI]),
		    {error, invalid}
	    end
    end.

text(Text,Line,CSt) ->
    ?dbg("TEXT:~w: ~p\n", [Line,Text]),
    Text1 = case ?SPACE(CSt) of
		?SPACE_PRESERVE ->
		    Text;
		?SPACE_REPLACE ->
		    text_replace(Text);
		?SPACE_COLLAPSE ->
		    text_collapse(Text);
		?SPACE_NORMALIZE ->
		    text_collapse(Text)
	    end,
    if Text1 == [] ->
	    CSt;   %% Allways remove empty data ???
       true ->
	    if ?VALIDATE(CSt) == true ->
		    text_validate(Text1,Line,CSt);
	       true ->
		    text_push(CSt,Text1)
	    end
    end.

text_validate(Data,Line,CSt) ->
    case peek_tag(CSt) of
	'#TOP' ->
	    text_push(CSt,Data);
	TagI ->
	    [E = #celem{sta=Si} | Stack] = CSt#cstate.stack,
	    Tag = pcdata, %% '#PCDATA',
	    case state(TagI,Si,Tag,CSt) of
		Sj=?EXCLUDE ->
		    error_excluded_tag(Tag,Line,
				       CSt#cstate { stack=[E#celem{sta=Sj}|
							   Stack]});
		Sj={_,?EXCLUDE} ->
		    error_excluded_tag(Tag,Line,
				       CSt#cstate { stack=[E#celem{sta=Sj}|
							   Stack]});
		Sj ->
		    case state_accept(TagI,Sj,CSt) of
			reject ->
			    Stack1 = [E#celem{sta=Sj}|Stack],
			    CSt1 = CSt#cstate { stack = Stack1 },
			    CSt2 = error_rule(TagI,Line,CSt1),
			    text_push(CSt2,Data);
			_ -> %% accept | continue
			    CSt1=CSt#cstate { stack = [E#celem{sta=Sj}|Stack]},
			    text_push(CSt1,Data)
		    end
	    end
    end.

text_push(CSt,Data) ->
    Tag = case CSt#cstate.stack of
	      [] -> '#TOP';
	      [#celem { tag=Tag0 }|_] -> Tag0
	  end,
    CSt1 = tag_action('#PCDATA',Tag,Data,CSt),
    CSt1#cstate { alist = [] }.

%%
%% <Tag ...>
%%
%%   Close any open tags that have optional end tag
%%   when 'Tag' is not part of acceptable input.
%%
%%

tag_begin(Tag, Line, CSt) ->
    As = CSt#cstate.alist,
    #cstate { ns_map=NsMap, flags=Flags, lang=Lang } = CSt,
    CSt1 = update_state(As,Line,CSt),
    Ns   = tag_to_ns(Tag,CSt1),
    E = #celem { tag=Tag,ns=Ns,alist=As,flags=Flags,lang=Lang,ns_map=NsMap },
    ?dbg("<~p>:~w (xmlns=~s)\n", [Tag,Line,Ns]),
    if ?VALIDATE(CSt1) == true ->
	    Valid = validate_attributes(Tag, As, Line, CSt1)
		andalso ?IS_VALID(CSt1),
	    case peek_tag(CSt1) of
		'#TOP' ->
		    tag_top(Tag, As, Line, E, CSt1);
		Tag0 ->
		    CSt2 = tag_begin_end(Tag0, Tag, Line, CSt1),
		    CSt3 =
			if Valid == false ->
				Fs =?CLR_FLAG(CSt2#cstate.flags,?FLAG_IS_VALID),
				CSt2#cstate { flags = Fs};
			   true ->
				CSt2
			end,
		    push_tag(Tag,As,E,CSt3)
	    end;
       ?DTD(CSt1) =/= undefined; ?DTD_TAB(CSt1) =/= undefined ->
	    case peek_tag(CSt1) of
		'#TOP' ->
		    tag_top(Tag,As,Line,E,CSt1);
		Tag0 ->
		    CSt2 = tag_begin_end(Tag0, Tag, Line, CSt1),
		    push_tag(Tag,As,E,CSt2)
	    end;
       true ->
	    push_tag(Tag,As,E,CSt1)
    end.

%%
%% Push the top most tag
%%
tag_top(Tag, As, Line, E, CSt0) ->
    Tag0 = ?START(CSt0),
    if Tag == Tag0 ->
	    push_tag(Tag,As,E, CSt0);
       Tag0 =/= undefined ->
	    case start_tag_optional(Tag0, CSt0) of
		true ->
		    {S0,CSt1} = 
			case CSt0#cstate.stack of
			    [#celem{sta={_,Sk}}] -> 
				{Sk,CSt0};
			    [#celem{sta=?ERROR}] -> 
				Sk = state_init(Tag0,CSt0),
				{Sk, CSt0};
			    [#celem{sta=Sk}] -> 
				{Sk,CSt0}
			end,
		    [E1|Stack] = CSt1#cstate.stack,
		    case state(Tag0,S0,Tag,CSt1) of
			?ERROR ->
			    Stack1 = [E1#celem{sta={Tag0,?ERROR}}|Stack],
			    CSt2 = CSt1#cstate { stack = Stack1},
			    CSt3 = error_missing_init_tag(Tag0, Line, CSt2),
			    push_tag(Tag,As,E,CSt3);
			?EXCLUDE ->
			    Stack1 = [E1#celem{sta={Tag0,?EXCLUDE}}|Stack],
			    CSt2 = CSt1#cstate { stack = Stack1 },
			    CSt3 = error_excluded_tag(Tag,Line,CSt2),
			    push_tag(Tag,As,E,CSt3);
			Sj ->
			    Stack1 = [E1#celem{sta={Tag0,Sj}}|Stack],
			    CSt2 = CSt1#cstate { stack = Stack1 },
			    push_tag(Tag,As,E,CSt2)
		    end;
		false ->
		    CSt1 = error_missing_init_tag(Tag0, Line, CSt0),
		    push_tag(Tag,As,E,CSt1)
	    end;
       true ->
	    CSt1 = error_no_init_tag(Line, CSt0),
	    push_tag(Tag,As,E,CSt1)
    end.

%%
%% Pop tags while top tag has a optional end tag and
%% we do not have a partial match on the <TagI>  ... <Tag> 
%%
tag_begin_end(TagI, Tag, Line, CSt) ->
    ?dbg("BEGIN-END: tagI=~p  tag=~p\n", [TagI, Tag]),
    [E=#celem { sta=Si} | Stack] = CSt#cstate.stack,
    case state(TagI,Si,Tag,CSt) of
	Sj=?EXCLUDE ->
	    error_excluded_tag(Tag,Line,
			       CSt#cstate { stack=[E#celem{sta=Sj}|Stack]});
	Sj={_,?EXCLUDE} ->
	    error_excluded_tag(Tag,Line,
			       CSt#cstate { stack=[E#celem{sta=Sj}|Stack]});
	Sj -> 
	    case stop_tag_optional(TagI, CSt) of
		false ->
		    CSt#cstate { stack = [E#celem{sta=Sj}|Stack]};
		true ->
		    case empty_rule(TagI,CSt) of
			true ->
			    CSt1 = pop_tag(CSt),
			    tag_begin_end(peek_tag(CSt1),Tag,Line,CSt1);
			false ->
			    case state_accept(TagI,Sj,CSt) of
				reject ->
				    CSt1 = pop_tag(CSt),
				    tag_begin_end(peek_tag(CSt1),Tag,Line,CSt1);
				_ -> %% accept | continue
				    CSt#cstate { stack = [E#celem{sta=Sj}|Stack]}
			    end
		    end
	    end
    end.

%%
%% </Tag>
%%   Close tags with optional endtag until Tag is found
%%   or fail.
%%

tag_end(Tag, Line, CSt) ->
    ?dbg("</~p>:~w\n", [Tag,Line]),
    if ?VALIDATE(CSt) == true ->
	    tag_end_mod(Tag, Line, true, CSt);
       ?DTD(CSt) =/= undefined; ?DTD_TAB(CSt) =/= undefined ->
	    tag_end_mod(Tag, Line, false, CSt);
       true ->
	    pop_tag(Tag, CSt)
    end.

tag_end_mod(Tag, Line, Validate, CSt) ->
    Tag0 = peek_tag(CSt),
    CSt1 = pop_tag(CSt),
    ?dbg("tag_end_mod: tag=~p, tag0=~p, line=~p, validate=~p\n",
	 [Tag, Tag0, Line, Validate]),
    if Tag == Tag0 ->
	    if Validate == true ->
		    [#celem{sta=Si}|_] = CSt#cstate.stack,
		    case state_accept(Tag,Si,CSt) of
			accept ->
			    CSt1;
			continue ->
			    error_rule(Tag,Line,CSt1);
			reject -> %% rule is nonaccept
			    error_rule(Tag,Line,CSt1)
		    end;
	       true ->
		    CSt1
	    end;
       true ->
	    case stop_tag_optional(Tag0, CSt1) of
		true -> %% assumes validate is enabled.
		    ?dbg("OPTIONAL </~s>\n", [Tag0]),
		    [#celem{sta=Si}|_] = CSt1#cstate.stack,
		    case state_accept(Tag,Si,CSt1) of
			accept ->
			    tag_end_mod(Tag, Line, Validate, CSt1);
			continue ->
			    CSt2 = error_rule(Tag,Line,CSt1),
			    tag_end_mod(Tag, Line, Validate, CSt2);
			reject ->
			    CSt2 = error_rule(Tag,Line,CSt1),
			    tag_end_mod(Tag, Line, Validate, CSt2)
		    end;
		false ->
		    %% check for Tag in tag stack otherwise drop -
		    %% we may want to restrict depth?
		    case lists:keysearch(Tag, #celem.tag, CSt#cstate.stack) of
			{value,_} ->
			    CSt2 = error_missing_end_tag(Tag0, Line, CSt),
			    CSt2;
			false ->
			    CSt2 = error_missing_begin_tag(Tag, Line, CSt),
			    pop_tag(Tag, CSt2)
		    end
	    end
    end.
		    

%%
%% <?<Target> [Value] ?>
%%
%%
processing(xml, Value, _Line, CSt) ->
    ?dbg("PROCESSING::~w xml ~s\n", [_Line,Value]),
    Flags = ?SET_FLAG(CSt#cstate.flags, ?FLAG_IS_XML),
    ?dbg(" XML-FLAGS=~w\n", [decode_flags(Flags)]),
    KeyVal = makeup:keyval(Value),
    case lists:keysearch(encoding, 1, KeyVal) of
	false ->
	    CSt1 = CSt#cstate { flags=Flags, alist = [] },
	    pi_action(xml,Value,CSt1);
	{value,{_,Charset}} ->
	    ?dbg("Charset: ~p\n", [Charset]),
	    CSt1 = makeup:ioptions([{encoding,Charset}], CSt),
	    COpt = CSt1#cstate.opt,
	    COpt1 = COpt#copt { encoding = Charset },
	    CSt2 = CSt1#cstate { opt=COpt1, flags=Flags, alist = [] },
	    pi_action(xml,Value,CSt2)	    
    end;
processing(Target, Value, _Line, CSt) ->
    ?dbg("PROCESSING::~w ~s\n", [_Line,Target]),
    pi_action(Target,Value,CSt).

pi_action(Tag,Value,CSt) ->
    if ?USE_PI(CSt) ->
	    tag_action('#PI',Tag,Value,CSt);
       true ->
	    CSt
    end.

%%
%%  <Tag ... Attr=Value...>
%%

attribute(_Tag, Attr, _Line, CSt) ->
    ?dbg("ATTRIBUTE:~w ~p ~p\n", [_Line,_Tag,Attr]),
    As = [Attr|CSt#cstate.alist],
    CSt#cstate { alist = As }.

attribute(_Tag, Attr, Value, _Line, CSt) ->
    ?dbg("ATTRIBUTE:~w ~p ~p=~p\n", [_Line,_Tag,Attr,Value]),
    As = [{Attr,Value}|CSt#cstate.alist],
    CSt#cstate { alist = As }.

declaration('DOCTYPE', _Line, CSt) ->
    As = foldl(fun({_,A},Acc) -> [A|Acc] end, [], CSt#cstate.alist),
    LnAs = reverse(CSt#cstate.alist),
    ?dbg("DOCTYPE:~w ~p\n", [_Line,As]),
    case LnAs of
	[{_,Tag},{_,"PUBLIC"},{_,PUBID}] ->
	    case makeup_dtd_srv:public_entry(unquote(PUBID)) of
		{ok,_Path,{_PUBID,_Url,Opts,DTD,_File}} ->
		    declaration_options(Tag,As,Opts,DTD,CSt);
		{error,Err} ->
		    io:format("PUBLIC id ~s : ~p\n", [PUBID, Err]),
		    declaration_options(Tag,As,[],undefined,CSt)
	    end;
	[{_,Tag},{_,"PUBLIC"},{_,PUBID},{_,_Url}] ->
	    %% FIXME: download of URL if needed
	    case makeup_dtd_srv:public_entry(unquote(PUBID)) of
		{ok,_Path,{_PUBID,_Url1,Opts,DTD,_File}} ->
		    declaration_options(Tag,As,Opts,DTD,CSt);
		{error,Err} ->
		    io:format("PUBLIC id ~s : ~p\n", [PUBID, Err]),
		    declaration_options(Tag,As,[],undefined,CSt)
	    end;
	[{Ln1,Tag},{_,"PUBLIC"},{_,PUBID},{_,_Url},{_,Sub}] ->
	    %% FIXME Download of URL if needed
	    CSt1 = add_subsection(Sub,Ln1,CSt),
	    case makeup_dtd_srv:public_entry(unquote(PUBID)) of
		{ok,_Path,{_PUBID,_Url1,Opts,DTD,_File}} ->
		    declaration_options(Tag,As,Opts,DTD,CSt1);
		{error,Err} ->
		    io:format("PUBLIC id ~s : ~p\n", [PUBID, Err]),
		    declaration_options(Tag,As,[],undefined,CSt1)
	    end;
	[{_,Tag},{_,"SYSTEM"},{_,Url}] ->
	    %% Lookup / Load and set dtd
	    case makeup_dtd_srv:system_entry(unquote(Url)) of
		{ok,_Path,{_Url,Opts,DTD,_File}} ->
		    declaration_options(Tag,As,Opts,DTD,CSt);
		{error,Err} ->
		    io:format("SYSTEM url ~s : ~p\n", [Url, Err]),
		    declaration_options(Tag,As,[],undefined,CSt)
	    end;
	[{Ln1,Tag},{_,"SYSTEM"},{_,Url},{_,Sub}] ->
	    CSt1 = add_subsection(Sub,Ln1,CSt),
	    %% Lookup / Load and set dtd
	    case makeup_dtd_srv:system_entry(unquote(Url)) of
		{ok,_Path,{_Url,Opts,DTD,_File}} ->
		    declaration_options(Tag,As,Opts,DTD,CSt1);
		{error,Err} ->
		    io:format("SYSTEM url ~s : ~p\n", [Url, Err]),
		    declaration_options(Tag,As,[],undefined,CSt1)
	    end;
	[{Ln1,Tag}, {_,Sub}] ->
	    CSt1 = add_subsection(Sub,Ln1,CSt),
	    declaration_options(Tag,As,[],undefined,CSt1);
	_ ->
	    COpt = CSt#cstate.opt,
	    COpt1 = COpt#copt { doctype = As },
	    CSt#cstate { opt = COpt1, alist = [] }
    end;
declaration(_Tag, _Line, CSt) ->
    ?dbg("DECLARATION:~w ~s ~p\n", [_Line,_Tag,reverse(CSt#cstate.alist)]),
    CSt#cstate { alist = [] }.

%% Process declaration options
declaration_options(Tag,As,[],undefined,CSt) ->
    Tag1 = list_to_tag_name(cvtcase(?IGNORE_CASE(CSt),Tag),CSt),
    COpt = CSt#cstate.opt,
    CSt#cstate { opt = COpt#copt { doctype=As, start=Tag1},alist = []};
declaration_options(Tag,As,Opts,undefined,CSt) ->
    CSt0 = makeup:ioptions(Opts, CSt),
    _CSt1 = coptions(Opts, CSt0),  %% FIXME?
    Tag1 = list_to_tag_name(cvtcase(?IGNORE_CASE(CSt),Tag),CSt),
    COpt = CSt#cstate.opt,
    CSt#cstate { opt = COpt#copt { doctype=As, start=Tag1},alist = []};
declaration_options(Tag,As,Opts,DTD,CSt) ->
    CSt0 = makeup:ioptions(Opts, CSt),
    CSt1 = coptions([{dtd,DTD}|Opts], CSt0),
    Tag1 = list_to_tag_name(cvtcase(?IGNORE_CASE(CSt1),Tag),CSt1),
    COpt = CSt1#cstate.opt,
    CSt1#cstate { opt = COpt#copt { doctype=As,start=Tag1 }, alist = []}.


%% declaration parameters
dparam(_Tag, Value, Line, St) ->
    ?dbg("DPARAM:~w ~s ~s\n", [Line,_Tag,Value]),
    As = [{Line,Value}|St#cstate.alist],
    St#cstate { alist = As }.

%% declaration comment
dcomment(_Tag,_Comment,_Line,CSt) ->
    ?dbg("COMMENT:~w ~s ~s\n", [_Line,_Tag,_Comment]),
    CSt.

section(_Tag,_Data, _Line, CSt) ->
    ?dbg("SECTION:~w: ~s ~s\n", [_Line,_Tag,_Data]),
    CSt.

comment(Comment, _Line,CSt) ->
    ?dbg("COMMENT:~w ~s\n", [_Line,Comment]),
    if ?USE_COMMENT(CSt) ->
	    tag_action('#!',Comment,"", CSt);
       true ->
	    CSt
    end.

charref(Ref, _Line, St) ->
    ?dbg("CHARREF:~w ~s\n", [_Line,Ref]),
    case lookup_charref(Ref, St) of
	undefined ->
	    case Ref of
		"lt" -> [?LT];
		"gt" -> [?GT];
		"amp"  -> [?AMP];
		"apos"  -> [?APOS];
		"quot" -> [?QUOT];
		_ -> undefined
	    end;
	Chars -> Chars
    end.

%%
%% Default actions is to build a tag tree structure
%%  actions:
%%     '#PCDATA' called with PCDATA
%%     '#BEGIN'  called when <tag ...>  seen
%%     '#END'    called when </tag>     seen
%%     '#PI'     <?target value?>
%%     '#!'      <!-- comment -->
%%
tag_action(What,Tag,Data,CSt) ->
    case ?DTD_STYLE(CSt) of
	verify -> 
	    CSt;
	plain ->
	    Stack = CSt#cstate.action_stack,
	    Stack1 = plain_action(What,Tag,Data,Stack),
	    CSt#cstate { action_stack = Stack1};
	record ->
	    Stack = CSt#cstate.action_stack,
	    Stack1 = record_action(What,Tag,Data,Stack,CSt),
	    CSt#cstate { action_stack = Stack1}
    end.

plain_action('#PCDATA', _ContextTag, Data, [Bdy|Bs0]) ->
    [[{'#PCDATA',Data}|Bdy] | Bs0];
plain_action('#BEGIN', _Tag, _As, ElemStack) ->
    [ [] | ElemStack];
plain_action('#END', '#TOP', [], ElemStack) ->
    case ElemStack of
	[Bdy] ->
	    reverse(Bdy);
	[Bdy,PBdy|BdyS1] -> 
	    [[reverse(Bdy)|PBdy]|BdyS1]
    end;
plain_action('#END', Tag, Att, ElemStack) ->
    case ElemStack of
	[Bdy] ->
	    [[plain_action_rule(Tag,Att,Bdy)]];
	[Bdy,PBdy|BdyS1] ->
	    [[plain_action_rule(Tag,Att,Bdy)|PBdy]|BdyS1]
    end;
plain_action('#PI', Target, Value, [Bdy|Bs0]) ->
    [[{'?',Target,Value}|Bdy] | Bs0];
plain_action('#!', Comment, _Value, [Bdy|Bs0]) ->
    [[{'!--',Comment}|Bdy] | Bs0];
plain_action('#INIT', _Tag, _Att, _ElemStack) ->
    [[]].

plain_action_rule(Tag,Att,Bdy) ->
    {Tag,reverse(Att),reverse(Bdy)}.


record_action('#PCDATA', _ContextTag, Data, [Bdy|Bs0], _CSt) ->
    [[{'#PCDATA',Data}|Bdy] | Bs0];
record_action('#BEGIN', _Tag, _As, ElemStack, _CSt) ->
    [ [] | ElemStack];
record_action('#END', '#TOP', [], ElemStack, _CSt) ->
    case ElemStack of
	[Bdy] ->
	    reverse(Bdy);
	[Bdy,PBdy|BdyS1] -> 
	    [[reverse(Bdy)|PBdy]|BdyS1]
    end;
record_action('#END', Tag, Att, ElemStack, CSt) ->
    case ElemStack of
	[Bdy] ->
	    [[record_action_rule(Tag,Att,Bdy,CSt)]];
	[Bdy,PBdy|BdyS1] ->
	    [[record_action_rule(Tag,Att,Bdy,CSt)|PBdy]|BdyS1]
    end;
record_action('#PI', _Target, _Value, ElemStack, _Cst) ->
    ElemStack;
record_action('#!', _Comment, _Value, ElemStack, _CSt) ->
    ElemStack;
record_action('#INIT', _Tag, _Att, _ElemStack, _CSt) ->
    [[]].

record_action_rule(Tag,Att,Bdy,CSt) ->
    case lookup_rule(Tag,CSt) of
	undefined ->
	    {Tag,reverse(Att),reverse(Bdy)};
	Rule ->
	    Es = reverse(Bdy),
	    ?dbg("Elements = ~p\n", [Es]),
	    ?dbg("Record rule = ~p\n", [Rule]),
	    case record_rule(Rule,Es) of
		fail ->
		    {Tag,reverse(Att),reverse(Bdy)};
		{[],Res} ->
		    case Rule of
			{sequence,_} ->
			    list_to_tuple(
			      [Tag,reverse(Att) |
			       foldl(fun({_,E},Acc) -> 
					     [E|Acc];
					(E,Acc) -> 
					     [E|Acc]
				     end,[],Res)]);
			_ ->
			    {Tag,Res}
		    end
	    end
    end.

%% Run a element rule on action stack
record_rule(R, Es) ->
    case R of
	pcdata when element(1,hd(Es)) == '#PCDATA' ->
	    %% Plain textual data
	    {tl(Es), element(2,hd(Es))};
	pcdata ->
	    %% Well this text is xml ....
	    {[], Es};
	any ->
	    %% Well this can be anything
	    {[], Es};
	{element,Name} when element(1,hd(Es)) == Name ->
	    {tl(Es), hd(Es)};
	empty when Es==[] -> 
	    {[], defined};
	{sequence,Rs1} ->
	    record_sequence(Rs1,Es,[]);
	{pclosure, R1} ->
	    record_pclosure(R1,Es,[]);
	{closure, R1} ->
	    record_closure(R1,Es,[]);
	{choice,Rs1} ->
	    record_choice(Rs1,Es);
	{optional,R1} ->
	    case record_rule(R1,Es) of
		fail ->
		    {Es,undefined};
		Result ->
		    Result
	    end;
	_ ->
	    fail
    end.
%%
%% Run R+
%%
record_pclosure(R, Es, _As) ->
    case record_rule(R,Es) of
	fail -> fail;
	{Es1,A} ->
	    record_closure(R,Es1,[A])
    end.

%%
%% Run R*
%%
record_closure(R,Es,As) ->
    case record_rule(R,Es) of
	fail -> {Es,reverse(As)};
	{Es1,A} ->
	    record_closure(R,Es1,[A|As])
    end.

%%
%% R1,R2,..Rn
%%
record_sequence([R|Rs],Es,As) ->
    case record_rule(R, Es) of
	fail -> fail;
	{Es1,A} ->
	    record_sequence(Rs, Es1,[A|As])
    end;
record_sequence([], Es, As) ->
    {Es, As}.

%%
%% R1|R2|..|Rn
%%
record_choice([R|Rs], Es) ->
    case record_rule(R, Es) of
	fail ->
	    record_choice(Rs, Es);
	Result ->
	    Result
    end;
record_choice([], _Es) ->
    fail.



add_subsection(Sub0,Ln1,CSt) ->
    case ?VALIDATE(CSt) of
	true ->
	    Opts = [{icase,?IGNORE_CASE(CSt)},
		    {ins,  ?IGNORE_NS(CSt)},
		    {ns,   ?USE_NS(CSt)},
		    {line,Ln1}
		   ],
	    Sub  = unblock(Sub0),
	    case ?DTD_TAB(CSt) of
		undefined -> 
		    %% FIXME: must cleanup if we crash 
		    Tab = ets:new(dtd, []),
		    %% io:format("Sub=~p\n", [Sub]),
		    case catch makeup_dtd:string(Sub,Opts,Tab) of
			{ok,_} ->
			    ?dbg("ADDED (New) SUBSECTION: ~999p\n",
				 [ets:tab2list(Tab)]),
			    COpt = CSt#cstate.opt,
			    Flags = ?SET_FLAG(CSt#cstate.flags, ?FLAG_USE_DTD),
			    CSt#cstate { flags=Flags,
					 opt = COpt#copt {dtd_tab = Tab }};
			{error,ErrorList} ->
			    ets:delete(Tab),
			    error_list(ErrorList,CSt)
		    end;
		Tab ->
		    case catch makeup_dtd:string(Sub,Opts,Tab) of
			{ok,_} ->
			    ?dbg("ADDED SUBSECTION: ~999p\n",
				 [ets:tab2list(Tab)]),
			    CSt;
			{error,ErrorList} ->
			    error_list(ErrorList,CSt)
		    end
	    end;
	false ->
	    CSt
    end.
    
exclude(Tag, Stack) ->
    exception(Tag, #celem.exc, Stack).

include(Tag, Stack) ->
    exception(Tag, #celem.inc, Stack).

exception(Tag, Pos, [E|Es]) ->
    case element(Pos, E) of
	[] -> exception(Tag, Pos, Es);
	Ls ->
	    case member({element,Tag}, Ls) of
		true -> true;
		false -> exception(Tag,Pos,Es)
	    end
    end;
exception(_Tag, _Pos, []) ->
    false.


%% Check the top tag
peek_tag(St) ->
    case St#cstate.stack of
	[] when ?USE_STRICT(St) == false; ?IS_VALID(St) == false ->
	    '#TOP';
	[#celem {tag=Tag}|_] ->
	    Tag
    end.

%%
%% Push a tag/attributes and children
%%
push_tag(Tag, Att, E, St0) ->
    S0 = state_init(Tag,St0),
    push_tag(Tag, Att, S0, include_list(Tag,St0), exclude_list(Tag,St0),E,St0).

push_tag(Tag, Att, S0, Inc, Exc, E, CSt) ->
    ?dbg("PUSH: ~p (~w)\n",[Tag,S0]),
    CSt1 = tag_action('#BEGIN',Tag,Att,CSt),
    CSt1#cstate { stack = [E#celem { sta=S0,inc=Inc,exc=Exc } | 
			   CSt#cstate.stack ],
		  alist = [] }.


%% Pop Named Tag
pop_tag(Tag, CSt) when element(#celem.tag,hd(CSt#cstate.stack)) == Tag ->
    pop_tag(CSt);
pop_tag(Tag, CSt) ->
    CSt1 = error_invalid(CSt),
    %% to recover - locate Tag in tag stack and pop until match.
    case lists:keysearch(Tag, #celem.tag, CSt1#cstate.stack) of
	{value,_} ->
	    pop_tag(Tag, pop_tag(CSt1));
	false ->
	    CSt1
    end.


%% Pop Top Tag
pop_tag(CSt) ->
    #cstate { flags = Flags0, stack = [Elem | Stack] } = CSt,
    ?dbg("POP-TAG ~p (~w)\n", [Elem#celem.tag,Elem#celem.sta]),
    CSt1 = tag_action('#END',Elem#celem.tag,Elem#celem.alist,CSt),
    #celem { ns_map=NsMap, flags=Flags1, lang=Lang} = Elem,
    %% keep sticky flags
    Fsticky = ?GET_FLAG(Flags0, ?FLAG_STICKY),
    Flags = ?SET_FLAG_MASK(Flags1, ?FLAG_STICKY, Fsticky),
    ?dbg(" END ~w: flags=~w\n", [Elem#celem.tag,decode_flags(Flags)]),
    CSt1#cstate { ns_map = NsMap, lang = Lang, flags = Flags, stack = Stack }.

%% remove quotes
unquote([?QUOT|String]) ->
    case reverse(String) of
	[?QUOT|RString] -> reverse(RString);
	_ -> String
    end;
unquote([?APOS|String]) ->
    case reverse(String) of
	[?APOS|RString] -> reverse(RString);
	_ -> String
    end;
unquote(String) ->
    String.

unblock([?LBRACKET|String]) ->
    case reverse(String) of
	[?RBRACKET|RString] -> reverse(RString);
	_ -> String
    end;
unblock(String) ->
    String.

text_replace(Text) ->
    text_replace(Text,[]).
    
%% replace and reverse
text_replace(Cs,Acc) ->
    case Cs of
	[?SP|Cs1] ->
	    text_replace(Cs1, [?SP|Acc]);
	[?TAB|Cs1] ->
	    text_replace(Cs1, [?SP|Acc]);
	[?CR|Cs1] ->
	    text_replace(Cs1, [?SP|Acc]);
	[?NL|Cs1] ->
	    text_replace(Cs1, [?SP|Acc]);
	[C|Cs1] ->
	    text_replace(Cs1, [C|Acc]);
	[] ->
	    reverse(Acc)
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

cvtcase(true, String) ->
    tolower(String);
cvtcase(_, String) ->
    String.

tolower([C|Cs]) ->
    if (C) >= $A, (C) =< $Z -> 
	    [((C)-$A)+$a | tolower(Cs)];
        true -> 
	    [(C) | tolower(Cs)]
    end;
tolower([]) ->
    [].

error_rule(Tag, Line, CSt) ->
    RStr = makeup_re:format(lookup_rule(Tag, CSt)),
    io:format("~s:~w: ~s rule ~s failed\n", 
	      [?IFILE(CSt),Line,makeup:format_tag_name(Tag),RStr]),
    error_invalid(CSt).

error_excluded_tag(Tag, Line, CSt) ->
    io:format("~s:~w: ~s excluded by exclusion rules\n",
	      [?IFILE(CSt), Line, makeup:format_tag_name(Tag)]),
    error_invalid(CSt).

error_missing_end_tag(Tag, Line, CSt) ->
    io:format("~s:~w: Missing end tag ~s\n",
	      [?IFILE(CSt), Line, makeup:format_tag_name(Tag)]),
    error_invalid(CSt).

error_missing_begin_tag(Tag, Line, CSt) ->
    io:format("~s:~w: Missing tag ~s\n",
	      [?IFILE(CSt), Line, makeup:format_tag_name(Tag)]),
    error_invalid(CSt).

error_missing_init_tag(Tag, Line, CSt) ->
    io:format("~s:~w: Missing starting tag ~s\n",
	      [?IFILE(CSt), Line, makeup:format_tag_name(Tag)]),
    error_invalid(CSt).

error_no_init_tag(Line, CSt) ->
    io:format("~s:~w: Initial tag not defined\n",
	      [?IFILE(CSt), Line]),
    error_invalid(CSt).

error_multiple_ns_map(Line, Nm1, Nm2, Ns, CSt) ->
    io:format("~s:~w: Multiple names ~s,~s for namespace ~s\n",
	      [?IFILE(CSt), Line, Nm1, Nm2, Ns]),
    error_invalid(CSt).

error_list([{Ln,Descr} | Err], CSt) ->
    case Descr of
	{element_not_defined,E} ->
	    io:format("~s:~w: ELEMENT ~p not defined\n",
		      [?IFILE(CSt), Ln, E]);
	{syntax_error,Where} ->
	    io:format("~s:~w: Syntax error (~s)\n",
		      [?IFILE(CSt), Ln, Where]);
	{entity_not_found,E} ->
	    io:format("~s:~w: ENTITY ~p not defined\n",
		      [?IFILE(CSt), Ln, E]);
	{entity_not_inline,E} ->
	    io:format("~s:~w: ENTITY ~p not inline\n",
	      [?IFILE(CSt), Ln, E]);
	{system_url_not_found,ID,Reason} ->
	    io:format("~s:~w: SYSTEM url ~p not found (~p)\n",
	      [?IFILE(CSt), Ln, ID, Reason]);
	{public_id_not_found,ID,Reason} ->
	    io:format("~s:~w: PUBLIC id ~p not found (~p)\n",
	      [?IFILE(CSt), Ln, ID, Reason]);
	{bad_entity_type,Name,Type} ->
	    io:format("~s:~w: Bad ENTITY type ~p (~p)\n",
	      [?IFILE(CSt), Ln, Name,Type]);
	_ ->
	    io:format("~s:~w: ERROR ~p\n", 
		      [?IFILE(CSt), Ln, Descr])
    end,
    error_list(Err, CSt);
error_list([], CSt) ->
    error_invalid(CSt).

%% mark state as invalid
error_invalid(CSt) ->
    Flags = ?CLR_FLAG(CSt#cstate.flags, ?FLAG_IS_VALID),
    CSt#cstate { flags = Flags }.

%% debug flags
-ifdef(debug).
decode_flags(Flags) ->
    case ?IS_FLAG_SET(Flags,?FLAG_USE_XML) of
	true -> [{xml,true}];
	false -> []
    end ++
    case ?IS_FLAG_SET(Flags,?FLAG_USE_NS) of
	true -> [{ns,true}];
	false -> []
    end ++
    case ?IS_FLAG_SET(Flags,?FLAG_USE_STRICT) of
	true -> [{strict,true}];
	false -> []
    end ++
    case ?IS_FLAG_SET(Flags,?FLAG_VALIDATE) of
	true -> [{validate,true}];
	false -> []
    end ++
    case ?IS_FLAG_SET(Flags,?FLAG_IGNORE_CASE) of
	true -> [{icase,true}];
	false -> []
    end ++
    case ?IS_FLAG_SET(Flags,?FLAG_IGNORE_NS) of
	true -> [{ins,true}];
	false -> []
    end ++
    case ?IS_FLAG_SET(Flags,?FLAG_IS_VALID) of
	true -> [{is_valid,true}];
	false -> [{is_valid,false}]
    end ++
    case ?IS_FLAG_SET(Flags,?FLAG_IS_XML) of
	true -> [{is_xml,true}];
	false -> [{is_xml,false}]
    end ++
    case ?IS_FLAG_SET(Flags,?FLAG_USE_DTD) of
	true -> [{use_dtd,true}];
	false -> [{use_dtd,false}]
    end ++
    case ?IS_FLAG_SET(Flags,?FLAG_USE_COMMENT) of
	true -> [{comment,true}];
	false -> [{comment,false}]
    end ++
    case ?IS_FLAG_SET(Flags,?FLAG_USE_PI) of
	true -> [{pi,true}];
	false -> [{pi,false}]
    end ++
    case ?GET_FLAG(Flags,?SPACE_MASK) of
	?SPACE_PRESERVE  -> [{space,preserve}];
	?SPACE_REPLACE   -> [{space,replace}];
	?SPACE_COLLAPSE  -> [{space,collapse}];
	?SPACE_NORMALIZE -> [{space,normalize}]
    end ++	
    case ?GET_FLAG(Flags,?SPACE_DEFAULT_MASK) bsr 2 of
	?SPACE_PRESERVE  -> [{space,preserve}];
	?SPACE_REPLACE   -> [{space,replace}];
	?SPACE_COLLAPSE  -> [{space,collapse}];
	?SPACE_NORMALIZE -> [{space,normalize}]
    end.
-endif.

    
%% process language/space and name space updates
update_state(As,Line,CSt) ->
    if ?USE_XML(CSt)==true; ?IS_XML(CSt) == true; ?USE_NS(CSt) == true ->
	    upd_state(As,Line,CSt);
       true ->
	    CSt
    end.

upd_state([{Tag,Value}|As],Line,CSt) when Tag==[xml|space]; Tag=='xml:space' ->
    Space = case Value of
		"default"  ->
		    ?DEFAULT_SPACE(CSt);
		"preserve" ->
		    ?SPACE_PRESERVE;
		_ ->
		    ?SPACE(CSt)
	    end,
    Flags = ?SET_FLAG_MASK(CSt#cstate.flags,?SPACE_MASK,Space),
    upd_state(As,Line,CSt#cstate { flags = Flags });
upd_state([{Tag,Value}|As],Line,CSt) when Tag==[xml|lang]; Tag=='xml:lang' ->
    upd_state(As,Line,CSt#cstate { lang = Value });
upd_state([{xmlns,Value}|As],Line,CSt) ->
    NsVal = list_to_atom(Value),
    NsMap=replace_ns(?DEFAULT_PREFIX, NsVal,CSt#cstate.ns_map),
    upd_state(As,Line,CSt#cstate { ns_map = NsMap });
upd_state([{[xmlns|Ns],Value}|As],Line,CSt) ->
    NsVal = list_to_atom(Value),
    NsMap0 = CSt#cstate.ns_map,
    case lists:keysearch(Ns, 1, NsMap0) of
	false ->
	    NsMap = [{Ns,NsVal}|NsMap0],
	    CSt1 = CSt#cstate { ns_map = NsMap},
	    case lists:keysearch(NsVal,2,NsMap0) of
		false -> 
		    upd_state(As,Line,CSt1);
		{value,{'$default',_}} -> %% hmmm ok?
		    upd_state(As,Line,CSt1);
		{value,{Ns2,_}} ->
		    %% warning ?
		    CSt2 = error_multiple_ns_map(Line,Ns,Ns2,NsVal,CSt1),
		    upd_state(As,Line,CSt2)
	    end;
	{value,{_,NsVal}} ->
	    upd_state(As,Line,CSt);
	{value,_} ->
	    NsMap = lists:keyreplace(Ns,1,NsMap0,{Ns,NsVal}),
	    upd_state(As,Line,CSt#cstate { ns_map = NsMap })
    end;
upd_state([_ | As],Line,CSt) ->
    upd_state(As,Line,CSt);
upd_state([],_Line,CSt) ->
    CSt.


%% First check the attributes listed in the tag
validate_attributes(Tag, As, Line, CSt) ->
    case lookup_attribute_list(Tag, CSt) of
	undefined when As == [] -> true;  %% FIXME: is this ok?
	undefined ->
	    foreach(
	      fun({A,_}) ->
		      io:format("~s:~w: attribute: ~s unknown for tag ~s\n",
				[?IFILE(CSt), Line, 
				 makeup:format_attr_name(A), 
				 makeup:format_tag_name(Tag)])
	      end, As),
	    true; %% false;  FIXME: add warning flag
	TAs ->
	    Flag1 =
		foldl(
		  fun({A,V},Flag) ->
			  validate_value(Tag,A,V,Line,CSt) andalso Flag;
		     (A,Flag) ->
			  validate_value(Tag,A,atom_to_list(A),Line,CSt) 
			      andalso Flag
		  end, true, As),
	    RAs = TAs -- map(fun({A,_}) -> A end, As),
	    Flag2 = 
		foldl(
		  fun(A, Flag) ->
			  case lookup_attribute_def(Tag,A,CSt) of
			      {_Type,required,_} ->
				  io:format("~s:~w: attribute: ~s required for tag ~s\n",
					    [?IFILE(CSt),Line,
					     makeup:format_attr_name(A),
					     makeup:format_tag_name(Tag)]),
				  false;
			      {_Type, _, _} ->
				  Flag
			  end
		  end, Flag1, RAs),
	    Flag2
    end.


validate_value(Tag,A,V,Line,CSt) ->
    case lookup_attribute_def(Tag,A,CSt) of
	undefined ->
	    case lookup_attribute_def(Tag, xmlns, CSt) of
		undefined ->
		    io:format("~s:~w: attribute: ~s unknown for tag ~s\n",
			      [?IFILE(CSt),Line,
			       makeup:format_attr_name(A),
			       makeup:format_tag_name(Tag)]),
		    false;
		{Type,_Status,_Value} ->
		    case makeup:format_attr_name(A) of
			"xmlns:"++_ ->
			    case validate_attribute_value(Type, V) of
				false ->
				    io:format("~s:~w: attribute: ~s has bad value (~p, ~s)\n",
					      [?IFILE(CSt),Line,
					       makeup:format_attr_name(A),
					       Type,V]),
				    false;
				true -> true
			    end;
			_ ->
			    io:format("~s:~w: attribute: ~s unknown for tag ~s\n",
				      [?IFILE(CSt),Line,
				       makeup:format_attr_name(A),
				       makeup:format_tag_name(Tag)]),
			    false
		    end
	    end;

	{Type,_Status,_Value} ->
	    case validate_attribute_value(Type, V) of
		false ->
		    io:format("~s:~w: attribute: ~s has bad value (~p, ~s)\n",
			      [?IFILE(CSt),Line,
			       makeup:format_attr_name(A),Type,V]),
		    false;
		true -> true
	    end
    end.
%%
%% Lookup DTD/Schema rules:
%%    What ==  start_tag_optional   (tag -> boolean)
%%           | stop_tag_optional    (tag -> boolean)
%%           | include              (tag -> [tag])
%%           | exclude              (tag -> [tag])
%%           | state_init           (tag -> state)
%%           | state_accept         (tag,state -> boolean)
%%           | state                (tag,state,tag -> state
%%           | rule                 (tag -> reg-exp)
%%           | charref              (name -> char)
%%           | attributes           (tag -> [attribute-name])
%%           | attribute            (tag,attribute -> {Type,Required,Value})
%%
%%
%% dtd_lookup(Mod, Tab, What, Arg [Arg]) -> Data | undefined
%%
dtd_lookup(undefined,undefined,_What,_As) -> 
    undefined;
dtd_lookup(undefined,Tab,What,As) ->
    case ets:lookup(Tab, list_to_tuple([What|As])) of
	[{_,Result}] -> Result;
	[] -> undefined
    end;
dtd_lookup(Mod,undefined,What,As) ->
    apply(Mod, What, As);
dtd_lookup(Mod,Tab,What,As) ->
    case apply(Mod, What, As) of
	undefined ->
	    case ets:lookup(Tab, list_to_tuple([What|As])) of
		[{_,Result}] -> Result;
		[] -> undefined
	    end;
	Result -> Result
    end.


%% Check if start tag is optional
start_tag_optional(Tag, CSt) ->
    if ?USE_DTD(CSt) == true ->
	    case dtd_lookup(?DTD(CSt),?DTD_TAB(CSt),start_tag_optional,[Tag]) of
		undefined -> false;
		Bool -> Bool
	    end;
       true -> false
    end.

%% Check if stop tag is optional
stop_tag_optional(Tag, CSt) ->
    if ?USE_DTD(CSt) == true ->
	    case dtd_lookup(?DTD(CSt),?DTD_TAB(CSt), stop_tag_optional,[Tag]) of
		undefined -> false;
		Bool -> Bool
	    end;
       true -> false
    end.

empty_rule(Tag, CSt)  ->
    if ?USE_DTD(CSt) == true ->
	    case dtd_lookup(?DTD(CSt),?DTD_TAB(CSt),rule,[Tag]) of
		'EMPTY' -> true;
		_ -> false
	    end;
       true -> false
    end.

include_list(Tag, CSt) ->
    if ?USE_DTD(CSt) == true ->
	    case dtd_lookup(?DTD(CSt), ?DTD_TAB(CSt),include,[Tag]) of    
		undefined -> [];
		List -> List
	    end;
       true -> []
    end.

exclude_list(Tag, CSt) ->
    if ?USE_DTD(CSt) == true ->
	    case dtd_lookup(?DTD(CSt), ?DTD_TAB(CSt),exclude,[Tag]) of
		undefined -> [];
		List -> List
	    end;
       true -> []
    end.

%% lookup initial state number
state_init(Tag, CSt) ->
    if ?USE_DTD(CSt) == true ->
	    case dtd_lookup(?DTD(CSt), ?DTD_TAB(CSt), state_init,[Tag]) of
		undefined -> ?ERROR;
		St when is_integer(St) -> St
	    end;
       true -> ?ERROR
    end.

%% lookup the state type
state_accept('#TOP',_Sn,_CSt) ->
    accept;
state_accept(Tag,Sn,CSt) ->
    if ?USE_DTD(CSt) == true ->
	    case dtd_lookup(?DTD(CSt), ?DTD_TAB(CSt), state_accept,[Tag,Sn]) of
		undefined -> reject;
		true -> accept;
		false -> continue
	    end;
       true ->
	    reject
    end.

%% Find next state
state(_Tag,{RealTag,Si},Sym,St) ->
    {RealTag,state(RealTag,Si,Sym,St)};
state(Tag,Si,Sym,CSt) ->
    case exclude(Sym, CSt#cstate.stack) of
	true -> 
	    ?EXCLUDE;
	false ->
	    Sj = dtd_lookup(?DTD(CSt),?DTD_TAB(CSt),state,[Tag,Si,Sym]),
	    if Sj == undefined ->
		    Sk = dtd_lookup(?DTD(CSt),?DTD_TAB(CSt),state,
				    [Tag,Si,'_']),
		    if Sk == undefined ->
			    ?dbg("state(~p,~p,~p) = -1\n",[Tag,Si,Sym]),
			    case include(Sym, CSt#cstate.stack) of
				true ->
				    ?dbg("~s INCLUDED in ~s\n", [Sym,Tag]),
				    Si;
				false -> ?ERROR
			    end;
		       true ->
			    ?dbg("state(~p,~p,~p) = ~p\n",[Tag,Si,Sym,Sk]),
			    Sk
		    end;
	       true ->
		    ?dbg("state(~p,~p,~p) = ~p\n",[Tag,Si,Sym,Sj]),
		    Sj
	    end
    end.
    
%% Lookup a rule
lookup_rule(Tag, CSt) ->
    dtd_lookup(?DTD(CSt),?DTD_TAB(CSt),rule,[Tag]).

%% Lookup a rule
lookup_charref(Name, CSt) ->
    if ?USE_DTD(CSt) == true ->
	    dtd_lookup(?DTD(CSt),?DTD_TAB(CSt),charref,[Name]);
       true ->
	    undefined
    end.

%% Lookup attribute list
lookup_attribute_list(Tag, CSt) ->
    if ?USE_DTD(CSt) == true ->
	    dtd_lookup(?DTD(CSt),?DTD_TAB(CSt),attributes,[Tag]);
       true -> 
	    undefined
    end.

    
%% Lookup attribute definition
lookup_attribute_def(Tag,A,CSt) ->
    if ?USE_DTD(CSt) == true ->
	    dtd_lookup(?DTD(CSt), ?DTD_TAB(CSt),attribute,[Tag,A]);
       true ->
	    undefined
    end.


validate_attribute_value(Type, Cs) when is_list(Type) ->
    case tolower(Type) of
	"cdata"     -> true;
	"nmtoken"   -> valid_nmtoken(Cs);
	"nmtokens"  -> valid_nmtokens(Cs);
	"number"    -> valid_number(Cs);
	"name"      -> valid_name(Cs);
	"id"        -> true;
	"idref"     -> true;
	"any"       -> true;
	_ -> Type == Cs
    end;
validate_attribute_value({enum,Es}, Cs) ->
    member(Cs, Es);
validate_attribute_value(_, _) ->
    false.

list_to_tag_name(Name,CSt) ->
    if ?IGNORE_NS(CSt) == true ->
	    case makeup:name_ns(Name,true) of
		[_Ns|Nm] -> Nm;
		Nm -> Nm
	    end;
       true ->
	    makeup:name_ns(Name,?USE_NS(CSt))
    end.

getopt(Opt, [{Opt,Val}|_], _) ->
    Val;
getopt(Opt, [_|Opts], Def) ->
    getopt(Opt, Opts, Def);
getopt(_Opt, [], Def) ->
    Def.

getopt_list(Opt, [{Opt,Val}|Opts]) ->
    [Val | getopt_list(Opt, Opts)];
getopt_list(Opt, [_|Opts]) ->
    getopt_list(Opt, Opts);
getopt_list(_Opt, []) ->
    [].


%% Return the namespace for a given tag/attribute name
tag_to_ns(Tag, CSt) ->
    if ?USE_NS(CSt) == true ->
	    case Tag of
		[Prefix|_Tg] ->
		    find_ns(Prefix, CSt#cstate.ns_map);
		_ ->
		    find_ns(?DEFAULT_PREFIX, CSt#cstate.ns_map)
	    end;
       true ->
	    ''
    end.

find_ns(Prefix, NsMap) ->
    find_ns(Prefix, NsMap, undefined).

find_ns(Prefix, [{Prefix,NameSpace}|_],_Default) ->
    NameSpace;
find_ns(Prefix, [{?DEFAULT_PREFIX,NameSpace}|NsMap],_Default) ->
    find_ns(Prefix, NsMap, NameSpace);
find_ns(Prefix, [_|NsMap], Default) ->
    find_ns(Prefix, NsMap, Default);
find_ns(_Prefix, [], Default) ->
    Default.

replace_ns(Prefix,NameSpace,[{Prefix,_}|NsMap]) ->
    [{Prefix,NameSpace} | NsMap];
replace_ns(Prefix,NameSpace,[NsPair|NsMap]) ->
    [NsPair | replace_ns(Prefix,NameSpace,NsMap)];
replace_ns(Prefix,NameSpace,[]) ->
    [{Prefix,NameSpace}].


%% Check NUMBER = (0..9)+
valid_number([C|Cs]) when C >= $0, C =< $9 -> valid_number1(Cs);
valid_number(_) -> false.

valid_number1([C|Cs]) when C >= $0, C =< $9 -> valid_number1(Cs);
valid_number1([_|_]) -> false;
valid_number1([]) -> true.    

%% Check NMTOKENS  == (ntoken <sp>)+
valid_nmtokens(Cs) ->
    lists:all(fun(T) -> valid_nmtoken(T) end, 
	      string:tokens(Cs, " \t")).

%% Check NMTOKEN (0..9A..Za..z)+  (Added -_. and :)

valid_nmtoken([C|Cs]) when C >= $a, C =< $z -> valid_nmtoken1(Cs);
valid_nmtoken([C|Cs]) when C >= $A, C =< $Z -> valid_nmtoken1(Cs);
valid_nmtoken([C|Cs]) when C >= $0, C =< $9 -> valid_nmtoken1(Cs);
valid_nmtoken(_) -> false.

valid_nmtoken1([C|Cs]) when C >= $a, C =< $z -> valid_nmtoken1(Cs);
valid_nmtoken1([C|Cs]) when C >= $A, C =< $Z -> valid_nmtoken1(Cs);
valid_nmtoken1([C|Cs]) when C >= $0, C =< $9 -> valid_nmtoken1(Cs);
valid_nmtoken1([$_|Cs]) -> valid_nmtoken1(Cs);
valid_nmtoken1([$-|Cs]) -> valid_nmtoken1(Cs);
valid_nmtoken1([$.|Cs]) -> valid_nmtoken1(Cs);
valid_nmtoken1([$:|Cs]) -> valid_nmtoken1(Cs);
valid_nmtoken1([]) -> true;
valid_nmtoken1(_) -> false.

%% Check NAME (A..Za..z) (A..Za..z0..9)*
valid_name([C|Cs]) when C >= $a, C =< $z -> valid_nmtoken1(Cs);
valid_name([C|Cs]) when C >= $A, C =< $Z -> valid_nmtoken1(Cs);
valid_name(_) -> false.
