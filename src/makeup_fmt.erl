%%% File    : makeup_fmt.erl
%%% Author  : Tony Rogvall <tony@pbook.synap.se>
%%% Description : HTML/XML formatter
%%% Created : 11 May 2006 by Tony Rogvall <tony@pbook.synap.se>

-module(makeup_fmt).

-rcsid("$Id: makeup_fmt.erl,v 1.7 2007/09/04 07:31:10 per Exp $\n").

-vsn("$Revision: 1.7 $ ").

-import(lists, [reverse/1,map/2, foldl/3, foreach/2, member/2,append/1]).

-export([format/1, format/2]).
-export([format_begin/1, format_begin/2, 
	 format_open/2, format_close/2, 
	 format_continue/2, format_end/2]).
-export([format_attributes/1]).
-export([format_tag_name/1]).
-export([format_attr_name/1]).


-include("../include/makeup.hrl").

-define(dbgi(Fmt,As), io:format((Fmt),(As))).

-ifdef(debug).
-define(dbg(Fmt,As), io:format((Fmt),(As))).
-else.
-define(dbg(Fmt,As), ok).
-endif.

%%
%%  Formatting options
%%    compact      = true|false      (use newlines)
%%    indent       = <integer>       (pretty indent)
%%    encoding     = <charset-name>  (utf-8)
%%
%%
-record(makeup_format_option,
	{
	  compact = false,
	  indent  = 2,
	  level   = 0,        %% Indent level
	  content_type,       %% MIME type if available
	  xml_version,        %% For text/xml
	  encoding,           %% Output encoding
	  partial=false,      %% Partial mode, false,true,open,close
	  obf                 %% output functions
	 }).

%% Partial mode continue state
-record(makeup_format_st,
	{
	  level,   %% Current indent level
	  level0,  %% Starting indent level
	  obf,     %% Output function
	  tab,     %% Tab characters in current encoding
	  nl       %% Newline character in current encoding
	 }).

-define(OBF_NL(St), (St)#makeup_format_st.nl).
-define(OBF_TAB(St), (St)#makeup_format_st.tab).

-define(OBF_INDENT(Level,Tab),
	if (Tab)=="" -> "";
	   true -> lists:duplicate((Level),(Tab))
	end).

%% Parse output list options
format_options(Opts) when is_list(Opts) ->
    format_options(Opts, #makeup_format_option{});
format_options(R) when is_record(R, makeup_format_option) ->
    R.

%% parse list options
format_options([Opt|Opts], R) ->
    case Opt of
	{compact,V} when V==true; V==false ->
	    format_options(Opts, R#makeup_format_option { compact=V});
	{indent,V} when is_integer(V), V>=0 ->
	    format_options(Opts, R#makeup_format_option { indent=V});
	{level,V} when is_integer(V), V>=0 ->
	    format_options(Opts, R#makeup_format_option { level=V});
	{xml_version,V} when is_list(V) ->
	    format_options(Opts, R#makeup_format_option { xml_version=V});
	{content_type,V} when is_list(V) ->
	    format_options(Opts, R#makeup_format_option {content_type=V});
	{encoding,V} when is_list(V) ->
	    format_options(Opts, R#makeup_format_option { encoding=V});
	{partial,Mode} when Mode == false; Mode == true;
			    Mode == open; Mode==close ->
	    format_options(Opts, R#makeup_format_option { partial=Mode })
    end;
format_options([], R) ->
    R.

merge_format_options(R1,R2) ->
    R1#makeup_format_option { 
      xml_version = merge_val(R1#makeup_format_option.xml_version,
			      R2#makeup_format_option.xml_version),
      content_type = merge_val(R1#makeup_format_option.content_type,
			       R2#makeup_format_option.content_type),
      encoding = merge_val(R1#makeup_format_option.encoding,
			   R2#makeup_format_option.encoding) }.
    
%% extend options with default values
default_format_options(R) ->
    Encoding0 = normalize_charset(R#makeup_format_option.encoding),
    Encoding = merge_val(Encoding0, "utf-8"),
    Version  = merge_val(R#makeup_format_option.xml_version, "1.0"),
    CT = merge_val(R#makeup_format_option.content_type, "text/xml"),
    R#makeup_format_option { xml_version = Version,
			      content_type = CT,
			      encoding = Encoding
			     }.

merge_val(undefined,Val2) -> Val2;
merge_val(Val1,undefined) -> Val1;
merge_val(Val1,_Val2) -> Val1.


xml_version(Opts) ->
    [{version,Opts#makeup_format_option.xml_version}].

xml_encoding(Opts) ->
    if Opts#makeup_format_option.encoding == undefined ->
	    [];
       true ->
	    [{encoding,Opts#makeup_format_option.encoding}]
    end.


format(XML) ->
    format(XML, []).

format(Doc, Opts) when is_record(Doc,makeup_document) ->
    DocOpts =
	if Doc#makeup_document.is_xml == true ->
		[{content_type, "text/xml"}];
	   true -> []
	end ++
	case Doc#makeup_document.xml_version of
	    undefined -> [];
	    {Maj,Min} when is_integer(Maj), is_integer(Min) ->
		[{xml_version,integer_to_list(Maj)++"."++integer_to_list(Min)}];
	    Vsn when is_list(Vsn) ->
		[{xml_version,Vsn}]
	end ++
	if Doc#makeup_document.encoding == undefined ->
		[];
	   true ->
		[{encoding,Doc#makeup_document.encoding}]
	end,
    R0 = format_options(DocOpts),
    R1 = format_options(Opts),
    R2 = merge_format_options(R1,R0), %% Opts override DocOpts
    R  = default_format_options(R2),
    OBF = obf(R#makeup_format_option.encoding),
    DocType = Doc#makeup_document.doctype,
    Content = Doc#makeup_document.content,
    Partial = R#makeup_format_option.partial,
    if DocType == undefined;
       Partial == close; Partial == true ->
	    format(Content,OBF,R,[]);
       true ->
	    case R#makeup_format_option.content_type of
		"text/xml" ->
		    XMLProc = append(
				[ "<?xml" |
				  format_attributes(xml_version(R)++
						    xml_encoding(R),["?>\n"])]),
		    {true,OBFXMLProc} = OBF(XMLProc),
		    DOCTYPE = if DocType==undefined -> "";
				 true ->
				      append(
					["<!DOCTYPE " | cat(DocType," ",[">\n"])])
			      end,
		    {true,OBFDOCTYPE} = OBF(DOCTYPE),
		    format(Content,OBF,R, [OBFDOCTYPE, OBFXMLProc]);
		_ ->
		    DOCTYPE = if DocType==undefined -> "";
				 true ->
				      append(
					["<!DOCTYPE " | cat(DocType," ",[">\n"])])
			      end,
		    {true,OBFDOCTYPE} = OBF(DOCTYPE),
		    format(Content,OBF,R, [OBFDOCTYPE])
	    end
    end;
format(XML,Opts) when is_list(XML) ->
    R0 = format_options(Opts),
    R  = default_format_options(R0),
    OBF = obf(R#makeup_format_option.encoding),
    Partial = R#makeup_format_option.partial,
    Acc0 =
	if Partial == close; Partial == true ->
		[];
	   true ->
		case R#makeup_format_option.content_type of
		    "text/xml" ->
			XMLProc = append(
				    [ "<?xml" |
				      format_attributes(xml_version(R)++
							xml_encoding(R),["?>\n"])]),
			{true,OBFXMLProc} = OBF(XMLProc),
			[OBFXMLProc];
		    _ ->
			[]
		end
	end,
    format(XML,OBF,R,Acc0);
format(XML={Tag,As,Cs},Opts) when ?is_tag(Tag),is_list(As),is_list(Cs) ->
    format([XML], Opts);
format(XML={'#PCDATA',_Data},Opts) ->
    format([XML], Opts);
format(XML={'?',_Target,_Value},Opts) ->
    format([XML], Opts);
format(XML={'!--',_Comment},Opts) ->
    format([XML], Opts).

format_begin(XML) ->
    format(XML, [{partial, open}]).

format_begin(XML, Opts) ->
    format(XML, [{partial, open}|Opts]).

format_open(XML, St) when is_record(St, makeup_format_st) ->
    do_format(St, XML, St#makeup_format_st.obf, open,
	      St#makeup_format_st.level,[]).

format_continue(XML, St) when is_record(St, makeup_format_st) ->
    do_format(St, XML, St#makeup_format_st.obf, true,
	      St#makeup_format_st.level,[]).

format_close(XML, St) when is_record(St, makeup_format_st) ->
    %% FIXME: the level should be the same as the open,
    %%        currently this can be achived by passing the same state as 
    %%        the state returned by format_open !
    do_format(St, XML, St#makeup_format_st.obf, close,
	      St#makeup_format_st.level,[]).

format_end(XML, St) when is_record(St, makeup_format_st) ->
    {Data,_} = do_format(St,XML,St#makeup_format_st.obf, close,
			 St#makeup_format_st.level0,[]),
    Data.



format(XML, OBF, R, Acc) ->
    Level = R#makeup_format_option.level,
    Partial = R#makeup_format_option.partial,
    St0 = if R#makeup_format_option.compact == true ->
		  #makeup_format_st { level = Level,
				      level0 = Level,
				      obf    = OBF,
				      tab    = [],
				      nl     = [] };
	     true ->
		  NL = [?NL],
		  TAB = lists:duplicate(R#makeup_format_option.indent,?SP),
		  {true,OBF_NL} = OBF(NL),
		  {true,OBF_TAB} = OBF(TAB),
		  #makeup_format_st { level = Level,
				      level0 = Level,
				      obf    = OBF,
				      tab    = OBF_TAB,
				      nl     = OBF_NL }
	  end,
    do_format(St0, XML, OBF, Partial, Level, Acc).


do_format(St, [H|T], OBF, Partial, Level, Acc) ->
    case H of
	{'#PCDATA',Text} ->
	    if Partial == close ->
		    do_format(St,T,OBF,Partial,Level,Acc);
	       true ->
		    {true,OBFText} = OBF(enc_chars(Text)),
		    do_format(St,T,OBF,Partial,Level,[OBFText|Acc])
	    end;

	{'?',Target,Value} ->
	    if Partial == close ->
		    do_format(St,T,OBF,Partial,Level,Acc);
	       true ->
		    {true,OBF_1}     = OBF("<?"),
		    {true,OBF_2}     = OBF("?>"),
		    {true,OBFTarget} = OBF(atom_to_list(Target)),
		    {true,OBFValue}  = OBF(Value),
		    do_format(St,T,OBF,Partial,Level,
			      [?OBF_NL(St),
			       OBF_2,OBFValue,OBFTarget,OBF_1|Acc])
	    end;

	{'!--',Comment} ->
	    if Partial == close ->
		    do_format(St,T,OBF,Partial,Level,Acc);
	       true ->
		    {true,OBF_1}     = OBF("<!--"),
		    {true,OBF_2}     = OBF("-->"),
		    {true,OBFText} = OBF(Comment),
		    do_format(St,T,OBF,Partial,Level,
			      [?OBF_NL(St),OBF_2,OBFText,OBF_1|Acc])
	    end;

	{Tag,As,[{'#PCDATA',Text}]} ->
	    if Partial == close ->
		    do_format(St,T,OBF,Partial,Level,Acc);
	       true ->
		    do_format(St,T,OBF,Partial,Level,
			      [format_ln(St,Level,format_start_tag(Tag,As),
					 enc_chars(Text),
					 format_end_tag(Tag)) | Acc])
	    end;

	{Tag,As,[]} ->
	    if Partial == open, T == [] ->
		    {reverse([format_ln(St,Level,format_start_tag(Tag,As))|
			      Acc]),
		     save_state(St,Level+1)};
	       Partial == close ->
		    if T==[] ->
			    Data = format_ln(St,Level,format_end_tag(Tag)),
			    {reverse([Data|Acc]),
			     save_state(St,Level)};
		       true ->
			    do_format(St,T,OBF,Partial,Level,Acc)
		    end;
	       true ->
		    do_format(St,T,OBF,Partial,Level,
			      [format_ln(St,Level,
					 format_empty_tag(Tag,As))|Acc])
	    end;

	{Tag,As,Content} ->
	    if Partial == open, T == [] ->
		    {Data,St1} =
			do_format(St,Content,OBF,Partial,Level+1,[]),
		    {reverse([Data,
			      format_ln(St,Level,format_start_tag(Tag,As)) |
			      Acc]),
		     St1};
	       Partial == close ->
		    if T == [] ->
			    {Data,St1} = 
				do_format(St,Content,OBF,Partial,Level+1,[]),
			    {reverse([format_ln(St,Level,format_end_tag(Tag)),
				      Data]), St1};
		       true ->
			    do_format(St,T,OBF,Partial,Level,Acc)
		    end;

	       Partial == true ->
		    {Body,St1} = do_format(St,Content,OBF,Partial,Level+1,[]),
		    do_format(St1,T,OBF,Partial,Level,
			      [format_ln(St,Level,format_end_tag(Tag)),
			       Body,
			       format_ln(St,Level,format_start_tag(Tag,As))|Acc]);
	       true ->
		    do_format(St,T,OBF,Partial,Level,
			      [format_ln(St,Level,format_end_tag(Tag)),
			       do_format(St,Content,OBF,Partial,Level+1,[]),
			       format_ln(St,Level,format_start_tag(Tag,As))|Acc])
	    end;
	{Tag, As} ->
	    if Partial == close ->
		    do_format(St,T,OBF,Partial,Level,Acc);
	       true ->
		    do_format(St,T,OBF,Partial,Level,
			      [format_ln(St,Level,format_empty_tag(Tag,As))|Acc])
	    end;
	_ ->
	    io:format("~w: ignore tag = ~999p\n", [?MODULE, H]),
	    do_format(St,T,OBF,Partial,Level,Acc)
    end;
do_format(St,[],_OBF,Partial,Level,Acc) ->
    if Partial == true ->
	    {reverse(Acc), save_state(St,Level)};
       true ->
	    reverse(Acc)
    end;
do_format(St,T,OBF,Partial,Level,Acc) when is_tuple(T) ->
    do_format(St,[T],OBF,Partial,Level,Acc).

%% format with one 1 data per line
format_ln(St,Level,D1) ->
    OBF = St#makeup_format_st.obf,
    TAB = St#makeup_format_st.tab,
    NL  = St#makeup_format_st.nl,
    if TAB == [], NL == [] ->
	    {true,OBFD1} = OBF(D1),
	    OBFD1;
       TAB == [] ->
	    {true,OBFD1} = OBF(D1),
	    [OBFD1, NL];
       true ->
	    {true,OBFD1} = OBF(D1),
	    [?OBF_INDENT(Level,TAB), OBFD1, NL]
    end.


%% format with 3 data per line
format_ln(St,Level,D1,D2,D3) ->
    OBF = St#makeup_format_st.obf,
    TAB = St#makeup_format_st.tab,
    NL  = St#makeup_format_st.nl,
    if TAB==[], NL==[] ->
	    {true,OBFD1} = OBF(D1),
	    {true,OBFD2} = OBF(D2),
	    {true,OBFD3} = OBF(D3),
	    [OBFD1, OBFD2, OBFD3];
       TAB == [] ->
	    {true,OBFD1} = OBF(D1),
	    {true,OBFD2} = OBF(D2),
	    {true,OBFD3} = OBF(D3),
	    [OBFD1, OBFD2, OBFD3, NL];
       true ->
	    {true,OBFD1} = OBF(D1),
	    {true,OBFD2} = OBF(D2),
	    {true,OBFD3} = OBF(D3),
	    [?OBF_INDENT(Level,TAB), OBFD1, OBFD2, OBFD3, NL]
    end.

     
format_start_tag(Tag, []) when ?is_tag(Tag) ->
    append(["<", format_tag_name(Tag), ">"]);
format_start_tag(Tag, As) when ?is_tag(Tag) ->
    append(["<", format_tag_name(Tag) | format_attributes(As,[">"])]).

format_empty_tag(Tag, []) when ?is_tag(Tag) ->
    append(["<", format_tag_name(Tag), "/>"]);
format_empty_tag(Tag, As) when ?is_tag(Tag) ->
    append(["<", format_tag_name(Tag) | format_attributes(As,["/>"])]).

format_end_tag(Tag) when ?is_tag(Tag) ->
    append(["</", format_tag_name(Tag), ">"]).

format_attributes(As) ->
    format_attributes(As, []).

format_attributes([{K,V}|As],Tail) ->
    [" ", format_attr_name(K), "=\"", value_string(V), "\"" | 
     format_attributes(As,Tail)];
format_attributes([K|As],Tail) ->
    [" ", format_attr_name(K) | format_attributes(As,Tail)];
format_attributes([],Tail) ->
    Tail.


%% Tag or attribute name (with namespace)
format_tag_name(A) when is_atom(A) ->
    atom_to_list(A);
format_tag_name([LNm|A]) when is_atom(LNm),is_atom(A) ->
    atom_to_list(LNm)++":"++atom_to_list(A).

format_attr_name(A) when is_atom(A) ->
    atom_to_list(A);
format_attr_name([LNm|A]) when is_atom(LNm),is_atom(A) ->
    atom_to_list(LNm)++":"++atom_to_list(A).

save_state(St, Level) ->
    St#makeup_format_st { level = Level }.

cat([A],_Sep,Tail) -> [A|Tail];
cat([A|As],Sep,Tail) -> [A,Sep | cat(As,Sep,Tail)];
cat([],_Sep,Tail) -> Tail.


%% Flatten attribute text and escape characters `"', `<' and `&'. (Note
%% that single-quote characters are not escaped; the markup-generating
%% functions (`start_tag', `end_tag', ...) always use `"' to delimit the
%% attribute values.)

enc_chars(T) when is_list(T) ->
    enc_chars(T,[],[]);
enc_chars(T) when is_binary(T) ->
    enc_chars(binary_to_list(T), [], []).

enc_chars(Cs,Cont,Acc) ->
    case Cs of
	[?LT|Cs1] ->
	    enc_chars(Cs1,Cont, [$;,$t,$l,$& | Acc]);
	[?GT|Cs1] ->
	    enc_chars(Cs1,Cont, [$;,$t,$g,$& | Acc]);
	[?AMP|Cs1] ->
	    enc_chars(Cs1,Cont, [$;,$p,$m,$a,$& | Acc]);
	[?APOS|Cs1] ->
	    enc_chars(Cs1,Cont, [$;,$s,$o,$p,$a,$& | Acc]);
	[?QUOT|Cs1] ->
	    enc_chars(Cs1,Cont, [$;,$t,$o,$u,$q,$& | Acc]);
	[C|Cs1] when is_integer(C) ->
	    enc_chars(Cs1,Cont,[C|Acc]);
	[C|Cs1] when is_list(C) ->
	    enc_chars(C,[Cs1|Cont],Acc);
	[] ->
	    case Cont of
		[] -> reverse(Acc);
		[Cs|Cont1] ->
		    enc_chars(Cs,Cont1,Acc)
	    end;
	Bin when is_binary(Bin) ->
	    enc_chars(binary_to_list(Bin),Cont,Acc)
    end.


value_string(I) when is_integer(I) ->
    integer_to_list(I);
value_string(A) when is_atom(A) ->
    enc_chars(atom_to_list(A));
value_string(L) when is_list(L) ->
    enc_chars(L);
value_string(B) when is_binary(B) ->
    enc_chars(binary_to_list(B));
value_string(T) ->
    enc_chars(io_lib:format("~p", [T])).

%% given a charset name
normalize_charset(undefined) ->
    undefined;
normalize_charset(Charset) ->
    case makeup_charset:from_mime_name(Charset) of
	0 ->
	    io:format("Warning: unknown charset: ~s\n",[Charset]),
	    tolower(Charset);
	N ->
	    Charset1 = makeup_charset:to_mime_name(N),
	    tolower(Charset1)
    end.

tolower([C|Cs]) ->
    if (C) >= $A, (C) =< $Z -> 
	    [((C)-$A)+$a | tolower(Cs)];
        true -> 
	    [(C) | tolower(Cs)]
    end;
tolower([]) ->
    [].


obf(Charset) ->
    case Charset of
	"utf-8" ->     fun(Data) -> makeup_utf8:output(Data) end;
	"utf-16" ->    fun (Data) -> makeup_utf16:output(Data) end;
	"utf-16be" ->  fun (Data) -> makeup_utf16_be:output(Data) end;
	"utf-16le" ->   fun (Data) -> makeup_utf16_le:output(Data) end;
	"utf-32"   ->   fun (Data) -> makeup_utf32:output(Data) end;
	"utf-32be" ->   fun (Data) -> makeup_utf32_be:output(Data) end;
	"utf-32le" ->   fun (Data) -> makeup_utf32_le:output(Data) end;
	"us-ascii" ->   fun (Data) -> makeup_ascii:output(Data) end;
	"iso-8859-1" -> fun (Data) -> makeup_iso_8859_1:output(Data) end;
	"iso-8859-2" -> fun (Data) -> makeup_iso_8859_2:output(Data) end;
	"iso-8859-3" -> fun (Data) -> makeup_iso_8859_3:output(Data) end;
	"iso-8859-4" -> fun (Data) -> makeup_iso_8859_4:output(Data) end;
	"iso-8859-5" -> fun (Data) -> makeup_iso_8859_5:output(Data) end;
	"iso-8859-6" -> fun (Data) -> makeup_iso_8859_6:output(Data) end;
	"iso-8859-7" -> fun (Data) -> makeup_iso_8859_7:output(Data) end;
	"iso-8859-8" -> fun (Data) -> makeup_iso_8859_8:output(Data) end;
	"iso-8859-9" -> fun (Data) -> makeup_iso_8859_9:output(Data) end;
	_ -> fun(Data) -> makeup_utf8:output(Data) end
    end.

