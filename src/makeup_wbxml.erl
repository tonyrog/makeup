%%% File    : makeup_wbxml.erl
%%% Author  : Tony Rogvall <tony@pbook.synap.se>
%%% Description : WBXML Encoder/Decoder
%%% Created : 11 May 2006 by Tony Rogvall <tony@pbook.synap.se>

-module(makeup_wbxml).

-rcsid("$Id: makeup_wbxml.erl,v 1.14 2007/09/04 07:31:10 per Exp $\n").

-vsn("$Revision: 1.14 $ ").

-define(dbgi(Fmt,As), io:format((Fmt),(As))).

-ifdef(debug).
-define(dbg(Fmt,As), io:format((Fmt),(As))).
-else.
-define(dbg(Fmt,As), ok).
-endif.

-include("../include/makeup.hrl").

-define(DEFAULT_CBMOD, makeup_tags).


-export([decode/1, 
	 decode/2, 
	 decode/3, 
	 decode/4]).
-export([debug_decode/1,
	 debug_decode/2,
	 debug_decode/3,
	 debug_decode/4,
	 debug_decode/5]).
-export([encode/1, encode/2]).
-export([encode_begin/1, encode_begin/2, 
	 encode_open/2, encode_close/2, 
	 encode_continue/2, encode_end/2]).
-export([encode_size/1, encode_size/2]).

-export([pubid_to_module/1]).
-export([version_to_code/1]).

-import(lists, [reverse/1,append/1]).

-define(SWITCH_PAGE, 16#00).    %% change code page u_int8
-define(END,         16#01).    %% end of attribute or element
-define(ENTITY,      16#02).    %% mb_u_int32
-define(STR_I,       16#03).    %% inline string

-define(EXT_I0,     16#40).
-define(EXT_I1,     16#41).
-define(EXT_I2,     16#42).
-define(PI,         16#43).

-define(EXT_T0,     16#80).
-define(EXT_T1,     16#81).
-define(EXT_T2,     16#82).
-define(STR_T,      16#83).

-define(EXT0,       16#C0).
-define(EXT1,       16#C1).
-define(EXT2,       16#C2).
-define(OPAQUE,     16#C3).

-define(LITERAL,     16#04).    %% unknown attribute name mb_u_int32 offset
-define(LITERAL_A,   16#84).
-define(LITERAL_C,   16#44).    %% unknown tag
-define(LITERAL_AC,  16#C4).

%% 1.0
%% ---
%% 0 public identifier is encoded as a literal in the string table. 
%% 1 Unknown or missing public identifier. 
%% 2 "-//WAPFORUM//DTD WML 1.0//EN" (WML 1.0) 
%% 3 "-//WAPFORUM//DTD WTA 1.0//EN" (WTA Event 1.0) 4 - 7F Reserved

%% 1.1
%% ---
%% 0 public identifier is encoded as a literal in the string table. 
%% 1 Unknown or missing public identifier. 
%% 2 "-//WAPFORUM//DTD WML 1.0//EN" (WML 1.0) 
%% 3 "-//WAPFORUM//DTD WTA 1.0//EN" (WTA Event 1.0) 
%% 4 "-//WAPFORUM//DTD WML 1.1//EN" (WML 1.1) 
%% 5 - 7F Reserved
%%
%% 1.2
%% ---
%% 0 public identifier is encoded as a literal in the string table. 
%% 1 Unknown or missing public identifier. 
%% 2 "-//WAPFORUM//DTD WML 1.0//EN" (WML 1.0) 
%% 3 DEPRECATED "-//WAPFORUM//DTD WTA 1.0//EN" (WTA Event 1.0) 
%% 4 "-//WAPFORUM//DTD WML 1.1//EN" (WML 1.1) 
%% 5 "-//WAPFORUM//DTD SI 1.0//EN" (Service Indication 1.0) 
%% 6 "-//WAPFORUM//DTD SL 1.0//EN" (Service Loading 1.0) 
%% 7 "-//WAPFORUM//DTD CO 1.0//EN" (Cache Operation 1.0) 
%% 8 "-//WAPFORUM//DTD CHANNEL 1.1//EN" (Channel 1.1) 
%% 9 "-//WAPFORUM//DTD WML 1.2//EN" (WML 1.2) 
%% A - 7F Reserved
%%
%% 1.3
%% ---
%% 0 public identifier is encoded as a literal in the string table. 
%% 1 Unknown or missing public identifier. 
%% 2 "-//WAPFORUM//DTD WML 1.0//EN" (WML 1.0) 
%% 3 DEPRECATED "-//WAPFORUM//DTD WTA 1.0//EN" (WTA Event 1.0) 
%% 4 "-//WAPFORUM//DTD WML 1.1//EN" (WML 1.1) 
%% 5 "-//WAPFORUM//DTD SI 1.0//EN" (Service Indication 1.0) 
%% 6 "-//WAPFORUM//DTD SL 1.0//EN" (Service Loading 1.0) 
%% 7 "-//WAPFORUM//DTD CO 1.0//EN" (Cache Operation 1.0) 
%% 8 "-//WAPFORUM//DTD CHANNEL 1.1//EN" (Channel 1.1) 
%% 9 "-//WAPFORUM//DTD WML 1.2//EN" (WML 1.2) 
%% A  -//WAPFORUM//DTD WML 1.3//EN  (WML 1.3) 
%% B  -//WAPFORUM//DTD PROV 1.0//EN  (Provisioning 1.0) 
%% C  -//WAPFORUM//DTD WTA-WML 1.2//EN  (WTA-WML 1.2) 
%% D  -//WAPFORUM//DTD CHANNEL 1.2//EN  (Channel 1.2) 
%% E- 7F Reserved
%%
-define(WBXML_string_table,  16#0).
-define(WBXML_unknown,       16#1).
-define(WBXML_wml_10,        16#2).
-define(WBXML_wta_10,        16#3). %% Deprecated in 1.2
-define(WBXML_wml_11,        16#4).
-define(WBXML_si_10,         16#5).
-define(WBXML_sl_10,         16#6).
-define(WBXML_co_10,         16#7).
-define(WBXML_channel_11,    16#8).
-define(WBXML_wml_12,        16#9).
-define(WBXML_wml_13,        16#A).
-define(WBXML_prov_10,       16#B).
-define(WBXML_wta_wml_12,    16#C).
-define(WBXML_channel_12,    16#D).

-define(WBXML_10,            16#00).
-define(WBXML_11,            16#01).
-define(WBXML_12,            16#02).
-define(WBXML_13,            16#03).


-record(wbxml_decf,
	{
	  tagf,  %% fun(Code,Page) -> undefined | {Tag,
	  atrf,  %% fun(Code,Page) -> undefined | {Attribute,
	  valf,  %% fun(Code,Page) -> undefined | {Value,}
	  extf   %% fun(What,TagCtx,Page,ExtVal) -> undefined | Value
	 }).

-record(wbxml_dec,
	{
	  version        = ?WBXML_11,
	  tag_page       = 0,
	  attribute_page = 0,
	  charset,
	  strtbl = {0,[]},
	  mod,
	  logfd = user,          %% Where to debug log
	  decf,                  %% #wbxml_decf {}
	  cstate,                %% client state
	  callback               %% callback module
	 }).

-record(wbxml_encf,
	{
	  tagf,  %% fun(Tag,Page) -> undefined | {Code,Page'}
	  atrf,  %% fun(Attriute,Value,Page,Map) -> undefined | {Code,Page',V'}
	  valf,  %% fun(Attribute,Value,Page) -> undefined | {Code,Page',Value'}
	  extf   %% fun(What,TagCtx,Page,Value) -> undefined | ...
	 }).

-record(wbxml_opt,
	{
	  empty_value_string=false,
	  codepage_map=[],
	  pubid = "",
	  version = {1,1},
	  charset = "utf-8",
	  env = [],
	  mod,
	  partial = false,
	  literals = []
	 }).
	  
-record(wbxml_enc,
	{
	  version        = ?WBXML_11,
	  tag_page       = 0,
	  attribute_page = 0,
	  strtbl = {0,[]},
	  env = [],
	  mod,
	  encf,              %% #wbxml_encf {}
	  tagdata,           %% Current {Tag,TagEncoding} (for partial mode)
	  partial = false,   %% Partial mode, false,true,open,close
	  space = remove,    %% preserve | remove | normalize
	  empty = false,     %% special treat "" value i.e [STR_I, 0]
	  pagemap = []       %% page mapping
	 }).

-define(CONS(H,T),
	if (H) == [] -> (T);
	   true -> [(H) | (T)]
	end).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% WBXML Decoder
%%
%% return {ok,#makeup_document{}}
%% or     {error, Reason}
%%
%% FIXME: make a special decoder for debug!!!
%%        that will emit the codes...
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%
%% start = version publicid charset strtbl body
%%
decode(Data) ->
    decode(Data, undefined, ?DEFAULT_CBMOD, []).

decode(Data, Mod) ->
    decode(Data, Mod, ?DEFAULT_CBMOD, []).

decode(Data, Mod, CbMod) ->
    decode(Data, Mod, CbMod, []).

decode(Data, Mod, CbMod, CbOpts) when is_binary(Data) ->
    decode(binary_to_list(Data), Mod, CbMod, CbOpts);

decode([VersionCode|Data], Mod, CbMod, CbOpts) ->
    {PubCode,Data1}      = decode_pubcode(Data),
    {CharSetCode,Data2}  = decode_charset(Data1),
    {StrTbl,Data3}       = decode_strtbl(Data2),
    CSt = CbMod:init(CbOpts),
    St0 = #wbxml_dec { charset = CharSetCode, 
		       strtbl = StrTbl,
		       version=VersionCode,
		       callback = CbMod, 
		       cstate = CSt },
    Mod2 = case pubcode_to_module(PubCode,St0) of
	       undefined -> Mod;
	       Mod1 -> Mod1
	   end,
    %% Prefere user supplied module before PubCode derived !!!
    Module = if Mod == undefined -> Mod2;
		true -> Mod
	     end,
    DecodeIf = decode_interface(Module),
    {St1,_Data4}  = decode_body(Data3,St0#wbxml_dec { mod = Module, decf=DecodeIf }),
    {ok, Doc} = CbMod:final("",0,St1#wbxml_dec.cstate),
    Encoding = if Doc#makeup_document.encoding == undefined ->
		       makeup_charset:to_mime_name(CharSetCode);
		  true ->
		       Doc#makeup_document.encoding
	       end,
    DocType = case module_to_pubid(Module) of
		  undefined -> undefined;
		  {StartTag,PUBID} ->
		      [StartTag,"PUBLIC","\""++PUBID++"\""]
	      end,
    {ok, Doc#makeup_document { is_xml   = true,
			       encoding = Encoding,
			       doctype  = DocType,
			       xml_version = {1,0},
			       wbxml_version = code_to_version(VersionCode),
			       wbxml_module  = Module }}.

decode_pubcode([0|Data]) ->
    {Code,Data1} = decode_mb_u_int32(Data),
    {{tref,Code}, Data1};
decode_pubcode(Data) ->
    decode_mb_u_int32(Data).

decode_charset(Data) ->
    decode_mb_u_int32(Data).

%% 
%% body = *pi element *pi
%%
decode_body(Data, St0) ->
    {St1,Data1}  = decode_pi_n(Data,St0),
    {St2,Data2}  = decode_element(Data1, St1),
    decode_pi_n(Data2,St2).

%% 
%% *pi
%%
decode_pi_n(Data0 = [?PI|_], St0) ->
    {St1,Data1} = decode_pi(Data0, St0),
    decode_pi_n(Data1, St1);
decode_pi_n(Data, St0) ->
    {St0,Data}.

%% 
%% pi = PI attrStart *attrValue END
%% 
%%
decode_pi([?PI|Data], St0) ->
    {{Target,Value},St1,[?END|Data1]} = decode_attribute(Data, St0),
    Callback = St1#wbxml_dec.callback,
    CSt0 = St1#wbxml_dec.cstate,
    CSt1 = Callback:processing(Target, Value, 0, CSt0),
    {St1#wbxml_dec { cstate = CSt1 }, Data1}.

%% 
%% element = ([switchPage] stag) [ 1*attribute END ] [ *content END]
%%
decode_element([?SWITCH_PAGE,Index|Data], St0) ->
    decode_element(Data, St0#wbxml_dec { tag_page = Index });
decode_element([], St0) ->
    {St0, []};
decode_element(Data, St0) ->
    {Tag,AP,CP,Data1} = decode_stag(Data, St0),
    {St1,Data2} = if AP == true ->
			  decode_attribute_n(Data1,Tag,St0);
		     true ->
			  {St0,Data1}
		  end,
    Callback = St1#wbxml_dec.callback,
    CSt1 = St1#wbxml_dec.cstate,

    CSt2 = Callback:tag_begin(Tag,0,CSt1),
    St2 = St1#wbxml_dec { cstate = CSt2 },
    {St3,Data3} = if CP == true ->
			  decode_content_n(Data2,[],St2);
		     true ->
			  {St2,Data2}
		  end,
    CSt3 = Callback:tag_end(Tag,0,St3#wbxml_dec.cstate),
    { St3#wbxml_dec { cstate = CSt3 }, Data3}.

%%
%% Decode of tags & literal tag
%%
decode_stag([T|Data],St) ->
    Tg = T band 16#3f,
    AP  = T band 16#80 =/= 0,
    CP  = T band 16#40 =/= 0,
    if Tg == ?LITERAL ->
	    {Lit,Data1} = decode_mb_u_int32(Data),
	    {list_to_atom(lookup_string(Lit, St)), AP, CP, Data1};
       true ->
	    {decode_tag(Tg, St), AP, CP, Data}
    end.

decode_tag(Tg, St) ->
    Page = St#wbxml_dec.tag_page,
    case ((St#wbxml_dec.decf)#wbxml_decf.tagf)(Tg,Page) of
	undefined -> 
	    %% FIXME: Eh DONT use tag = undefined, so far no tab file is!
	    list_to_atom("TAG_"++integer_to_list(Page)++"_"
			 ++integer_to_list(Tg));
	TAG when is_atom(TAG) -> 
	    TAG
    end.

%%
%% *content END
%%

decode_content_n([?SWITCH_PAGE,Page|Data],Text,St0) ->
    decode_content_n(Data,Text,St0#wbxml_dec { tag_page = Page });
decode_content_n([?END|Data],Text,St0) ->
    {call_text(Text, St0), Data};
decode_content_n(Data,Text,St0) ->
    decode_content_1(Data,Text,St0).

%%
%% content = element | string | extension | entity | pi | opaque
%%
    
decode_content_1(Data0=[Code|Data],Text,St0) ->
    case Code of
	?ENTITY ->
	    {Int,Data1} = decode_mb_u_int32(Data),
	    decode_content_n(Data1,[[Int]|Text],St0);
	?STR_I ->
	    {String,Data1} = get_cstring(Data),
	    decode_content_n(Data1,[String|Text],St0);
	?PI ->
	    St1 = call_text(Text, St0),
	    {St2,Data1} = decode_pi([?PI|Data],St1),
	    decode_content_n(Data1, [], St2);

	?OPAQUE when St0#wbxml_dec.version > ?WBXML_10 -> %% CHECK THIS
	    {Length,Data1} = decode_mb_u_int32(Data),
	    {Opaque,Data2} = get_n_bytes(Data1,Length),
	    decode_content_n(Data2,[ Opaque|Text],St0);

	?STR_T ->
	    {Index,Data1} = decode_mb_u_int32(Data),
	    String = lookup_string(Index, St0),
	    decode_content_n(Data1,[String|Text],St0);

	?EXT_I0 ->
	    {Ext,Data1} = get_cstring(Data),
	    String = decode_content_extension(ext0, Ext, St0),
	    decode_content_n(Data1,[String|Text],St0);

	?EXT_I1 ->
	    {Ext,Data1} = get_cstring(Data),
	    String = decode_content_extension(ext1, Ext, St0),
	    decode_content_n(Data1,[String|Text],St0);

	?EXT_I2 ->
	    {Ext,Data1} = get_cstring(Data),
	    String = decode_content_extension(ext2, Ext, St0),
	    decode_content_n(Data1,[String|Text],St0);

	?EXT_T0 ->
	    {Int,Data1} = decode_mb_u_int32(Data),
	    Ext = lookup_string(Int, St0),
	    String = decode_content_extension(ext0, Ext, St0),
	    decode_content_n(Data1,[String|Text],St0);

	?EXT_T1 ->
	    {Int,Data1} = decode_mb_u_int32(Data),
	    Ext = lookup_string(Int, St0),
	    String = decode_content_extension(ext1, Ext, St0),
	    decode_content_n(Data1,[String|Text],St0);

	?EXT_T2 ->
	    {Int,Data1} = decode_mb_u_int32(Data),
	    Ext = lookup_string(Int, St0),
	    String = decode_content_extension(ext2, Ext, St0),
	    decode_content_n(Data1,[String|Text],St0);

	?EXT0 ->
	    String = decode_content_extension(ext0, undefined, St0),
	    decode_content_n(Data,[String|Text],St0);

	?EXT1 ->
	    String = decode_content_extension(ext1, undefined, St0),
	    decode_content_n(Data,[String|Text],St0);

	?EXT2 ->
	    String = decode_content_extension(ext2, undefined, St0),
	    decode_content_n(Data,[String|Text],St0);
	_ ->
	    St1 = call_text(Text, St0),
	    {St2,Data1} = decode_element(Data0,St1),
	    decode_content_n(Data1,[],St2)
    end.

call_text([], St0) ->
    St0;
call_text(RTextList, St0) ->
    Callback = St0#wbxml_dec.callback,
    Text = append(reverse(RTextList)),
    CSt1 = Callback:text(Text, 0, St0#wbxml_dec.cstate),
    St0#wbxml_dec { cstate = CSt1 }.

%%
%% *attribute END
%%

decode_attribute_n([?END|Data], _Tag, St0) ->
    {St0, Data};
decode_attribute_n(Data, Tag, St0) ->
    case decode_attribute(Data,St0) of
	{{Attr,Value},St1,Data1} ->
	    Callback = St0#wbxml_dec.callback,
	    CSt=Callback:attribute(Tag,Attr,Value,0,St1#wbxml_dec.cstate),
	    decode_attribute_n(Data1,Tag,St1#wbxml_dec { cstate = CSt });
	{Attr,St1,Data1} ->
	    Callback = St0#wbxml_dec.callback,
	    CSt=Callback:attribute(Tag,Attr,0,St1#wbxml_dec.cstate),
	    decode_attribute_n(Data1,Tag,St1#wbxml_dec { cstate = CSt })
    end.

%%
%% attribute = attrStart *attrValue
%%
decode_attribute([?SWITCH_PAGE,Page|Data],St0) ->
    decode_attribute(Data,St0#wbxml_dec { attribute_page = Page });
decode_attribute([?LITERAL|Data],St0) ->
    {Index,Data1} = decode_mb_u_int32(Data),
    Attr = list_to_atom(lookup_string(Index, St0)),
    case Data1 of
	[?END|_]                  -> {Attr,St0,Data1};
	[?LITERAL|_]              -> {Attr,St0,Data1};
	[A|_] when A > 3, A < 128 -> {Attr,St0,Data1};
	_ -> 
	    {Value,St1,Data3} = decode_attrValue_n(Data1,St0),
	    {{Attr,Value},St1,Data3}
    end;
decode_attribute([Code|Data],St0) when Code < 128 ->
    Page = St0#wbxml_dec.attribute_page,
    {Attr,Prefix} =
	case ((St0#wbxml_dec.decf)#wbxml_decf.atrf)(Code,Page) of
	    undefined ->
		{list_to_atom("ATTR_"++integer_to_list(Page)++"_"
			      ++integer_to_list(Code)),""};
	    AttrVal -> AttrVal
	end,
    case Data of
	[?END|_]                  -> {{Attr,Prefix},St0,Data};
	[?LITERAL|_]              -> {{Attr,Prefix},St0,Data};
	[A|_] when A > 3, A < 128 -> {{Attr,Prefix},St0,Data};
	_ -> 
	    {Value,St1,Data1} = decode_attrValue_n(Data,St0),
	    {{Attr,Prefix++Value},St1,Data1}
    end.

%%
%% attrValue = ([switchPage]ATTRVALUE) | string | extension | entity | opaque
%%

decode_attrValue(Data0=[Code|Data], St0) ->
    case Code of
	?SWITCH_PAGE ->
	    [Page|Data1] = Data,
	    decode_attrValue(Data1, St0#wbxml_dec { attribute_page = Page });
	?ENTITY ->
	    {Int,Data1} = decode_mb_u_int32(Data),
	    {[Int],St0,Data1};
	?STR_I ->
	    {String,Data1} = get_cstring(Data),
	    {String,St0,Data1};
	?STR_T ->
	    {Index,Data1} = decode_mb_u_int32(Data),
	    String = lookup_string(Index, St0),
	    {String,St0,Data1};
	?OPAQUE when St0#wbxml_dec.version > ?WBXML_10 ->
	    {Length,Data1} = decode_mb_u_int32(Data),
	    {Opaque,Data2} = get_n_bytes(Data1,Length),
	    String = decode_value_extension(opaque, Opaque, St0),
	    {String,St0,Data2};
	?EXT_I0 ->
	    {Ext,Data1} = get_cstring(Data),
	    String = decode_value_extension(ext0, Ext, St0),
	    {String, St0, Data1};
	?EXT_I1 ->
	    {Ext,Data1} = get_cstring(Data),
	    String = decode_value_extension(ext1, Ext, St0),
	    {String, St0, Data1};
	?EXT_I2 ->
	    {Ext,Data1} = get_cstring(Data),
	    String = decode_value_extension(ext2, Ext, St0),
	    {String, St0, Data1};
	?EXT_T0 ->
	    {Int,Data1} = decode_mb_u_int32(Data),
	    Ext = lookup_string(Int, St0),
	    String = decode_value_extension(ext0, Ext, St0),
	    {String, St0, Data1};
	?EXT_T1 ->
	    {Int,Data1} = decode_mb_u_int32(Data),
	    Ext = lookup_string(Int, St0),
	    String = decode_value_extension(ext1, Ext, St0),
	    {String, St0, Data1};
	?EXT_T2 ->
	    {Int,Data1} = decode_mb_u_int32(Data),
	    Ext = lookup_string(Int, St0),
	    String = decode_value_extension(ext2, Ext, St0),
	    {String, St0, Data1};
	?EXT0 ->
	    String = decode_value_extension(ext0, undefined, St0),
	    {String, St0, Data};
	?EXT1 ->
	    String = decode_value_extension(ext1, undefined, St0),
	    {String, St0, Data};
	?EXT2 ->
	    String = decode_value_extension(ext2, undefined, St0),
	    {String, St0, Data};
	V when V >= 128 ->
	    Value = case ((St0#wbxml_dec.decf)#wbxml_decf.valf)
			(V,St0#wbxml_enc.attribute_page) of
			undefined ->
			    "VALUE"++integer_to_list(V);
			Val -> Val
		    end,
	    {Value,St0,Data};
	_ ->
	    {false,St0,Data0}
    end.


decode_attrValue_n(Data, St0) ->
    decode_attrValue_n(Data, [], St0).

decode_attrValue_n(Data=[_Code|_], Acc, St0) ->
    case decode_attrValue(Data, St0) of
	{false, St1, Data1} ->
	    {Acc, St1, Data1};
	{Val, St1, Data1} ->
	    decode_attrValue_n(Data1, Acc++Val, St1)
    end;
decode_attrValue_n([], Acc, St0) ->
    {Acc, St0, []}.

%%
%% EXTENSION(s)
%%
decode_content_extension(Ext,ExtVal,St) ->
    case ((St#wbxml_dec.decf)#wbxml_decf.extf)
	(content,Ext, St#wbxml_dec.tag_page, ExtVal) of
	undefined ->
	    "";
	String -> 
	    String
    end.

decode_value_extension(Ext, ExtVal, St) ->
    case ((St#wbxml_dec.decf)#wbxml_decf.extf)
	(value,Ext,St#wbxml_dec.attribute_page, ExtVal) of
	undefined ->
	    "";
	String -> 
	    String
    end.


%% decode multibyte integer.
decode_mb_u_int32([X|Data]) ->
    if X > 16#7f ->
	    decode_mb_u_int32(Data, X band 16#7f);
       true ->
	    {X, Data}
    end.


decode_mb_u_int32([X|Data], X0) ->
    if X > 16#7f ->
	    decode_mb_u_int32(Data, (X0 bsl 7) + (X band 16#7f));
       true ->
	    {(X0 bsl 7)+X, Data}
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% WBXML DEBUG Decoder
%%
%% return {ok,#makeup_document{}}
%% or     {error, Reason}
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%
%% start = version publicid charset strtbl body
%%
debug_decode(Data) when is_binary(Data) ->
    debug_decode(Data, undefined, ?DEFAULT_CBMOD, [], user).

debug_decode(Data, Mod) ->
    debug_decode(Data, Mod, ?DEFAULT_CBMOD, [], user).

debug_decode(Data, Mod, CbMod) ->
    debug_decode(Data, Mod, CbMod, [], user).

debug_decode(Data, Mod, CbMod, CbOpts) ->
    debug_decode(Data, Mod, CbMod, CbOpts, user).

debug_decode(Data, Mod, CbMod, CbOpts, LogFd) when is_binary(Data) ->
    debug_decode(binary_to_list(Data), Mod, CbMod, CbOpts, LogFd);
debug_decode(Data0=[VersionCode|Data], Mod, CbMod, _CbOpts, LogFd) ->
    {PubCode,Data1}     = decode_pubcode(Data),
    {CharSetCode,Data2} = decode_charset(Data1),
    {StrTbl,Data3} = decode_strtbl(Data2),
    CSt = CbMod:init([]),
    St0 = #wbxml_dec { charset = CharSetCode, 
		       strtbl = StrTbl,
		       version=VersionCode,
		       logfd = LogFd,
		       callback = CbMod, 
		       cstate = CSt },
    Mod2 = case pubcode_to_module(PubCode,St0) of
	       undefined -> Mod;
	       Mod1 -> Mod1
	   end,
    PUBID = case module_to_pubid(Mod2) of
		undefined -> "";
		{_StartTag,ID} -> ID
	    end,
    log2(St0, "~s", " PubCode ~w : \"~s\"", 
	 [fmt_bytes(Data0,Data1)], [PubCode,PUBID]),
    
    log2(St0, "~s", " CharSetCode ~w: \"~s\"",
	 [fmt_bytes(Data1,Data2)], 
	 [CharSetCode, 
	  makeup_charset:to_mime_name(CharSetCode)]),
    log(St0, "%% STRING TABLE BEGIN (TREF)", []),
    lists:foreach(
      fun({Pos0, _Pos1,String}) ->
	      log2(St0, "~w", "~s", [Pos0],[String])
      end, reverse(element(2,StrTbl))),
    log(St0, "%% STRING TABLE END", []),


    %% Prefere user supplied module before PubCode derived !!!
    Module = if Mod == undefined -> Mod2;
		true -> Mod
	     end,
    DecodeIf = decode_interface(Module),
    {St1,_Data4} = debug_decode_body(Data3,St0#wbxml_dec { mod = Module, decf=DecodeIf }),
    {ok, Doc} = CbMod:final("",0,St1#wbxml_dec.cstate),
    Encoding = if Doc#makeup_document.encoding == undefined ->
		       makeup_charset:to_mime_name(CharSetCode);
		  true ->
		       Doc#makeup_document.encoding
	       end,
    DocType = case module_to_pubid(Module) of
		  undefined -> undefined;
		  {StartTag,PUBID} ->
		      [StartTag,"PUBLIC","\""++PUBID++"\""]
	      end,
    {ok, Doc#makeup_document { is_xml   = true,
			       encoding = Encoding,
			       doctype  = DocType,
			       xml_version = {1,0},
			       wbxml_version = code_to_version(VersionCode),
			       wbxml_module  = Module }}.

%% 
%% body = *pi element *pi
%%
debug_decode_body(Data, St0) ->
    {St1,Data1}  = debug_decode_pi_n(Data,St0),
    {St2,Data2}  = debug_decode_element(Data1, St1),
    debug_decode_pi_n(Data2,St2).

%% 
%% *pi
%%
debug_decode_pi_n(Data0 = [?PI|_], St0) ->
    {St1,Data1} = debug_decode_pi(Data0, St0),
    debug_decode_pi_n(Data1, St1);
debug_decode_pi_n(Data, St0) ->
    {St0,Data}.

%% 
%% pi = PI attrStart *attrValue END
%% 
%%
debug_decode_pi([?PI|Data], St0) ->
    log2(St0, "~2.16.0B","PI", [?PI],[]),
    {{Target,Value},St1,[?END|Data1]} = debug_decode_attribute(Data, St0),
    Callback = St1#wbxml_dec.callback,
    CSt0 = St1#wbxml_dec.cstate,
    CSt1 = Callback:processing(Target, Value, 0, CSt0),
    {St1#wbxml_dec { cstate = CSt1 }, Data1}.

%% 
%% element = ([switchPage] stag) [ 1*attribute END ] [ *content END]
%%
debug_decode_element([?SWITCH_PAGE,Page|Data], St0) ->
    log2(St0, "~2.16.0B ~2.16.0B", " SWITCH_PAGE ~w -> ~w", 
	 [?SWITCH_PAGE,Page],[St0#wbxml_dec.tag_page,Page]),
    debug_decode_element(Data, St0#wbxml_dec { tag_page = Page });
debug_decode_element([], St0) ->
    {St0, []};
debug_decode_element(Data, St0) ->
    {Tag,AP,CP,Data1} = debug_decode_stag(Data, St0),
    log2(St0, "~s", "<~s>", [fmt_bytes(Data,Data1)],[Tag]),
    {St1,Data2} = if AP == true ->
			  debug_decode_attribute_n(Data1,Tag,St0);
		     true ->
			  {St0,Data1}
		  end,
    Callback = St1#wbxml_dec.callback,
    CSt1 = St1#wbxml_dec.cstate,

    CSt2 = Callback:tag_begin(Tag,0,CSt1),
    St2 = St1#wbxml_dec { cstate = CSt2 },
    {St3,Data3} = if CP == true ->
			  debug_decode_content_n(Data2,[],St2);
		     true ->
			  {St2,Data2}
		  end,
    CSt3 = Callback:tag_end(Tag,0,St3#wbxml_dec.cstate),
    { St3#wbxml_dec { cstate = CSt3 }, Data3}.

%%
%% Decode of tags & literal tag
%%
debug_decode_stag([T|Data],St) ->
    Tg = T band 16#3f,
    AP  = T band 16#80 =/= 0,
    CP  = T band 16#40 =/= 0,
    if Tg == ?LITERAL ->
	    {Lit,_Bytes,Data1} = debug_decode_mb_u_int32(Data),
	    {list_to_atom(lookup_string(Lit, St)), AP, CP, Data1};
       true ->
	    {debug_decode_tag(Tg, St), AP, CP, Data}
    end.

debug_decode_tag(Tg, St) ->
    Page = St#wbxml_dec.tag_page,
    case ((St#wbxml_dec.decf)#wbxml_decf.tagf)(Tg,Page) of
	undefined -> 
	    %% FIXME: Eh DONT use tag = undefined, so far no tab file is!
	    list_to_atom("TAG_"++integer_to_list(Page)++"_"
			 ++integer_to_list(Tg));
	TAG when is_atom(TAG) -> 
	    TAG
    end.

%%
%% *content END
%%

debug_decode_content_n([?SWITCH_PAGE,Page|Data],Text,St0) ->
    log2(St0,"~2.16.0B ~2.16.0B"," SWITCH_PAGE ~w -> ~w", 
	 [?SWITCH_PAGE, Page], [St0#wbxml_dec.tag_page, Page]),
    debug_decode_content_n(Data,Text,St0#wbxml_dec { tag_page = Page });
debug_decode_content_n([?END|Data],Text,St0) ->
    log2(St0,"~2.16.0B", " END", [?END], []),
    {call_text(Text, St0), Data};
debug_decode_content_n(Data,Text,St0) ->
    debug_decode_content_1(Data,Text,St0).

%%
%% content = element | string | extension | entity | pi | opaque
%%
    
debug_decode_content_1(Data0=[Code|Data],Text,St0) ->
    case Code of
	?ENTITY ->
	    {Int,_Bytes,Data1} = debug_decode_mb_u_int32(Data),
	    log2(St0, "~2.16.0B ~s", " Entity: ~p", 
		 [?ENTITY, fmt_bytes(Data, Data1)], [Int]),
	    debug_decode_content_n(Data1,[[Int]|Text],St0);
	?STR_I ->
	    {String,Data1} = get_cstring(Data),
	    log2(St0, "~2.16.0B ~s", " STR_I: ~999p", 
		[?STR_I, fmt_bytes(Data, Data1)],[String]),
	    debug_decode_content_n(Data1,[String|Text],St0);
	?PI ->
	    St1 = call_text(Text, St0),
	    {St2,Data1} = debug_decode_pi([?PI|Data],St1),
	    debug_decode_content_n(Data1, [], St2);

	?OPAQUE when St0#wbxml_dec.version > ?WBXML_10 -> %% CHECK THIS
	    {Length,Bytes,Data1} = debug_decode_mb_u_int32(Data),
	    log2(St0, "~s", " LENGTH=~w", [fmt_byte_list(Bytes)],[Length]),
	    {Opaque,Data2} = get_n_bytes(Data1,Length),
	    log2(St0, "~s", " OPAQUE", [fmt_byte_list(Opaque)],[]),
	    debug_decode_content_n(Data2,[ Opaque|Text],St0);

	?STR_T ->
	    {Index,Bytes,Data1} = debug_decode_mb_u_int32(Data),
	    String = lookup_string(Index, St0),
	    log2(St0,"~s", " STR_T: ~w = ~s", 
		 [fmt_byte_list(Bytes)], [Index, String]),
	    debug_decode_content_n(Data1,[String|Text],St0);

	?EXT_I0 ->
	    {Ext,Data1} = get_cstring(Data),
	    String = debug_decode_content_extension(ext0, Ext, St0),
	    log2(St0,"~2.16.0B ~s"," EXT_I0: ~w = ~p", 
		 [?EXT_I0,fmt_bytes(Data,Data1)],[Ext,String]),
	    debug_decode_content_n(Data1,[String|Text],St0);

	?EXT_I1 ->
	    {Ext,Data1} = get_cstring(Data),
	    String = debug_decode_content_extension(ext1, Ext, St0),
	    log2(St0,"~2.16.0B ~s", " EXT_I1: ~w = ~p",
		 [?EXT_I1,fmt_bytes(Data,Data1)],[Ext,String]),
	    debug_decode_content_n(Data1,[String|Text],St0);

	?EXT_I2 ->
	    {Ext,Data1} = get_cstring(Data),
	    String = debug_decode_content_extension(ext2, Ext, St0),
	    log2(St0,"~2.16.0B ~s", " EXT_I1: ~w = ~p",
		[?EXT_I2,fmt_bytes(Data,Data1)],[Ext,String]),
	    debug_decode_content_n(Data1,[String|Text],St0);

	?EXT_T0 ->
	    {Int,Bytes,Data1} = debug_decode_mb_u_int32(Data),
	    Ext = lookup_string(Int, St0),
	    String = debug_decode_content_extension(ext0, Ext, St0),
	    log2(St0,"~2.16.0B ~s", " EXT_T0: ~w = ~p",
		 [?EXT_T0,fmt_byte_list(Bytes)],[Ext,String]),
	    debug_decode_content_n(Data1,[String|Text],St0);

	?EXT_T1 ->
	    {Int,Bytes,Data1} = debug_decode_mb_u_int32(Data),
	    Ext = lookup_string(Int, St0),
	    String = debug_decode_content_extension(ext1, Ext, St0),
	    log2(St0,"~2.16.0B ~s", " EXT_T1: ~w = ~p",
		[?EXT_T0,fmt_byte_list(Bytes)],[Ext,String]),
	    debug_decode_content_n(Data1,[String|Text],St0);

	?EXT_T2 ->
	    {Int,Bytes,Data1} = debug_decode_mb_u_int32(Data),
	    Ext = lookup_string(Int, St0),
	    String = debug_decode_content_extension(ext2, Ext, St0),
	    log2(St0,"~2.16.0B ~s", " EXT_T2: ~w = ~p",
		 [?EXT_T0,fmt_byte_list(Bytes)],[Ext,String]),
	    debug_decode_content_n(Data1,[String|Text],St0);

	?EXT0 ->
	    String = debug_decode_content_extension(ext0, undefined, St0),
	    log2(St0,"~2.16.0B", " EXT0: ~p", [?EXT0],[String]),
	    debug_decode_content_n(Data,[String|Text],St0);

	?EXT1 ->
	    String = debug_decode_content_extension(ext1, undefined, St0),
	    log2(St0,"~2.16.0B", " EXT1: ~p", [?EXT1],[String]),
	    debug_decode_content_n(Data,[String|Text],St0);

	?EXT2 ->
	    String = debug_decode_content_extension(ext2, undefined, St0),
	    log2(St0,"~2.16.0B", " EXT2: ~p", [?EXT2],[String]),
	    debug_decode_content_n(Data,[String|Text],St0);
	_ ->
	    St1 = call_text(Text, St0),
	    {St2,Data1} = debug_decode_element(Data0,St1),
	    debug_decode_content_n(Data1,[],St2)
    end.

%%
%% *attribute END
%%

debug_decode_attribute_n([?END|Data], _Tag, St0) ->
    log2(St0,"~2.16.0B", " END", [?END],[]),
    {St0, Data};
debug_decode_attribute_n(Data, Tag, St0) ->
    case debug_decode_attribute(Data,St0) of
	{{Attr,Value},St1,Data1} ->
	    Callback = St0#wbxml_dec.callback,
	    CSt=Callback:attribute(Tag,Attr,Value,0,St1#wbxml_dec.cstate),
	    debug_decode_attribute_n(Data1,Tag,St1#wbxml_dec { cstate = CSt });
	{Attr,St1,Data1} ->
	    Callback = St0#wbxml_dec.callback,
	    CSt=Callback:attribute(Tag,Attr,0,St1#wbxml_dec.cstate),
	    debug_decode_attribute_n(Data1,Tag,St1#wbxml_dec { cstate = CSt })
    end.

%%
%% attribute = attrStart *attrValue
%%
debug_decode_attribute([?SWITCH_PAGE,Page|Data],St0) ->
    log2(St0,"~2.16.0B ~2.16.0B", " SWITCH_PAGE: attribute ~p -> ~p", 
	 [?SWITCH_PAGE,Page],[St0#wbxml_dec.attribute_page, Page]),
    debug_decode_attribute(Data,St0#wbxml_dec { attribute_page = Page });
debug_decode_attribute([?LITERAL|Data],St0) ->
    {Index,_Bytes,Data1} = debug_decode_mb_u_int32(Data),
    Attr = list_to_atom(lookup_string(Index, St0)),
    log2(St0,"~2.16.0B ~s", " LITERAL '~s'",
	 [?LITERAL, fmt_bytes(Data, Data1)], [Attr]),
    case Data1 of
	[?END|_]                  -> {Attr,St0,Data1};
	[?LITERAL|_]              -> {Attr,St0,Data1};
	[A|_] when A > 3, A < 128 -> {Attr,St0,Data1};
	_ -> 
	    {Value,St1,Data3} = debug_decode_attrValue_n(Data1,St0),
	    {{Attr,Value},St1,Data3}
    end;
debug_decode_attribute([Code|Data],St0) when Code < 128 ->
    Page = St0#wbxml_dec.attribute_page,
    {Attr,Prefix} =
	case ((St0#wbxml_dec.decf)#wbxml_decf.atrf)(Code,Page) of
	    undefined ->
		{list_to_atom("ATTR_"++integer_to_list(Page)++"_"
			      ++integer_to_list(Code)),""};
	    AttrVal -> AttrVal
	end,
    log2(St0,"~2.16.0B", " ATTRIBUTE '~s'=\"~s\"...",
	 [Code], [Attr, Prefix]),
    case Data of
	[?END|_]                  -> {{Attr,Prefix},St0,Data};
	[?LITERAL|_]              -> {{Attr,Prefix},St0,Data};
	[A|_] when A > 3, A < 128 -> {{Attr,Prefix},St0,Data};
	_ -> 
	    {Value,St1,Data1} = debug_decode_attrValue_n(Data,St0),
	    {{Attr,Prefix++Value},St1,Data1}
    end.

%%
%% attrValue = ([switchPage]ATTRVALUE) | string | extension | entity | opaque
%%

debug_decode_attrValue(Data0=[Code|Data], St0) ->
    case Code of
	?SWITCH_PAGE ->
	    [Page|Data1] = Data,
	    log2(St0,"~2.16.0B ~2.16.0B", " SWITCH: attribute ~p -> ~p", 
		 [?SWITCH_PAGE,Page],[St0#wbxml_dec.attribute_page, Page]),
	    debug_decode_attrValue(Data1, St0#wbxml_dec { attribute_page = Page });
	?ENTITY ->
	    {Int,Data1} = decode_mb_u_int32(Data),
	    log2(St0,"~2.16.0B ~s", " Entity: ~p", 
		 [?ENTITY, fmt_bytes(Data, Data1)], [Int]),
	    {[Int],St0,Data1};
	?STR_I ->
	    {String,Data1} = get_cstring(Data),
	    log2(St0,"~2.16.0B ~s", " STR_I: ~999p", 
		 [?STR_I, fmt_bytes(Data,Data1)], [String]),
	    {String,St0,Data1};
	?STR_T ->
	    {Index,_Bytes,Data1} = debug_decode_mb_u_int32(Data),
	    String = lookup_string(Index, St0),
	    log2(St0,"~2.16.0B ~s", " STR_T: ~p => ~p", 
		 [?STR_T, fmt_bytes(Data, Data1)], [Index, String]),
	    {String,St0,Data1};
	?OPAQUE when St0#wbxml_dec.version > ?WBXML_10 ->
	    {Length,_Bytes,Data1} = debug_decode_mb_u_int32(Data),
	    {Opaque,Data2} = get_n_bytes(Data1,Length),
	    log2(St0,"~2.16.0B ~s ~s", " OPAQUE: len=~w, string=~999p", 
		 [?OPAQUE, fmt_bytes(Data, Data1), fmt_bytes(Data1,Data2)], 
		 [Length, Opaque]),
	    String = debug_decode_value_extension(opaque, Opaque, St0),
	    {String,St0,Data2};
	?EXT_I0 ->
	    {Ext,Data1} = get_cstring(Data),
	    String = debug_decode_value_extension(ext0, Ext, St0),
	    log2(St0,"~2.16.0B ~s", " EXT_I0: ~p", 
		 [?EXT_I0, fmt_bytes(Data, Data1)], [String]),
	    {String, St0, Data1};
	?EXT_I1 ->
	    {Ext,Data1} = get_cstring(Data),
	    String = debug_decode_value_extension(ext1, Ext, St0),
	    log2(St0,"~2.16.0B ~s", " EXT_I2: ~p", 
		 [?EXT_I2, fmt_bytes(Data, Data1)], [String]),
	    {String, St0, Data1};
	?EXT_I2 ->
	    {Ext,Data1} = get_cstring(Data),
	    String = debug_decode_value_extension(ext2, Ext, St0),
	    log2(St0,"~2.16.0B ~s", " EXT_I2: ~p", 
		 [?EXT_I2, fmt_bytes(Data, Data1)], [String]),
	    {String, St0, Data1};
	?EXT_T0 ->
	    {Int,Bytes,Data1} = debug_decode_mb_u_int32(Data),
	    Ext = lookup_string(Int, St0),
	    String = debug_decode_value_extension(ext0, Ext, St0),
	    log2(St0,"~2.16.0B ~s", " EXT_T0: ref=~w ~p", 
		 [?EXT_T0, fmt_byte_list(Bytes)], [Int,String]),
	    {String, St0, Data1};
	?EXT_T1 ->
	    {Int,Bytes,Data1} = debug_decode_mb_u_int32(Data),
	    Ext = lookup_string(Int, St0),
	    String = debug_decode_value_extension(ext1, Ext, St0),
	    log2(St0,"~2.16.0B ~s", " EXT_T1: ref=~w ~p", 
		 [?EXT_T0, fmt_byte_list(Bytes)], [Int,String]),
	    {String, St0, Data1};
	?EXT_T2 ->
	    {Int,Bytes,Data1} = debug_decode_mb_u_int32(Data),
	    Ext = lookup_string(Int, St0),
	    String = debug_decode_value_extension(ext2, Ext, St0),
	    log2(St0,"~2.16.0B ~s", " EXT_T2: ref=~w ~p", 
		 [?EXT_T0, fmt_byte_list(Bytes)], [Int,String]),
	    {String, St0, Data1};
	?EXT0 ->
	    String = debug_decode_value_extension(ext0, undefined, St0),
	    log2(St0,"~2.16.0B", " EXT0: ~p", [?EXT0],[String]),
	    {String, St0, Data};
	?EXT1 ->
	    String = debug_decode_value_extension(ext1, undefined, St0),
	    log2(St0,"~2.16.0B", " EXT1: ~p", [?EXT1],[String]),
	    {String, St0, Data};
	?EXT2 ->
	    String = debug_decode_value_extension(ext2, undefined, St0),
	    log2(St0,"~2.16.0B", " EXT2: ~p", [?EXT2],[String]),
	    {String, St0, Data};
	V when V >= 128 ->
	    Value = case ((St0#wbxml_dec.decf)#wbxml_decf.valf)
			(V,St0#wbxml_enc.attribute_page) of
			undefined ->
			    "VALUE"++integer_to_list(V);
			Val -> Val
		    end,
	    {Value,St0,Data};
	_ ->
	    {false,St0,Data0}
    end.


debug_decode_attrValue_n(Data, St0) ->
    debug_decode_attrValue_n(Data, [], St0).

debug_decode_attrValue_n(Data=[_Code|_], Acc, St0) ->
    case debug_decode_attrValue(Data, St0) of
	{false, St1, Data1} ->
	    {Acc, St1, Data1};
	{Val, St1, Data1} ->
	    debug_decode_attrValue_n(Data1, Acc++Val, St1)
    end;
debug_decode_attrValue_n([], Acc, St0) ->
    {Acc, St0, []}.

%%
%% EXTENSION(s)
%%
debug_decode_content_extension(Ext,ExtVal,St) ->
    case ((St#wbxml_dec.decf)#wbxml_decf.extf)
	(content,Ext, St#wbxml_dec.tag_page, ExtVal) of
	undefined ->
	    "";
	String -> 
	    String
    end.

debug_decode_value_extension(Ext, ExtVal, St) ->
    case ((St#wbxml_dec.decf)#wbxml_decf.extf)
	(value,Ext,St#wbxml_dec.attribute_page, ExtVal) of
	undefined ->
	    "";
	String -> 
	    String
    end.

%%
%% strtbl = length *byte
%%

%% decode multibyte integer.
debug_decode_mb_u_int32([X|Data]) ->
    if X > 16#7f ->
	    debug_decode_mb_u_int32(Data,[X],X band 16#7f);
       true ->
	    {X,[X],Data}
    end.

debug_decode_mb_u_int32([X|Data],Acc,X0) ->
    if X > 16#7f ->
	    debug_decode_mb_u_int32(Data,[X0|Acc],(X0 bsl 7) + (X band 16#7f));
       true ->
	    {(X0 bsl 7)+X,reverse([X|Acc]),Data}
    end.

%% ONE COLUMN OUTPUT
log(St, Fmt, Args) ->
    io:format(St#wbxml_dec.logfd, Fmt++"\n", Args).

%% TWO COLUMN OUTPUT 39 | 39 \n
%% 01 02 03 04 ..    | ..... \n
log2(St, Fmt1, Fmt2, Args1, Args2) ->
    C1 = lists:flatten(io_lib:format(Fmt1, Args1)),
    C2 = lists:flatten(io_lib:format(Fmt2, Args2)),
    log2(St#wbxml_dec.logfd, C1, C2, 39, [], []).

log2(Fd, Cs1, Cs2, 0, Acc1, Acc2) ->
    %% Full flush
    io:format("~s|~s\n", [reverse(Acc1),reverse(Acc2)]),
    log2(Fd, Cs1, Cs2, 39, [], []);
log2(Fd, [C1|Cs1], [C2|Cs2], I, Acc1, Acc2) ->
    log2(Fd, Cs1, Cs2, I-1, [C1|Acc1], [C2|Acc2]);
log2(Fd, [C1|Cs1], [], I, Acc1, Acc2) ->
    log2(Fd, Cs1, [], I-1, [C1|Acc1], [$\s|Acc2]);
log2(Fd, [], [C2|Cs2], I, Acc1, Acc2) ->
    log2(Fd, [], Cs2, I-1, [$\s|Acc1], [C2|Acc2]);
log2(_Fd, [], [], I, Acc1, Acc2) ->
    Pad = lists:duplicate(I, $\s),
    io:format("~s|~s\n", [reverse(Acc1)++Pad,reverse(Acc2)++Pad]).

%% Format a plain bytes list    
fmt_byte_list([H]) ->
    fmt_byte(H);
fmt_byte_list([H|T]) ->
    fmt_byte(H, [$\s|fmt_byte_list(T)]);
fmt_byte_list([]) ->
    [].

%% fmt_bytes(Data, DataTail) 
%% format all bytes until Data == DataTail
fmt_bytes(Data, DataTail) ->
    fmt_bytes1(Data,DataTail).

fmt_bytes1(X, X) ->
    [];
fmt_bytes1([H|X], X) ->
    fmt_byte(H);
fmt_bytes1([H|Data], X) ->
    fmt_byte(H, [$\s|fmt_bytes1(Data, X)]);
fmt_bytes1([], _X) ->
    [].

%% Format one hex byte as two hex digits
fmt_byte(B) ->
    fmt_byte(B, []).

fmt_byte(B, Acc) ->
    case erlang:integer_to_list(B, 16) of
	[H] -> [$0,H | Acc];
	[H1,H2] -> [H1,H2|Acc]
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% WBXML Encoder
%%
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

encode_opts(Opts) ->
    encode_opts(Opts,#wbxml_opt{}).

encode_opts([], R) ->
    R;
encode_opts([H|T], R) ->
    case H of
	empty_value_string ->
	    encode_opts(T, R#wbxml_opt{empty_value_string=true});
	nokia_codepage_bug ->
	    %% special handling for nokia codepage bugs
	    Map = [{16#05,0},  %% name=..., 
		   {16#06,0},  %% value=...,
		   {16#07,0},  %% name=NAME...
		   {16#22,0},  %% name=TO-NAPID...
		   {16#23,0},  %% name=PORTNBR...
		   {16#53,0}   %% type=PORT...
		  ],
	    encode_opts(T, R#wbxml_opt{codepage_map=Map});
	{pubid,PubId} ->
	    encode_opts(T, R#wbxml_opt{pubid=PubId});
	{version,Version} ->
	    encode_opts(T, R#wbxml_opt{version=Version});
	{charset,Charset} ->
	    encode_opts(T, R#wbxml_opt{charset=Charset});
	{env,Env} ->
	    encode_opts(T, R#wbxml_opt{env=Env});
	{mod,Mod} ->
	    encode_opts(T, R#wbxml_opt{mod=Mod});
	{partial,Mode} when Mode == false; Mode == true;
			    Mode == open; Mode==close ->
	    encode_opts(T, R#wbxml_opt{partial=Mode});
	{literal, L} ->
	    %% Pre declare literals needed for streaming
	    Ls = R#wbxml_opt.literals,
	    encode_opts(T, R#wbxml_opt{literals = [L|Ls]});
	    
	_Other ->
	    io:format("WARNING: unsupported wbxml option = ~p\n", [H]),
	    encode_opts(T, R)
    end.


%%
%% Encode XML into WBXML
%% return:  {VarList, | VarBinaryList}
%%
encode(XML) ->
    encode(XML, []).

encode(XML, Opts) ->
    encode1(XML, encode_opts(Opts)).

encode1(XML, R) ->
    Mod = R#wbxml_opt.mod,
    PubCode0 = pubid_to_code(R#wbxml_opt.pubid),
    PubCode = 
	if PubCode0 == ?WBXML_unknown ->
		%% See if we can deduce the PubCode from Mod!
		case module_to_pubid(Mod) of
		    undefined -> PubCode0;
		    {_,ID} -> pubid_to_code(ID)
		end;
	   true ->
		PubCode0
	end,
    Mod1 = pubcode_to_module(PubCode),
    Mod2 = if Mod == undefined -> Mod1;
	      true -> Mod
	   end,
    VerCode     = version_to_code(R#wbxml_opt.version),
    CharSetCode = makeup_charset:from_mime_name(R#wbxml_opt.charset),
    EncodeIf = encode_interface(Mod2),
    Partial = R#wbxml_opt.partial,
    St0 = #wbxml_enc { mod = Mod2, 
		       encf = EncodeIf,
		       version = VerCode, 
		       strtbl  = new_strtbl(R#wbxml_opt.literals),
		       env = R#wbxml_opt.env,
		       pagemap = R#wbxml_opt.codepage_map,
		       empty   = R#wbxml_opt.empty_value_string,
		       partial = Partial
		      },
    {Body, St1} = encode_document(XML, St0),
    case Partial of
	false ->
	    list_to_binary([VerCode, 
			    encode_pubcode(PubCode),
			    encode_charset(CharSetCode),
			    encode_strtbl(St1#wbxml_enc.strtbl),
			    Body]);
	close ->
	    list_to_binary(Body);
	true ->
	    {list_to_binary(Body), St1};
	open ->
	    {list_to_binary([VerCode, 
			     encode_pubcode(PubCode),
			     encode_charset(CharSetCode),
			     encode_strtbl(St1#wbxml_enc.strtbl),
			     Body]), St1}
    end.

%% Open generating WBXML
encode_begin(XML) ->
    encode(XML, [{partial,open}]).

encode_begin(XML, Opts) ->
    encode(XML, [{partial,open}|Opts]).

encode_open(XML, St)  when is_record(St, wbxml_enc) ->
    {Body, St1} = encode_document(XML, St#wbxml_enc { partial = open }),
    {list_to_binary(Body), St1}.

encode_close(XML, St)  when is_record(St, wbxml_enc) ->
    case St#wbxml_enc.tagdata of
	{_Tag,[]} ->
	    {Body, St1} = encode_document(XML, 
					  St#wbxml_enc { partial = close }),
	    {list_to_binary(Body),St1};
	{_Tag,TData} ->
	    {Body, St1} = encode_document(XML, 
					  St#wbxml_enc { partial = close }),
	    {list_to_binary([TData,Body]),St1}
    end.

%% Continue generating WBXML
encode_continue(XML, St)  when is_record(St, wbxml_enc) ->
    case St#wbxml_enc.tagdata of
	{Tag,[]} ->
	    {Body, St1} = encode_content_n(XML,Tag,
					   St#wbxml_enc { partial = true }),
	    {list_to_binary(Body), St1};
	{Tag,[Tg|TData]} when is_integer(Tg) ->
	    {Body, St1} = encode_content_n(XML,Tag,
					   St#wbxml_enc { partial = true }),
	    {list_to_binary([[Tg bor 16#40 | TData], Body]),
	     St1#wbxml_enc { tagdata = {Tag,[]}}}
    end.

%% Close partial mode
encode_end(XML, St) when is_record(St, wbxml_enc) ->
    {Data,_St1} = encode_close(XML, St),
    Data.


%%
%% publicid = mb_u_int32 | (zero index)
%%

encode_pubcode({tref,Code}) ->
    [0 | encode_mb_u_int32(Code)];
encode_pubcode(Code) ->
    encode_mb_u_int32(Code).
    
encode_charset(CharSet) ->
    encode_mb_u_int32(CharSet).

encode_strtbl({Length,Tab}) ->
    ?dbg("string table(~w) = ~p\n",[Length,Tab]),
    [encode_mb_u_int32(Length),
     lists:map(fun({_,_,Str}) -> [Str,0] end, reverse(Tab))].

encode_document(E, St) when tuple(E) ->
    encode_body([E], St);
encode_document(Es, St) when list(Es) ->
    encode_body(Es, St).

%% 
%% body = *pi element *pi
%%
encode_body([{'#DECL',_} |Cs], St0) ->
    encode_body(Cs, St0);
encode_body(Cs, St0) ->
    case St0#wbxml_enc.partial of
	open ->
	    {PI1, St1, Cs1}   = encode_pi_n(Cs, St0),
	    {Element,St2, _Cs2} = encode_element(Cs1, St1),
	    {?CONS(PI1,Element), St2};
	close ->
	    {_PI1, St1, Cs1}   = encode_pi_n(Cs, St0),
	    {Element,St2,Cs2}  = encode_element(Cs1, St1),
	    {PI2, St3, _Cs3}   = encode_pi_n(Cs2, St2),
	    {?CONS(Element,PI2), St2};
	true ->
	    {Element,St2, _Cs2}  = encode_element(Cs, St0),
	    {Element,St2};
	false ->
	    {PI1, St1, Cs1}   = encode_pi_n(Cs, St0),
	    {Element,St2,Cs2} = encode_element(Cs1, St1),
	    {PI2, St3, _Cs3}   = encode_pi_n(Cs2, St2),
	    {?CONS(PI1,?CONS(Element,PI2)), St3}
    end.

%%
%% FIXME: implement {'#PI', {Name, As}}
%% Need to fix Value => stringify attribute list
%%
encode_pi_n([{'#PI',{Name,Value}} | Cs], St0) ->
    {Data, St1} = encode_attribute(Name, Value, St0),
    {Data2, St2, Cs1} = encode_pi_n(Cs, St1),
    {?CONS([?PI, Data, ?END], Data2), St2, Cs1};
encode_pi_n(Cs, St0) ->
    {[], St0, Cs}.

%% 
%% element = ([switchPage] stag) [ 1*attribute END ] [ *content END]
%%
encode_element([{Name,As,Cs}|Es], St0) ->
    if St0#wbxml_enc.partial == close ->
	    {Data,St1} = encode_content_n(Cs, Name, St0),
	    {Data,St1,Es};
       true ->
	    AP = (As =/= []),
	    CP = (Cs =/= []),
	    {TagData,St1} = encode_stag(Name, St0, AP, CP),
	    {AsData,St2} = case AP of
			       true  -> encode_attribute_n(As, St1);
			       false -> {[], St1}
			   end,
	    {CsData,St3} = case CP of
			       true -> encode_content_n(Cs, Name, St2);
			       false -> {[],St2}
			   end,
	    {?CONS(TagData,?CONS(AsData,CsData)), St3, Es}
    end;
encode_element([], St0) ->
    {[], St0, []}.


%%
%% Encode of tags
%%
encode_stag(Name, St, AP, CP) ->
    case ((St#wbxml_enc.encf)#wbxml_encf.tagf)(Name,St#wbxml_enc.tag_page) of
	Tg when is_integer(Tg) ->
	    T = encode_tg(AP, CP, Tg),
	    {T, St};
	{Tg,Page} when is_integer(Tg) ->
	    T = encode_tg(AP, CP, Tg),
	    ?dbg("encode_stag: ~s page=~w, code=~p\n",
		 [Name, Page,T]),
	    {[?SWITCH_PAGE,Page,T], St#wbxml_enc { tag_page = Page}};
	undefined ->
	    ?dbg("~p: encode tag ~s\n", [St#wbxml_enc.mod, Name]),
	    encode_ltag(Name, St, AP, CP)
    end.

%% Encode Tag type (ie. add Ap and Cp)
%% encode_tg(AP, CP, Tg)
%%
encode_tg(true, true, Tg)  -> 16#C0 bor (Tg band 16#3f);
encode_tg(true, false, Tg) -> 16#80 bor (Tg band 16#3f);
encode_tg(false, true, Tg) -> 16#40 bor (Tg band 16#3f);
encode_tg(false, false, Tg) -> (Tg band 16#3f).


encode_ltag(Name, St, AP, CP) ->
    T = (if AP == true -> 16#80;
	    AP == false -> 16#00
	 end) bor
	(if CP == true -> 16#40;
	    CP == false -> 16#00
	 end) bor ?LITERAL,
    {Lit,St1} = insert_string(atom_to_list(Name), St),
    { [T | encode_mb_u_int32(Lit)], St1}.
    

%%
%% *content END
%%

encode_content_n(Es=[{_,_,_}|T],Tag,St0) ->
    Partial = St0#wbxml_enc.partial,
    if Partial == open ->
	    if T == [] ->
		    {Data1, St1, _Es1} = encode_element(Es, St0),
		    {tl(Data1), St1#wbxml_enc { tagdata={Tag,hd(Data1)} }};
	       true ->
		    {Data1, St1, Es1} = encode_element(Es, St0),
		    {Data2, St2} = encode_content_n(Es1,Tag,St1),
		    {?CONS(Data1,Data2), St2}
	    end;
       true ->
	    {Data1, St1, Es1} = encode_element(Es, St0),
	    {Data2, St2} = encode_content_n(Es1,Tag,St1),
	    {?CONS(Data1,Data2), St2}
    end;
encode_content_n([{'#PCDATA',Text1},{'#PCDATA',Text2}|Es],Tag,St0) ->
    %% i.e merge multi text
    encode_content_n([{'#PCDATA',Text1++Text2} | Es],Tag,St0);

encode_content_n([{'#PCDATA',Text0}|Es],Tag,St0) when binary(Text0) ->
    Partial = St0#wbxml_enc.partial,
    if Partial == close ->
	    encode_content_n(Es,Tag,St0);
       true ->
	    {Data1, St1} = enc_opaque(Text0, St0),
	    {Data2, St2} = encode_content_n(Es,Tag,St1),
	    {?CONS(Data1,Data2), St2}
    end;
encode_content_n([{'#PCDATA',Text0}|Es],Tag,St0) ->
    Partial = St0#wbxml_enc.partial,
    if Partial == close ->
	    encode_content_n(Es,Tag,St0);
       true ->
	    case St0#wbxml_enc.space of
		preserve ->
		    {Data1, St1} = encode_text(Text0,Tag,St0),
		    {Data2, St2} = encode_content_n(Es,Tag,St1),	    
		    {?CONS(Data1,Data2), St2};
		normalize ->
		    {Data1,St1} = 
			case reverse(strip(reverse(strip(Text0)))) of
			    [] -> encode_text(" ",Tag,St0);
			    Text1 -> encode_text(Text1,Tag,St0)
			end,
		    {Data2, St2} = encode_content_n(Es,Tag,St1),
		    {?CONS(Data1,Data2), St2};
		remove ->
		    {Data1,St1} = 
			case reverse(strip(reverse(strip(Text0)))) of
			    [] ->  {[],St0};
			    Text1 -> encode_text(Text1,Tag,St0)
			end,
		    {Data2, St2} = encode_content_n(Es,Tag,St1),
		    {?CONS(Data1,Data2), St2}
	    end
    end;
encode_content_n(Elem, _Tag,St0) when tuple(Elem) ->
    {Data1, St1, _Es1} = encode_element([Elem], St0),
    {Data1, St1};
encode_content_n([],_Tag,St0) ->
    case St0#wbxml_enc.partial of
	open  -> {[],St0};
	_ ->     {[?END], St0}
    end;
encode_content_n(_Es,_Tag,St0) ->
    {[?END], St0}.

encode_text(Text,Tag,St) ->
    Page = St#wbxml_enc.tag_page,
    Codes = case ((St#wbxml_enc.encf)#wbxml_encf.extf)(content,Tag,Page,Text) of
		undefined ->
		    [{string,Text}];
		Codes0 -> Codes0
	    end,
    ?dbg("text codes=~p\n", [Codes]),
    {Data1,Page1,St1} = enc_value_codes(Codes, [], Page,St),
    {Data1, St1#wbxml_enc { tag_page = Page1}}.

%%
%% *attribute END
%%

encode_attribute_n([{Name,Value} | As], St) ->
    {Data, St1} = encode_attribute(Name, Value, St),
    {Data1, St2} = encode_attribute_n(As, St1),
    {?CONS(Data,Data1), St2};
encode_attribute_n([], St) ->
    {[?END], St}.
    
%%
%% attribute = attrStart *attrValue
%%
encode_attribute(Name, Value, St) ->
    Page0 = St#wbxml_enc.attribute_page,
    Map = St#wbxml_enc.pagemap,
    case ((St#wbxml_enc.encf)#wbxml_encf.atrf)(Name,Value,Page0,Map) of
	undefined ->
	    encode_lattribute(Name, Value, St);
	{Code,Page0,Value1} -> %% same code page
	    {ValData,St1} = encode_value(Name, Value1, St),
	    {[Code|ValData], St1};
	{Code,Page,Value1} -> %% other code page
	    ?dbg("encode_attribute: page=~w, ~s=~s\n",
		 [Page, Name, Value]),
	    {ValData,St1} =
		encode_value(Name, Value1,St#wbxml_enc { attribute_page=Page}),
	    {[?SWITCH_PAGE,Page,Code|ValData],St1}
    end.

encode_lattribute(Name, Value, St) ->
    {Literal,St1} = insert_string(atom_to_list(Name), St),
    {ValData,St2} = encode_value(Name, Value, St1),
    { [?LITERAL,encode_mb_u_int32(Literal),ValData], St2}.

encode_value(Name, Val, St) ->
    Page = St#wbxml_enc.attribute_page,
    EncodeEmpty = St#wbxml_enc.empty,
    case ((St#wbxml_enc.encf)#wbxml_encf.valf)(Name,Val,Page) of
	undefined ->
	    %% FIXME: entity coding
	    {Data,Page1,St1} = enc_value_codes([{string,Val}],[],Page,St),
	    {Data,St1#wbxml_enc { attribute_page = Page1}};
	[] when EncodeEmpty == true ->
	    {[?STR_I,0],St};
	[{string,""}] when EncodeEmpty == true ->
	    {[?STR_I,0],St};
	Codes ->
	    ?dbg("encode_value: code=~p\n", [Codes]),
	    {Data,Page1,St1} = enc_value_codes(Codes, [], Page,St),
	    {Data,St1#wbxml_enc { attribute_page = Page1}}
    end.

enc_value_codes([Code | Codes], Acc, Page, St) when is_integer(Code) ->
    enc_value_codes(Codes, [Code|Acc], Page, St);
enc_value_codes([{string,Value}|Codes], Acc, Page, St) ->
    if Value == "" -> %% no need to code empty list!
	    enc_value_codes(Codes,Acc,Page, St);
       Value == <<>> ->
	    enc_value_codes(Codes,Acc,Page, St);
       true ->
	    {StrVal,St1} = enc_str(Value, St),
	    enc_value_codes(Codes,[StrVal|Acc],Page,St1)
    end;
enc_value_codes([{page,NewPage}|Codes], Acc, Page, St) ->
    if NewPage == Page ->
	    enc_value_codes(Codes, Acc, Page, St);
       true ->
	    enc_value_codes(Codes, [NewPage,?SWITCH_PAGE|Acc], NewPage, St)
    end;
enc_value_codes([{opaque, Data} | Codes], Acc, Page, St)
  when St#wbxml_enc.version > ?WBXML_10 ->
    {Opaque,St1} = enc_opaque(Data,St),
    enc_value_codes(Codes, [Opaque | Acc], Page, St1);
enc_value_codes([{entity,Ent} | Codes],Acc, Page, St) ->
    enc_value_codes(Codes,[[?ENTITY,encode_mb_u_int32(Ent)]|Acc],Page,St);
enc_value_codes([{extension,Ext} | Codes], Acc, Page, St) ->
    Data = case Ext of
	       ext0 -> [?EXT0];
	       ext1 -> [?EXT1];
	       ext2 -> [?EXT2]
	   end,
    enc_value_codes(Codes, [Data | Acc], Page, St);
enc_value_codes([{extension,Ext,ExtVal} | Codes],Acc,Page,St) ->
    %% FIXME: code EXT_Ti (when needed)
    Data = case Ext of
	       ext0 -> [?EXT_I0,ExtVal,0];
	       ext1 -> [?EXT_I1,ExtVal,0];
	       ext2 -> [?EXT_I2,ExtVal,0]
	   end,
    enc_value_codes(Codes, [Data|Acc],Page,St);
enc_value_codes([],Acc,Page,St) ->
    {lists:flatten(reverse(Acc)),Page, St}.

enc_opaque(Data, St) ->
    ?dbg("encode_value: opaque=~p\n", [Data]),
    Len = if binary(Data) -> size(Data);
	     list(Data) -> length(Data)
	  end,
    {[?OPAQUE,encode_mb_u_int32(Len),Data], St}.


enc_str(Value, St) ->
    {enc_str(Value, [], St#wbxml_enc.env), St}.

enc_str([$$,${ | Cs], Acc, Env) ->
    enc_str1(Cs, Acc, Env);
enc_str([C | Cs], Acc, Env) ->
    enc_str(Cs, [C|Acc], Env);
enc_str([], Acc, _Env) ->
    [?STR_I,reverse(Acc),0].

enc_str1([$\s|Cs],Acc,Env) -> enc_str1(Cs,Acc,Env);
enc_str1([$\t|Cs],Acc,Env) -> enc_str1(Cs,Acc,Env);
enc_str1([$}|Cs],Acc,Env) -> enc_str(Cs,Acc,Env);
enc_str1([C|Cs],Acc,Env) -> enc_str2(Cs,[C],Acc,Env);
enc_str1([],Acc,Env) -> enc_str([],Acc,Env).

%% allow leading space i.e ${ Var}
enc_str2([$\s|Cs],Var,Acc,Env) -> enc_str3(Cs,Var,Acc,Env);
enc_str2([$\t|Cs],Var,Acc,Env) -> enc_str3(Cs,Var,Acc,Env);
enc_str2([$}|Cs],Var,Acc,Env) -> enc_str_var(Cs,Var,Acc,Env);
enc_str2([C|Cs],Var,Acc,Env) -> enc_str2(Cs,[C|Var],Acc,Env);
enc_str2([],_Var,Acc,Env) -> enc_str([],Acc,Env).
%% allow trailing space i.e ${Var }
enc_str3([$\s|Cs],Var,Acc,Env) -> enc_str3(Cs,Var,Acc,Env);
enc_str3([$\t|Cs],Var,Acc,Env) -> enc_str3(Cs,Var,Acc,Env);
enc_str3([$}|Cs],Var,Acc,Env) -> enc_str_var(Cs,Var,Acc,Env);
enc_str3(Cs,_Var,Acc,Env) -> enc_str(Cs,Acc,Env).

enc_str_var(Cs, Var, Acc, Env) ->
    case lists:keysearch(reverse(Var), 1, Env) of
	false -> enc_str(Cs, Acc, Env);
	{value,{_,Value}}  -> enc_str(Cs, [Value|Acc], Env)
    end.

%% encode mulitbyte integer.
encode_mb_u_int32(X) ->
    if X < 128 -> [X];
       X > 0   -> encode_mb_u_int32(X bsr 7,[X band 16#7f])
    end.

encode_mb_u_int32(X, Acc) ->
    if X < 128 -> [(X bor 16#80)|Acc];
       true -> encode_mb_u_int32(X bsr 7, [((X band 16#7f) bor 16#80)|Acc])
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% WBXML Size
%%
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%
%% Calculate size of WBXML given the XML tag structure
%% return:  Size
%%
encode_size(XML) ->
    encode_size(XML,[]).

encode_size(XML, Opts) ->
    encode1_size(XML, encode_opts(Opts)).

encode1_size(XML, R) ->
    Mod = R#wbxml_opt.mod,
    PubCode0 = pubid_to_code(R#wbxml_opt.pubid),
    PubCode = 
	if PubCode0 == ?WBXML_unknown ->
		%% See if we can deduce the PubCode from Mod!
		case module_to_pubid(Mod) of
		    undefined -> PubCode0;
		    {_,ID} -> pubid_to_code(ID)
		end;
	   true ->
		PubCode0
	end,
    Mod1 = pubcode_to_module(PubCode),
    Mod2 = if Mod == undefined -> Mod1;
	      true -> Mod
	   end,
    VerCode     = version_to_code(R#wbxml_opt.version),
    CharSetCode = makeup_charset:from_mime_name(R#wbxml_opt.charset),
    EncodeIf = encode_interface(Mod2),
    St0 = #wbxml_enc { mod = Mod2, 
		       encf = EncodeIf,
		       version = VerCode, 
		       env = R#wbxml_opt.env,
		       pagemap = R#wbxml_opt.codepage_map,
		       empty   = R#wbxml_opt.empty_value_string
		      },
    {BodySize, St1} = size_document(XML, St0),
    1 + size_pubcode(PubCode) +
	size_charset(CharSetCode) +
	size_strtbl(St1#wbxml_enc.strtbl) + 
	BodySize.


size_pubcode({tref,Code}) ->
    1+size_mb_u_int32(Code);
size_pubcode(Code) ->
    size_mb_u_int32(Code).

size_charset(CharSet) ->
    size_mb_u_int32(CharSet).

size_strtbl({Length,_Tab}) ->
    size_mb_u_int32(Length) + Length.

size_document(E, St) when tuple(E) ->
    size_body([E], St);
size_document(Es, St) when list(Es) ->
    size_body(Es, St).

%% 
%% body = *pi element *pi
%%
size_body([{'#DECL',_} |Cs], St0) ->
    size_body(Cs, St0);
size_body(Cs, St0) ->
    {Size1, St1, Cs1}  = size_pi_n(Cs, St0),
    {Size2,St2,Cs2}    = size_element(Cs1, St1),
    {Size3, St3, _Cs3} = size_pi_n(Cs2, St2),
    {Size1+Size2+Size3, St3}.

size_pi_n([{'#PI',{Name,Value}} | Cs], St0) ->
    {Size1, St1} = size_attribute(Name, Value, St0),
    {Size2, St2, Cs1} = size_pi_n(Cs, St1),
    {1+Size1+1+Size2, St2, Cs1};
size_pi_n(Cs, St0) ->
    {0, St0, Cs}.

%% 
%% element = ([switchPage] stag) [ 1*attribute END ] [ *content END]
%%
size_element([{Name,As,Cs}|Es], St0) ->
    AP = (As =/= []),
    CP = (Cs =/= []),
    {Size1,St1} = size_stag(Name, St0, AP, CP),
    {Size2,St2} = case AP of
		      true  -> size_attribute_n(As, St1);
		      false -> {0, St1}
		  end,
    {Size3,St3} = case CP of
		      true -> size_content_n(Cs, Name, St2);
		      false -> {0,St2}
		  end,
    {Size1+Size2+Size3, St3, Es};
size_element([], St0) ->
    {0, St0, []}.


%%
%% Size of tags
%%
size_stag(Name, St, AP, CP) ->
    case ((St#wbxml_enc.encf)#wbxml_encf.tagf)(Name,St#wbxml_enc.tag_page) of
	Tg when is_integer(Tg) ->
	    {1, St};	    
	{Tg,Page} when is_integer(Tg) ->
	    %% count 1 for tag and 2 for page switch
	    {3, St#wbxml_enc { tag_page = Page}};
	undefined ->
	    size_ltag(Name, St, AP, CP)
    end.

size_ltag(Name, St, _AP, _CP) ->
    {Lit,St1} = insert_string(atom_to_list(Name), St),
    { 1+size_mb_u_int32(Lit), St1}.

%%
%% *content END
%%

size_content_n(Es=[{_,_,_}|_],Tag,St0) ->
    {Size1, St1, Es1} = size_element(Es, St0),
    {Size2, St2} = size_content_n(Es1,Tag,St1),
    {Size1+Size2, St2};
size_content_n([{'#PCDATA',Text1},{'#PCDATA',Text2}|Es],Tag,St0) ->
    %% i.e merge multi text
    size_content_n([{'#PCDATA',Text1++Text2} | Es],Tag,St0);
size_content_n([{'#PCDATA',Text0}|Es],Tag,St0) when binary(Text0) ->
    {Size1, St1} = size_opaque(Text0, St0),
    {Size2, St2} = size_content_n(Es,Tag,St1),
    {Size1+Size2, St2};
size_content_n([{'#PCDATA',Text0}|Es],Tag,St0) ->
    case St0#wbxml_enc.space of
	preserve ->
	    {Size1, St1} = size_text(Text0,Tag,St0),
	    {Size2, St2} = size_content_n(Es,Tag,St1),	    
	    {Size1+Size2, St2};
	normalize ->
	    {Size1,St1} = 
		case reverse(strip(reverse(strip(Text0)))) of
		    [] -> size_text(" ",Tag,St0);
		    Text1 -> size_text(Text1,Tag,St0)
		end,
	    {Size2, St2} = size_content_n(Es,Tag,St1),
	    {Size1+Size2, St2};
	remove ->
	    {Size1,St1} = 
		case reverse(strip(reverse(strip(Text0)))) of
		    [] ->  {0,St0};
		    Text1 -> size_text(Text1,Tag,St0)
		end,
	    {Size2, St2} = size_content_n(Es,Tag,St1),
	    {Size1+Size2, St2}
    end;
size_content_n(_Es,_Tag,St0) ->
    {1, St0}.

size_text(Text,Tag,St) ->
    Page = St#wbxml_enc.tag_page,
    Codes = case ((St#wbxml_enc.encf)#wbxml_encf.extf)(content,Tag,Page,Text) of
		undefined ->
		    [{string,Text}];
		Codes0 -> Codes0
	    end,
    {Size1,Page1,St1} = size_value_codes(Codes, 0, Page,St),
    {Size1,St1#wbxml_enc { tag_page = Page1}}.

%%
%% *attribute END
%%

size_attribute_n([{Name,Value} | As], St) ->
    {Size1, St1} = size_attribute(Name, Value, St),
    {Size2, St2} = size_attribute_n(As, St1),
    {Size1+Size2, St2};
size_attribute_n([], St) ->
    {1, St}.
    
%%
%% attribute = attrStart *attrValue
%%
size_attribute(Name, Value, St) ->
    Page0 = St#wbxml_enc.attribute_page,
    Map = St#wbxml_enc.pagemap,
    case ((St#wbxml_enc.encf)#wbxml_encf.atrf)(Name,Value,Page0,Map) of
	undefined ->
	    size_lattribute(Name, Value, St);
	{_Code,Page0,Value1} -> %% same code page
	    {Size1,St1} = size_value(Name, Value1, St),
	    {1+Size1, St1};
	{_Code,Page,Value1} -> %% other code page
	    {Size1,St1} =
		size_value(Name, Value1,St#wbxml_enc { attribute_page=Page}),
	    {3+Size1,St1}
    end.

size_lattribute(Name, Value, St) ->
    {Literal,St1} = insert_string(atom_to_list(Name), St),
    {Size1,St2} = size_value(Name, Value, St1),
    { 1+size_mb_u_int32(Literal)+Size1, St2}.

size_value(Name, Val, St) ->
    Page = St#wbxml_enc.attribute_page,
    EncodeEmpty = St#wbxml_enc.empty,
    case ((St#wbxml_enc.encf)#wbxml_encf.valf)(Name,Val,Page) of
	undefined ->
	    %% FIXME: entity coding
	    {Size1,Page1,St1} = size_value_codes([{string,Val}],0,Page,St),
	    {Size1,St1#wbxml_enc { attribute_page = Page1}};
	[] when EncodeEmpty == true ->
	    {2,St};
	[{string,""}] when EncodeEmpty == true ->
	    {2,St};
	Codes ->
	    {Size,Page1,St1} = size_value_codes(Codes, 0, Page,St),
	    {Size,St1#wbxml_enc { attribute_page = Page1}}
    end.

size_value_codes([Code | Codes], Acc, Page, St) when is_integer(Code) ->
    size_value_codes(Codes, 1+Acc, Page, St);
size_value_codes([{string,Value}|Codes], Acc, Page, St) ->
    if Value == "" -> %% no need to code empty list!
	    size_value_codes(Codes,Acc,Page, St);
       Value == <<>> ->
	    size_value_codes(Codes,Acc,Page, St);
       true ->
	    {Size,St1} = size_str(Value, St),
	    size_value_codes(Codes,Size+Acc,Page,St1)
    end;
size_value_codes([{page,NewPage}|Codes], Acc, Page, St) ->
    if NewPage == Page ->
	    size_value_codes(Codes, Acc, Page, St);
       true ->
	    size_value_codes(Codes, 2+Acc, NewPage, St)
    end;
size_value_codes([{opaque, Data} | Codes], Acc, Page, St)
  when St#wbxml_enc.version > ?WBXML_10 ->
    {Size,St1} = size_opaque(Data,St),
    size_value_codes(Codes, Size+Acc, Page, St1);
size_value_codes([{entity,Ent} | Codes],Acc, Page, St) ->
    size_value_codes(Codes,1+size_mb_u_int32(Ent)+Acc,Page,St);
size_value_codes([{extension,_Ext} | Codes], Acc, Page, St) ->
    enc_value_codes(Codes, 1+Acc, Page, St);
size_value_codes([{extension,_Ext,ExtVal} | Codes],Acc,Page,St) ->
    Size = length(ExtVal)+2,
    enc_value_codes(Codes, Size+Acc,Page,St);
size_value_codes([],Acc,Page,St) ->
    {Acc,Page, St}.

size_opaque(Data, St) ->
    Len = if binary(Data) -> size(Data);
	     list(Data) -> length(Data)
	  end,
    {1+size_mb_u_int32(Len)+Len, St}.

size_str(Value, St) ->
    {size_str(Value, 0, St#wbxml_enc.env), St}.

size_str([$$,${ | Cs], Acc, Env) ->
    size_str1(Cs, Acc, Env);
size_str([_C | Cs], Acc, Env) ->
    size_str(Cs, 1+Acc, Env);
size_str([], Acc, _Env) ->
    2+Acc.

size_str1([$\s|Cs],Acc,Env) -> size_str1(Cs,Acc,Env);
size_str1([$\t|Cs],Acc,Env) -> size_str1(Cs,Acc,Env);
size_str1([$}|Cs],Acc,Env) -> size_str(Cs,Acc,Env);
size_str1([C|Cs],Acc,Env) -> size_str2(Cs,[C],Acc,Env);
size_str1([],Acc,Env) -> size_str([],Acc,Env).

%% allow leading space i.e ${ Var}
size_str2([$\s|Cs],Var,Acc,Env) -> size_str3(Cs,Var,Acc,Env);
size_str2([$\t|Cs],Var,Acc,Env) -> size_str3(Cs,Var,Acc,Env);
size_str2([$}|Cs],Var,Acc,Env) -> size_str_var(Cs,Var,Acc,Env);
size_str2([C|Cs],Var,Acc,Env) -> size_str2(Cs,[C|Var],Acc,Env);
size_str2([],_Var,Acc,Env) -> size_str([],Acc,Env).
%% allow trailing space i.e ${Var }
size_str3([$\s|Cs],Var,Acc,Env) -> size_str3(Cs,Var,Acc,Env);
size_str3([$\t|Cs],Var,Acc,Env) -> size_str3(Cs,Var,Acc,Env);
size_str3([$}|Cs],Var,Acc,Env) -> size_str_var(Cs,Var,Acc,Env);
size_str3(Cs,_Var,Acc,Env) -> size_str(Cs,Acc,Env).

size_str_var(Cs, Var, Acc, Env) ->
    case lists:keysearch(reverse(Var), 1, Env) of
	false -> size_str(Cs, Acc, Env);
	{value,{_,Value}}  -> size_str(Cs, length(Value)+Acc, Env)
    end.


%% size of mulitbyte integer.
size_mb_u_int32(X) ->
    if X < 128 -> 1;
       X > 0   -> size_mb_u_int32(X bsr 7, 1)
    end.

size_mb_u_int32(X, N) ->
    if X < 128 -> N+1;
       true -> size_mb_u_int32(X bsr 7, N+1)
    end.



	    
%% FIXME return {StartTag, PUDBID} !
module_to_pubid(wbxml_wml_10)     ->
    {"wml", "-//WAPFORUM//DTD WML 1.0//EN"};
module_to_pubid(wbxml_wml_11)     -> 
    {"wml", "-//WAPFORUM//DTD WML 1.1//EN"};
module_to_pubid(wbxml_wml_12)     -> 
    {"wml", "-//WAPFORUM//DTD WML 1.2//EN"};
module_to_pubid(wbxml_wml_13)     -> 
    {"wml", "-//WAPFORUM//DTD WML 1.3//EN"};
module_to_pubid(wbxml_wta_10)     -> 
    {"fixme", "-//WAPFORUM//DTD WTA 1.0//EN"};
module_to_pubid(wbxml_si_10)      -> 
    {"si", "-//WAPFORUM//DTD SI 1.0//EN"};
module_to_pubid(wbxml_sl_10)      -> 
    {"sl", "-//WAPFORUM//DTD SL 1.0//EN"};
module_to_pubid(wbxml_co_10)      -> 
    {"co", "-//WAPFORUM//DTD CO 1.0//EN"};
module_to_pubid(wbxml_channel_11) -> 
    {"channel", "-//WAPFORUM//DTD CHANNEL 1.1//EN"};
module_to_pubid(wbxml_channel_12) -> 
    {"channel", "-//WAPFORUM//DTD CHANNEL 1.2//EN"};
module_to_pubid(wbxml_prov_10)    -> 
    {"wap-provisioningdoc", "-//WAPFORUM//DTD PROV 1.0//EN"};
module_to_pubid(wbxml_wta_wml_12) -> 
    {"wta", "-//WAPFORUM//DTD WTA-WML 1.2//EN"};
module_to_pubid(wbxml_syncml_10)  -> 
    {"SyncML", "-//SYNCML//DTD SyncML 1.0//EN"};
module_to_pubid(wbxml_syncml_101) -> 
    {"SyncML", "-//SYNCML//DTD SyncML 1.0//EN"};
module_to_pubid(wbxml_syncml_11)  -> 
    {"SyncML", "-//SYNCML//DTD SyncML 1.1//EN"};
module_to_pubid(wbxml_syncml_112) -> 
    {"SyncML", "-//SYNCML//DTD SyncML 1.1//EN"};
module_to_pubid(wbxml_syncml_12)  -> 
    {"SyncML", "-//SYNCML//DTD SyncML 1.2//EN"};
module_to_pubid(wbxml_devinf_10)  -> 
    {"DevInf", "-//SYNCML//DTD DevInf 1.0//EN"};
module_to_pubid(wbxml_devinf_101) -> 
    {"DevInf", "-//SYNCML//DTD DevInf 1.0//EN"};
module_to_pubid(wbxml_devinf_11)  -> 
    {"DevInf", "-//SYNCML//DTD DevInf 1.1//EN"};
module_to_pubid(_)                -> undefined.



pubid_to_code("-//WAPFORUM//DTD WML 1.0//EN") -> ?WBXML_wml_10;
pubid_to_code("-//WAPFORUM//DTD WML 1.1//EN") -> ?WBXML_wml_11;
pubid_to_code("-//WAPFORUM//DTD WML 1.2//EN") -> ?WBXML_wml_12;
pubid_to_code("-//WAPFORUM//DTD WML 1.3//EN") -> ?WBXML_wml_13;
pubid_to_code("-//WAPFORUM//DTD WTA 1.0//EN") -> ?WBXML_wta_10;
pubid_to_code("-//WAPFORUM//DTD SI 1.0//EN") -> ?WBXML_si_10;
pubid_to_code("-//WAPFORUM//DTD SL 1.0//EN") -> ?WBXML_sl_10;
pubid_to_code("-//WAPFORUM//DTD CO 1.0//EN") -> ?WBXML_co_10;
pubid_to_code("-//WAPFORUM//DTD CHANNEL 1.1//EN") -> ?WBXML_channel_11;
pubid_to_code("-//WAPFORUM//DTD CHANNEL 1.2//EN") -> ?WBXML_channel_12;
pubid_to_code("-//WAPFORUM//DTD PROV 1.0//EN") -> ?WBXML_prov_10;
pubid_to_code("-//WAPFORUM//DTD WTA-WML 1.2//EN") -> ?WBXML_wta_wml_12;
pubid_to_code("-//SYNCML//DTD SyncML 1.0//EN") -> 16#FD1;
pubid_to_code("-//SYNCML//DTD SyncML 1.1//EN") -> 16#FD3;
pubid_to_code("-//SYNCML//DTD SyncML 1.2//EN") -> 16#1201;
pubid_to_code("-//SYNCML//DTD DevInf 1.0//EN") -> 16#FD2;
pubid_to_code("-//SYNCML//DTD DevInf 1.1//EN") -> 16#FD4; %% ?? spec says FD2!
pubid_to_code(_PubID) ->
    ?dbg("pubid_to_code: ~p not mapped\n", [_PubID]),
    ?WBXML_unknown.

pubcode_to_module({tref,Code}, St) ->
    PubId = lookup_string(Code, St),
    pubcode_to_module(pubid_to_code(PubId));
pubcode_to_module(Code, _St) ->
    pubcode_to_module(Code).
    

pubcode_to_module(?WBXML_wml_10) -> wbxml_wml_10;
pubcode_to_module(?WBXML_wml_11) -> wbxml_wml_11;
pubcode_to_module(?WBXML_wml_12) -> wbxml_wml_12;
pubcode_to_module(?WBXML_wml_13) -> wbxml_wml_13;
pubcode_to_module(?WBXML_wta_10) -> wbxml_wta_10;
pubcode_to_module(?WBXML_si_10) -> wbxml_si_10;
pubcode_to_module(?WBXML_sl_10) -> wbxml_sl_10;
pubcode_to_module(?WBXML_co_10) -> wbxml_co_10;
pubcode_to_module(?WBXML_channel_11) -> wbxml_channel_11;
pubcode_to_module(?WBXML_channel_12) -> wbxml_channel_12;
pubcode_to_module(?WBXML_prov_10) -> wbxml_prov_10;
pubcode_to_module(?WBXML_wta_wml_12) -> wbxml_wta_wml_12;
pubcode_to_module(?WBXML_unknown) -> undefined;
pubcode_to_module(16#FD1) -> wbxml_syncml_101;
pubcode_to_module(16#FD3) -> wbxml_syncml_112;
pubcode_to_module(16#1201) -> wbxml_syncml_12;
pubcode_to_module(16#1202) -> wbxml_syncml_12; %% Meta info
pubcode_to_module(16#FD2) -> wbxml_devinf_10;  %% Decodes 1.0, 1.0.1, 1.1
pubcode_to_module(16#FD4) -> wbxml_devinf_11;  %% ????
pubcode_to_module(_) -> undefined.


pubid_to_module(PublicId) ->
    pubcode_to_module(pubid_to_code(PublicId)).

version_to_code({1,0}) -> ?WBXML_10;
version_to_code({1,1}) -> ?WBXML_11;
version_to_code({1,2}) -> ?WBXML_12;
version_to_code({1,3}) -> ?WBXML_13;
version_to_code({Ma,Mi}) -> (((Ma-1) band 16#f) bsl 4) + Mi band 16#f.

code_to_version(?WBXML_10) -> {1,0};
code_to_version(?WBXML_11) -> {1,1};
code_to_version(?WBXML_12) -> {1,2};
code_to_version(?WBXML_13) -> {1,3};
code_to_version(Code) -> { ((Code bsr 4) band 16#f)+1, Code band 16#f}.


%%
%% strtbl = length *byte
%%
decode_strtbl(Data) ->
    {Length,Data1} = decode_mb_u_int32(Data),
    {TabBytes,Data2} = get_n_bytes(Data1, Length),
    Tab = make_strtbl(TabBytes,0,0,[],[]),
    {{Length,Tab}, Data2}.

%%
%% The string table is loaded reversed!
%%
make_strtbl([0|Cs],Pos0,Pos1,Acc,Tbl) ->
    make_strtbl(Cs,Pos1+1,Pos1+1,[],[{Pos0,Pos1+1,reverse(Acc)}|Tbl]);
make_strtbl([C|Cs],Pos0,Pos1,Acc,Tbl) ->
    make_strtbl(Cs,Pos0,Pos1+1,[C|Acc],Tbl);
make_strtbl([],Pos0,Pos1,Acc,Tbl) ->
    if Pos0 =/= Pos1 ->
	    if Acc == [] ->
		    Tbl;
	       true ->
		    [{Pos0,Pos1,reverse(Acc)}|Tbl]
	    end;
       true ->
	    Tbl
    end.

%% Create an initial string table from literals
new_strtbl(Literals) ->
    new_strtbl(Literals,{0,[]}).
    
new_strtbl([L|Ls],Tbl) ->
    String = if is_atom(L) -> atom_to_list(L);
		is_list(L) -> L
	     end,
    case find_str(String, Tbl) of
	false ->
	    {_Index,Tbl1} = ins_str(String,Tbl),
	    new_strtbl(Ls, Tbl1);
	_Index ->
	    new_strtbl(Ls, Tbl)
    end;
new_strtbl([], Tbl) ->
    Tbl.
    

lookup_string(Index, St) ->
    lookup_str(Index, St#wbxml_dec.strtbl).

lookup_str(Index, {Length,_Tab}) when Index >= Length ->
    "";
lookup_str(Index, {_, Tab}) ->
    lookup_str_tab(Index, Tab).

lookup_str_tab(Index, [{Start,_Stop,Str}|Tab]) ->
    if Index < Start ->
	    lookup_str_tab(Index, Tab);
       true ->
	    lists:nthtail(Index-Start, Str)
    end;
lookup_str_tab(_Index, []) ->
    "".

insert_string(String, St) ->
    case find_string(String, St) of
	false ->
	    if St#wbxml_enc.partial == true ->
		    %% Warn but add the string any way, we may be able
		    %% to backpatch if data is not already written...
		    io:format("Warning: updates of string table in stream mode not allowed, (use literal option to declare \"~s\")\n", [String]);
	       true ->
		    ok
	    end,
	    {Index,Tbl} = ins_str(String, St#wbxml_enc.strtbl),
	    {Index, St#wbxml_enc { strtbl = Tbl}};
	Index ->
	    {Index, St}
    end.

find_string(String, St) ->
    find_str(String, St#wbxml_enc.strtbl).

find_str(String, {_, Tab}) ->
    find_str_tab(String, Tab).

find_str_tab(String,[{_Start,Stop,Str}|Tab]) ->
    case lists:suffix(String, Str) of
	true -> 
	    Stop - length(String);
	false ->
	    find_str_tab(String, Tab)
    end;
find_str_tab(_String, []) ->
    false.

ins_str(String, {Length,Tab}) ->
    Len = length(String),
    Tab1 = [{Length,Length+Len,String} | Tab],
    {Length, {Length+Len+1,Tab1}}.


get_cstring(Cs) ->
    get_cstr(Cs, []).

get_cstr([0|Cs], Acc) ->
    {reverse(Acc), Cs};
get_cstr([C|Cs], Acc) ->
    get_cstr(Cs, [C|Acc]).

get_n_bytes(Data,N) ->
    get_n_bytes(Data,N,[]).

get_n_bytes(Data,0,Acc) ->
    {reverse(Acc),Data};
get_n_bytes([C|Data],I,Acc) ->
    get_n_bytes(Data,I-1,[C|Acc]).

strip([$\s|Cs]) -> strip(Cs);
strip([$\t|Cs]) -> strip(Cs);
strip([$\n|Cs]) -> strip(Cs);
strip([$\r|Cs]) -> strip(Cs);
strip(Cs) -> Cs.

%%
%% Module interface 
%%

%%
%% Load decode interface functions
%% We also make sure that the module exist and
%% that the function exists, if not we return undefined.
%%
%% FIXME: Generate the interface in the wbxml modules instead.
%%        That way I think the calls will be faster than
%%        apply calls.
%%
decode_interface(undefined) ->
    io:format("Warning: No decoder module given, trying generic coding\n", []),
    #wbxml_decf { tagf = fun(_T,_P) -> undefined end,
		  atrf = fun(_A,_P) -> undefined end,
		  valf = fun(_V,_P) -> undefined end,
		  extf = fun(_W,_T,_P,_V) -> undefined end
		 };
decode_interface(Mod) ->
    case code:ensure_loaded(Mod) of
	{error,_} ->
	    io:format("Warning: Decoder module ~s not found.\n", [Mod]),
	    #wbxml_decf { tagf = fun(_T,_P) -> undefined end,
			  atrf = fun(_A,_P) -> undefined end,
			  valf = fun(_V,_P) -> undefined end,
			  extf = fun(_W,_T,_P,_V) -> undefined end
			 };
	{module,M} ->
	    TagF =
		case erlang:function_exported(M,decode_tag,2) of
		    true ->
			fun(T,P) -> 
				M:decode_tag(T,P) 
			end;
		    false ->
			fun(_T,_P) -> undefined end
		end,
	    AtrF = 
		case erlang:function_exported(M,decode_attribute,2) of
		    true ->
			fun(A,P) -> M:decode_attribute(A,P) end;
		    false ->
			fun(_A,_P) -> undefined end
		end,
	    ValF =
		case erlang:function_exported(M,decode_value,2) of
		    true ->
			fun(V,P) -> M:decode_value(V,P) end;
		    false ->
			fun(_V,_P) -> undefined end
		end,
	    ExtF =
		case erlang:function_exported(M,decode_extension,4) of
		    true ->
			fun(W,E,P,V) -> M:decode_extension(W,E,P,V) end;
		    false ->
			fun(_W,_E,_P,_V) -> undefined end
		end,
	    #wbxml_decf { tagf = TagF,
			  atrf = AtrF,
			  valf = ValF,
			  extf = ExtF }
    end.

%%
%% Load encode interface functions
%% We also make sure that the module exist and
%% that the function exists, if not we return undefined.
%%
%% FIXME: Generate the interface in the wbxml modules instead.
%%        That way I think the calls will be faster than
%%        apply calls.
%%
encode_interface(undefined) ->
    io:format("Warning: No encoder module given, trying generic coding\n", []),
    #wbxml_encf { tagf = fun(_T,_P) -> undefined end,
		  atrf = fun(_A,_P,_V,_M) -> undefined end,
		  valf = fun(_A,_V,_P) -> undefined end,
		  extf = fun(_W,_T,_P,_V) -> undefined end
		 };    
encode_interface(Mod) ->
    case code:ensure_loaded(Mod) of
	{error,_} ->
	    io:format("Warning: Encoder module ~s not found.\n", [Mod]),
	    #wbxml_encf { tagf = fun(_T,_P) -> undefined end,
			  atrf = fun(_A,_P,_V,_M) -> undefined end,
			  valf = fun(_A,_V,_P) -> undefined end,
			  extf = fun(_W,_T,_P,_V) -> undefined end
			 };
	{module,M} ->
	    TagF =
		case erlang:function_exported(M,encode_tag,2) of
		    true ->
			fun(T,P) -> 
				?dbg("~w:encode_tag(~w, ~w)\n", [M,T,P]),
				M:encode_tag(T,P) 
			end;
		    false ->
			fun(_T,_P) -> undefined end
		end,
	    AtrF = 
		case erlang:function_exported(M,encode_attribute,4) of
		    true ->
			fun(A,V,P,Map) -> 
				?dbg("~w:encode_attribute(~w,~w,~w,~w)\n", 
				     [M,A,V,P,Map]),
				M:encode_attribute(A,V,P,Map) 
			end;
		    false ->
			fun(_A,_V,_P,_Map) -> undefined end
		end,
	    ValF =
		case erlang:function_exported(M,encode_value,3) of
		    true ->
			fun(A,V,P) -> 
				?dbg("~w:encode_value(~w,~w,~w)\n", 
				     [M,A,V,P]),
				M:encode_value(A,V,P) 
			end;
		    false ->
			fun(_A,_V,_P) -> undefined end
		end,
	    ExtF =
		case erlang:function_exported(M,encode_extension,4) of
		    true ->
			fun(W,A,V,P) -> M:encode_extension(W,A,V,P) end;
		    false ->
			fun(_W,_A,_V,_P) -> undefined end
		end,
	    #wbxml_encf { tagf = TagF,
			  atrf = AtrF,
			  valf = ValF,
			  extf = ExtF }
    end.
