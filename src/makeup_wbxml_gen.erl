%%% File    : makeup_wbxml_gen.erl
%%% Author  : Tony Rogvall <tony@PBook.local>
%%% Description : Generate Erlang modules from WBXML csv tab format
%%% Created : 20 Aug 2006 by Tony Rogvall <tony@PBook.local>

-module(makeup_wbxml_gen).

-export([compile/1, compile/2]).

-define(DQUOTE, $\").  % "
-define(SQUOTE, $\').  % '

-record(s,
	{
	  pubid      = [],   %% [{ID,PUDID}]
	  mimetype   = [],   %% [{<text|binary>, MimeType}]
	  tags       = [],   %% [{atom(), Code, Page}]
	  attributes = [],   %% [{atom(), Code, Page, string()}]
	  values     = [],   %% [{string(), Code, Page}]
	  extensions = []    %% [{type(),ctx(),tag(),pattern(),Page,FuncName}
	                     %% type()=opaque|ext0...ext2
	                     %% ctx=content|value
	                     %% tag()='_'|atom()
	                     %% pattern()='_'|string()  (PREFIX)
	                     %% makeup_wbxml_ext:FuncName 
	 }).

compile(File) ->
    compile(File, []).

compile(File, Opts) ->
    case csvfile:fold(File,
		 [{quote,?DQUOTE},{xquote,?SQUOTE},
		  {comment,$\#},{space,trim}], 
		      fun(A,S) -> [A|S] end, []) of
	Error={error,_} ->
	    Error;
	List ->
	    Name   = filename:basename(File, ".tab"),
	    Prefix = getopt(prefix, Opts, "wbxml_"),
	    Module = getopt(mod, Opts, list_to_atom(Prefix++Name)),
	    OutDir = getopt(outdir, Opts, "."),
	    OutFile = filename:join(OutDir, Prefix++Name++".erl"),
	    {ok,Sections} = comp(lists:reverse(List), #s{}, 
				 undefined, undefined),
	    generate(Sections, File, OutFile, Module)
    end.

comp([ Ts | List], S, Section, Page) ->
    case Ts of
	["tag", "page", Page1 | T] when T==[]; T==[[]] ->
	    comp(List, S, tag, erlang:list_to_integer(Page1, 10));
	["attribute", "page", Page1 | T] when T==[]; T==[[]] ->
	    comp(List, S, attribute, erlang:list_to_integer(Page1, 10));
	["value", "page", Page1 | T] when T==[]; T==[[]] ->
	    comp(List, S, value, erlang:list_to_integer(Page1, 10));
	["extension", "page", Page1 | T] when T==[]; T==[[]] ->
	    comp(List, S, extension, erlang:list_to_integer(Page1, 10));
	
	[Code, Name|T] when Section==tag, T==[]; 
			    Section==tag, T==[[]] ->
	    E = {list_to_atom(Name),erlang:list_to_integer(Code,16),Page},
	    comp(List, S#s { tags = [E|S#s.tags] }, tag, Page);

	[Code, Name | Prefix0] when Section==attribute ->
	    Prefix = case Prefix0 of
			 [] -> "";
			 [P1] -> P1
		     end,
	    E = {list_to_atom(Name),erlang:list_to_integer(Code,16),Page,
		 Prefix},
	    comp(List, S#s { attributes = [E|S#s.attributes] },attribute, Page);
	[Code, String|T] when Section == value, T==[];
	                      Section == value, T==[[]] ->
	    E = {String,erlang:list_to_integer(Code,16),Page},
	    comp(List, S#s { values = [E|S#s.values] }, value, Page);

	[Ext, CtxPart, CtxTag, CtxVal, Fun|T] when 
	      Section == extension, T==[];
	      Section == extension, T==[[]] ->
	    E = {list_to_atom(Ext),
		 list_to_atom(CtxPart),
		 list_to_atom(CtxTag),
		 CtxVal,
		 Page,
		 list_to_atom(Fun)},
	    comp(List, S#s { extensions = [E|S#s.extensions]}, extension, Page);

	["pubid",Code,PubID|T] when Section == undefined, T==[];
				    Section == undefined, T==[[]] ->
	    E = {erlang:list_to_integer(Code,16), PubID},
	    comp(List, S#s { pubid = [E|S#s.pubid] }, Section,  Page);

	["mimetype", Style, MimeType|T] when Section == undefined, T==[];
					     Section == undefined, T==[[]] ->
	    E = {list_to_atom(Style), MimeType},
	    comp(List, S#s { mimetype = [E|S#s.mimetype] }, Section,  Page);

	_ ->
	    io:format("Syntax error: ~p\n", [Ts]),
	    io:format("Or check page declaration\n",[]),
	    {error, bad_syntax}
    end;
comp([], S, _Section, _Page) ->
    {ok, S}.


generate(S, _InFile, OutFile, Module) ->
    case file:open(OutFile, [write]) of
	{ok,Fd} ->
	    Res = 
		(catch
		    begin 
			gen_header(Fd, Module),
			gen_decl(Fd, S),
			if S#s.tags == [] -> ok;
			   true ->
				gen_tags(Fd, S)
			end,
			if S#s.attributes == [] -> ok;
			   true -> 
				gen_attributes(Fd, S)
			end,
			if S#s.values == [] -> ok;
			   true -> 
				gen_values(Fd, S)
			end,
			if S#s.extensions == [] -> ok;
			   true -> 
				gen_extensions(Fd, S)
			end
		    end),
	    file:close(Fd),
	    case Res of
		{'EXIT',Reason} ->
		    io:format("crash: ~p\n", [Reason]),
		    halt(1);
		_ ->
		    ok
	    end;
	Error ->
	    Error
    end.

makeup_version() ->
    case application:get_key(makeup, vsn) of
	{ok, Vsn} -> Vsn;
	undefined -> "unknown"
    end.

gen_header(Fd, Module) ->
    io:format(Fd, "%% WBXML module generated by: ~s\n"
	          "%% Version: ~p\n"
	          "%%    Data: ~s\n",
	      [?MODULE, 
	       makeup_version(),
	       format_date(calendar:universal_time())]),
    io:format(Fd, "-module('~s').\n",[Module]).

%%
%% Generate export declarations for used functionallity
%% Generate pubid and mimetype encode/decode
%%
gen_decl(Fd, S) ->
    io:put_chars(Fd, "\n"),
    
    if S#s.pubid == [] -> ok;
       true -> io:put_chars(Fd, "-export([pubid/1]).\n")
    end,

    if S#s.mimetype == [] -> ok;
       true -> io:put_chars(Fd, "-export([mimetype/1]).\n")
    end,

    if S#s.tags == [] -> ok;
       true ->
	    io:put_chars(Fd, "-export([decode_tag/2]).\n"),
	    io:put_chars(Fd, "-export([encode_tag/2]).\n")
    end,

    if S#s.attributes == [] -> ok;
       true ->
	    io:put_chars(Fd, "-export([decode_attribute/2]).\n"),
	    io:put_chars(Fd, "-export([encode_attribute/4]).\n")
    end,
    if S#s.values == [] -> ok;
       true ->
	    io:put_chars(Fd, "-export([decode_value/2]).\n"),
	    io:put_chars(Fd, "-export([encode_value/3]).\n")
	    
    end,
    if S#s.extensions == [] -> ok;
       true ->
	    io:put_chars(Fd, "-export([decode_extension/4]).\n"),
	    io:put_chars(Fd, "-export([encode_extension/4]).\n")
    end,

    io:put_chars(Fd, "\n"),

    if S#s.pubid == [] -> ok;
       true ->
	    gen(
	      fun({ID,PubID},Sep) ->
		      io:format(Fd, "pubid(16#~2.16.0B) -> \"~s\"~s\n",
				[ID,PubID,Sep])
	      end, ";", ".", S#s.pubid)
    end,
    io:format(Fd, "\n", []),
    if S#s.mimetype == [] ->
	    io:format(Fd, "mimetype(_Style) -> [].\n", []);
       true ->
	    MimeList =
		lists:map(fun({Style,Types}) ->
				  {Style,lists:map(fun({T}) -> T end, Types)}
			  end, key_group(1, S#s.mimetype)),
	    gen(
	      fun({Style,MimeTypes},Sep) ->
		      io:format(Fd, "mimetype(~s) -> ~p~s\n", 
				[Style,MimeTypes,Sep])
	      end, ";", ".", MimeList)
    end.

%%
%% Generate tag encode/decode
%%
gen_tags(Fd, S) ->
    %% generate decode section.
    Tags = lists:sort(S#s.tags),
    gen_doc(Fd, ["decode_tag(Code,CodePage)", "TagName"]),
    gen(
      fun({Tag,Code,Page},Sep) ->
	      io:format(Fd, "decode_tag(16#~2.16.0B,~w) -> '~s'~s\n",
			[Code,Page,Tag,Sep])
      end, ";", ";", Tags),
    io:format(Fd, "decode_tag(_Code,_Page) -> undefined.\n", []),

    gen_doc(Fd, ["encode_tag(TagName,CurrentPage)","Code", "{Code,NewPage}"]),
    %% clauses that match the current code page
    gen(
      fun({Tag,Code,Page},Sep) ->
	      io:format(Fd, "encode_tag('~s',~w) -> 16#~2.16.0B~s\n",
			[Tag,Page,Code,Sep])
      end, ";", ";", Tags),
    
    %% clauses that switch code page (take the first available)
    Tags1 = uukeysort(1, Tags),
    
    gen(
      fun({Tag,Code,Page},Sep) ->
	      io:format(Fd,"encode_tag('~s',_) -> {16#~2.16.0B,~w}~s\n",
			[Tag,Code,Page,Sep])
      end,  ";", ";", Tags1),
    io:format(Fd, "encode_tag(_Tag,_Page) -> undefined.\n", []).

%%
%% Generate attribute encode/decode
%%
gen_attributes(Fd, S) ->
    %% generate decode section.
    Attributes = lists:keysort(2, lists:keysort(3, S#s.attributes)),

    gen_doc(Fd, ["decode_attribute(Code,CodePage)","{Attribute,Prefix}"]),
    gen(
      fun({Name,Code,Page,Prefix},Sep) ->
	      io:format(Fd, "decode_attribute(16#~2.16.0B,~w) -> {'~s',\"~s\"}~s\n",
			[Code,Page,Name,Prefix,Sep])
      end, ";", ";", Attributes),
    io:format(Fd, "decode_attribute(_Code,_Page) -> undefined.\n", []),

    %%
    %% Group Attributes [{Attribute,Code,Page,Prefix}] on form:
    %%  
    %%  [ {Attribute, [{Prefix,[{Code,Page}]}]} ]
    %%
    %%  reverse sorted on Prefix:
    %%
    gen_doc(Fd,["encode_attribute(Name,Value,Page,Map)",
		"{Code, Page',Value'}"]),
    EAttributes = 
	lists:map(fun({Attr,PCPs}) ->
		    {Attr,key_group(3, lists:reverse(lists:keysort(3, PCPs)))}
	    end, key_group(1, lists:keysort(1, S#s.attributes))),
    %% Condition to include map_page function (avoid warnings)
    UseMapPage =
	lists:any(
	  fun({_Name,PCPs}) ->
		  if length(PCPs) > 1 ->
			  lists:any(fun({_Prefix,CPs}) ->
					    length(CPs) > 1
				    end, PCPs);
		     true -> false
		  end
	  end, EAttributes),
			  
    gen(
      fun({Name,[{"",[{Code,Page}]}]},Sep0) ->
	      io:format(Fd, "encode_attribute('~s',Value,_Page,_Map) ->\n",
			[Name]),
	      io:format(Fd, "  {16#~2.16.0B,~w,Value}~s\n", 
			[Code,Page,Sep0]);
	 ({Name,[{Prefix,[{Code,Page}]}]},Sep0) ->
	      io:format(Fd, "encode_attribute('~s',\"~s\"++T,_Page,_Map) ->\n",
			[Name,Prefix]),
	      io:format(Fd, "  {16#~2.16.0B,~w,T}~s\n", 
			[Code,Page,Sep0]);
	 ({Name,PCPs},Sep0) ->
	      %% This is to generate _ (warning avoid)
	      LHf = case lists:any(fun({_,CPs}) -> length(CPs)>1 end, PCPs) of
			true -> "";
			false -> "_"
		    end,
	      io:format(Fd, "encode_attribute('~s',Value,~sPage,~sMap) ->\n",
			[Name,LHf,LHf]),
	      io:put_chars(Fd, "  case Value of\n"),
	      gen(
		fun
		    ({Prefix,CPs},Sep) ->
			[{MinCode,MinPage}|_] = lists:keysort(2, CPs),
			if length(CPs) == 1, Prefix == "" ->
				io:format(Fd, "    "
					  "_ -> {16#~2.16.0B,~w,Value}~s\n", 
					  [MinCode,MinPage,Sep]);
			   length(CPs) == 1 ->
				io:format(Fd, "    "
					  "\"~s\"++T -> {16#~2.16.0B,~w,T}~s\n", 
					  [Prefix,MinCode,MinPage,Sep]);
			   true ->
				[{MC,MP}|_] = RCPs1 = lists:reverse(
							lists:keysort(2,CPs)),
				io:format(Fd, "    "
					  "\"~s\"++T -> ", [Prefix]),
				io:format(Fd, "\n      case Page of\n", []),
				io:put_chars(Fd,
					     lists:map(fun({C,P}) ->
							       io_lib:format("        "
								       "~w -> "
								       "map_page(Page,16#~2.16.0B,~w,Map,T);\n", [P,C,P])
						 end, RCPs1)),
				io:put_chars(Fd,
					     io_lib:format("        "
							   "_ -> map_page(Page,16#~2.16.0B,~w,Map,T)\n"
							   "      end~s\n",
							   [MC,MP,Sep]))
			end
		end, ";", "", PCPs),
	      io:format(Fd, "  end~s\n", [Sep0])
      end, ";", ";", EAttributes),
    io:format(Fd, "encode_attribute(_Attr,_Value,_Page,_Map) -> undefined.\n",
	      []),
    if UseMapPage == true ->
	    io:put_chars(Fd,
			 ["\n",
			  "map_page(Page,Code,Page1,[],T) -> {Code,Page1,T};\n",
			  "map_page(_Page,Code,_,[{Code,Page2}|_],T) -> {Code,Page2,T};\n",
			  "map_page(Page,Code,Page1,[_|Map], T) -> map_page(Page,Code,Page1,Map,T).\n",
			  "\n"]);
       true ->
	    ok
    end.


gen_values(Fd, S) ->
    %% generate decode section.
    gen_doc(Fd, ["decode_value(Code,CodePage)", "Value"]),

    DValues = lists:keysort(2, lists:keysort(3, S#s.values)),
    gen(
      fun({Value,Code,Page},Sep) ->
	      io:format(Fd, "decode_value(16#~2.16.0B,~w) -> ~p~s\n",
			[Code,Page,Value,Sep])
      end, ";", ".", DValues),

    gen_doc(Fd, ["encode_value(Context,Value,Page)", "Value"]),
    %% sort on string and run reversed to match prefix first
    EValue = key_group(1, lists:reverse(lists:keysort(1, DValues))),
    io:put_chars(Fd, 
		 "encode_value(Ctx,Value,Page) -> \n"
		 "  e_value(Ctx,Value,Page,[],[]).\n\n"),
    
    io:put_chars(Fd,
		 "e_value(_Ctx,[],_CP,String,Codes) ->\n"
		 "  lists:reverse(e_string(String,Codes));\n"),
    io:put_chars(Fd,
		 "e_value(Ctx,Text,CP,Str,Codes) -> \n"
		 "  case Text of\n"),
    lists:foreach(
      fun
	  ({Value,[{Code,Page}]}) ->
	      io:format(Fd, "    ~p ++ Text1 ->\n"
			"      e_code(Ctx,Text1,16#~2.16.0B,CP,~w,Str,Codes);\n",
			[Value,Code,Page]);
	  ({Value,[{Code0,Page0}|CPs]}) ->
	      io:format(Fd, "    ~p ++ Text1 ->\n", [Value]),
	      io:format(Fd, "      if CP == ~w ->\n", [Page0]),
	      io:format(Fd, "        e_code(Ctx,Text1,16#~2.16.0B,CP,~w,Str,Codes);\n", [Code0,Page0]),
	      lists:map(
		fun({Codei,Pagei}) ->
			io:format(Fd, "         CP == ~w ->\n", [Pagei]),
			io:format(Fd, "           e_code(Ctx,Text1,16#~2.16.0B,CP,~w,Str,Codes);\n", [Codei,Pagei])
		end, CPs),
	      io:format(Fd, "         true ->\n", []),
	      io:format(Fd, "           e_code(Ctx,Text1,16#~2.16.0B,CP,~w,Str,Codes)\n", [Code0,Page0]),
	      io:format(Fd, "       end;\n", [])
      end,  EValue),

    VExts0 =
	lists:foldl(fun({_Ext,value,'_',Val,_Page,Func},Acc) when Val =/= [] ->
			    [{'_',Val,Func}|Acc];
		       ({_Ext,value,Ctx,Val,_Page,Func},Acc) ->
			    [{Ctx,Val,Func}|Acc];
		       (_Ext,Acc) ->
			    %% io:format("Skip: ~p\n", [Ext]),
			    Acc
		    end,  [], S#s.extensions),
    VExts1 = ordsets:from_list(VExts0),
    VExts  = ordsets:to_list(VExts1),
    lists:foreach(
      fun({'_',Val,Func}) when Val =/= [] ->
	      io:format(Fd, 
			"    \"~s\" ++ _ ->\n"
			"      {Code,Text1} = makeup_wbxml_ext:~s_encode(Text,Ctx,CP),\n"
			"      e_value(Ctx,Text1,CP,[],[Code|e_string(Str,Codes)]);\n",
			[Val,Func]);
	 ({Ctx,Val,Func}) ->
	      io:format(Fd, 
			"    \"~s\" ++ _ when Ctx == ~p ->\n"
			"      {Code,Text1} = makeup_wbxml_ext:~s_encode(Text,Ctx,CP),\n"
			"      e_value(Ctx,Text1,CP,[],[Code|e_string(Str,Codes)]);\n",
			[Val,Ctx,Func]);
	 (_Ext) ->
	      %% io:format("Skip: ~p\n", [Ext]),
	      ok
      end, VExts),

    io:put_chars(Fd, 
		 "    [C|Text1] ->\n"
		 "      e_value(Ctx,Text1, CP, [C|Str], Codes)\n"
		 "  end.\n"),
    io:put_chars(Fd,
		 "\n"
		 "e_string([], Codes) ->\n"
		 "  Codes;\n"
		 "e_string(String, Codes) ->\n"
		 "  [{string,lists:reverse(String)}|Codes].\n"
		 "\n"
		 "e_code(Ctx,Text, Code, CP, CP, Str, Codes) ->\n"
		 "    e_value(Ctx,Text, CP, [], [Code|e_string(Str,Codes)]);\n"
		 "e_code(Ctx,Text, Code, _CP, NP, Str, Codes) ->\n"
		 "    e_value(Ctx,Text, NP, [], \n"
		 "		 [Code,{page,NP}|e_string(Str,Codes)]).\n"
		).


gen_extensions(Fd, S) ->
    %% generate extension section.
    gen_doc(Fd, ["decode_extension(Part,Ext,Page,ExtVal)", "Value"]),
    
    gen(
      fun({Ext,Part,_Tag,_Val,_Page,Function},Sep) ->
	      io:format(Fd,
			"decode_extension(~s,~s,Page,Data) ->\n"
			"  makeup_wbxml_ext:~s_decode(~s,~s,Page,Data)~s\n",
			[Part,Ext,Function,Ext,Part,Sep])
      end, ";", ";", S#s.extensions),
    io:format(Fd,"decode_extension(_Part,_Ext,_Page,_Data) -> undefined.\n",[]),

    gen_doc(Fd, ["encode_extension(Part,Ext,Page,ExtVal)", "Codes"]),

    VExts0 =
	lists:map(fun({_Ext,Part,Tag,Val,_Page,Func}) -> 
			  {Part,Tag,Val,Func}
		  end, S#s.extensions),
    VExts1 = ordsets:from_list(VExts0),
    VExts  = ordsets:to_list(VExts1),

    gen(
      fun({Part,Tag,_Val,Function},Sep) ->
	      io:format(Fd,
			"encode_extension(~s,~p,Page,Data) ->\n"
			"  makeup_wbxml_ext:~s_encode(Data,~p,Page)~s\n",
			[Part,Tag,Function,Tag,Sep])
      end, ";", ";", VExts),
    io:format(Fd,"encode_extension(_Part,_Tag,_Page,_Data) -> undefined.\n",[]).

%%"
%% select the first element on the key discard the rest
%%
uukeysort(_Pos,[]) ->
    [];
uukeysort(Pos,List) ->
    [H|T] = lists:keysort(Pos,List),
    uukeysort(Pos,T,[H]).

uukeysort(Pos, [H|T], As=[A|_]) ->
    if element(Pos,H) == element(Pos,A) ->
	    uukeysort(Pos, T, As);
       true ->
	    uukeysort(Pos, T, [H|As])
    end;
uukeysort(_Pos,[], As) ->
    lists:reverse(As).


%% group a key list on key=pos
%% key_group(1, [{a,b,c}, {a,c,d}, {b,e,f}, {b,g,h}]) ->
%%   =>  [{a,[{a,c},{c,d}]}, {b,[{e,f}, {g,h}]}]
%%
key_group(Pos, [H|T]) ->
    H1 = remove_element(Pos, H),
    E1 = element(Pos, H),
    key_group(Pos, E1, [H1], T, []);
key_group(_Pos, []) ->
    [].

key_group(Pos, E, Group, [H|T], Acc) ->
    H1 = remove_element(Pos, H),
    E1 = element(Pos, H),
    if E == E1 ->
	    key_group(Pos, E, [H1|Group], T, Acc);
       true ->
	    key_group(Pos, E1, [H1], T, [{E,lists:reverse(Group)}|Acc])
    end;
key_group(_Pos, E, Group, [], Acc) ->
    lists:reverse([{E,lists:reverse(Group)}|Acc]).


remove_element(I, Tuple) ->
    List = tuple_to_list(Tuple),
    Len  = length(List),
    case I of
	1 -> list_to_tuple(tl(List));
	2 -> list_to_tuple([hd(List),tl(tl(List))]);
	N when N == Len ->
	    list_to_tuple(lists:sublist(List,1,N-1));
	N ->
	    list_to_tuple(lists:sublist(List,1,N-1)++
			  lists:sublist(List,N+1,Len))
    end.

%% foreach that send Sep or LSep (Sep for separator token)
gen(_Fun,_Sep,_LSep,[]) ->
    ok;
gen(Fun,_Sep,LSep,[H]) ->
    Fun(H,LSep);
gen(Fun,Sep,LSep,[H|T]) ->
    Fun(H,Sep),
    gen(Fun,Sep,LSep,T).

getopt(Opt, Opts, Default) ->
    case lists:keysearch(Opt, 1, Opts) of
	false -> Default;
	{value,{_,Value}} -> Value
    end.

gen_doc(Fd, [Head,Alt1|Alts]) ->
    io:put_chars(Fd,
		 ["%%\n",
		  "%% ", Head," ->\n",
		  "%%    ", Alt1, "\n",
		  lists:map(fun(A) -> ["%%  | ", A, "\n"] end, Alts),
		  "%%\n"]).


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