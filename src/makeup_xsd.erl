%%% File    : makeup_xsd.erl
%%% Author  : Tony Rogvall <tony@iMac.local>
%%% Description : XMLSchema verifier and schema generator
%%% Created : 18 Nov 2005 by Tony Rogvall <tony@iMac.local>

-module(makeup_xsd).

-export([file/1, file_xsd/1, file_makeup/1, file_makeup/2]).
-export([xml_xsd/1, xml_makeup/1, xsd_makeup/1]).
-export([load/2]).
-export([gen_coders/5]).
-export([gen_hrl/3]).
-export([gen_types/4]).
-export([gen_def/3]).
-export([gen_fields/2]).
-export([xml_makeup/2]).

-export([encode_string/1]).
-export([encode_int/1]).
-export([encode_byte/1]).
-export([encode_short/1]).
-export([encode_boolean/1]).
-export([decode_string/4]).
-export([decode_int/4]).
-export([decode_byte/4]).
-export([decode_short/4]).
-export([decode_boolean/4]).
-export([decode_string/1]).
-export([decode_int/1]).
-export([decode_byte/1]).
-export([decode_short/1]).
-export([decode_boolean/1]).

-export([mk_annotation/1]).
-export([mk_comment/1]).
-export([mk_comment_xml/1]).
-export([xsd_cs/2]).
-export([attr/2]).
-export([attr_am/2]).

-include("../include/makeup.hrl").
-include("../include/makeup_xsd.hrl").

-define(ATTRIBUTES(XsdName,As),
	attributes(record_info(fields,XsdName), #XsdName {}, As)).

-record(xsd_state,
	{
	  ns,               %% Namespace tag
	  tns,              %% Target name space
	  types = [],
	  elements = [],
	  attributes = [],
	  entities = [],
	  error_list = []
	 }).

file(File) ->
    makeup:file(File, [{space,normalize},
		       {validate,false},
		       {ns, true},
		       %% {dtd, xmlschema_dtd},
		       {start, [xs|schema]}
		      ]).

file_xsd(File) ->
    case file(File) of
	{ok,Doc} ->
		io:format("Content:~p\n",[Doc#makeup_document.content]),
		{_,A,V} = hd(lists:reverse(Doc#makeup_document.content)),
		XML = {[roap|schema],A,V},
	    XSD = xsd_schema(XML),
	    {ok,XSD};
	Error ->
	    Error
    end.

file_makeup(File) ->
    file_makeup(File,[]).

file_makeup(File,Opts) ->
    case file_xsd(File) of
	{ok,XSD} ->
	    St = mk(XSD, #xsd_state{},Opts),
	    {ok,{St#xsd_state.types, St#xsd_state.elements}}; 
	Error -> Error
    end.

gen_coders(HrlFileName,Types,Elements,Nss,Opts) ->
    gen_coders([],HrlFileName,Types,Types,Elements,Nss,Opts).

gen_coders(Fds,_,[],_,_,_,_) ->
    lists:foreach(fun({_,Fd}) -> file:close(Fd) end, Fds);
gen_coders(Fds,HrlFileName,[{[Ns|_Type]=NsType,_}=Def|T],Types,Elements,Nss,Opts) ->
    io:format("gen_coders:~p\n",[Def]),
    {Fd,Fds1} = case lists:keysearch(Ns,1,Fds) of
		    {value,{_,OFd}} -> {OFd,Fds};
		    _ ->
			ModuleName = "makeup_"++atom_to_list(Ns),
			IncNs = [xmlns|Ns],
			{value,{_,NsName}} = lists:keysearch(IncNs,1,Nss),
			{ok,NFd} = file:open(ModuleName ++ ".erl", [write]),
			io:format(NFd,"-module(" ++ ModuleName ++ ").\n",[]),
			io:format(NFd,"-include(\"" ++ HrlFileName ++ "\").\n",[]),
			io:format(NFd,"-compile(export_all).\n\n\n",[]),
			io:format(NFd,"-define(NS,{'" ++ makeup:format_attr_name(IncNs) ++ "',\"" ++  NsName ++ "\"}).\n\n\n",[]),
			io:format(NFd,"find_record(Body, ID) ->\n" 
				  "    Rec = [{_,Attr,_}] = lists:filter(fun({_,A,_}) -> lists:member({id,ID},A) end,Body),\n"
				  "    {value, {_,TA}} = lists:keysearch([xsi|type],1,Attr),\n"
				  "    {TA,Rec}.\n", []),

			{NFd,[{Ns,NFd}|Fds]}
		end,
    gen_encode(Fd,Def,Types,Elements),
    gen_decode(Fd,NsType),
    gen_coders(Fds1,HrlFileName,T,Types,Elements,Nss,Opts).

gen_encode(Fd,{[_Ns|EType],Def}=_Type,Types,Elements) ->
    FunctionName = "encode_" ++ atom_to_list(EType),
    Record = "'" ++ atom_to_list(EType) ++ "'",
    Fields = fields_def(Def,Types),
    Map = lists:map(fun(F) -> {value,{_,{ref,Ref}}} = lists:keysearch(F,1,Elements), {F,makeup:name_ns(Ref,true)} end,Fields),
    FormatedFields = lists:reverse(format_fields(Map,Record,[])),
    io:format(Fd, "\n\n%% ~s : ~s() => soap encoded\n",[FunctionName,atom_to_list(EType)]),
    io:format(Fd, FunctionName ++ "(Rec) when is_record(Rec," ++ Record ++ ") ->\n",[]),
    io:format(Fd, ["[\n",FormatedFields,"];\n",FunctionName,"(_) -> []."],[]).

format_fields([],_,Acc) ->
    io:format("Empty fields\n",[]),
    Acc;
format_fields([{F,T}],Record,Acc) when is_atom(T) -> format_fields([{F,[xsd|T]}],Record,Acc);
format_fields([{F,Type}|T],Record,Acc) when is_atom(Type) -> format_fields([{F,[xsd|Type]}|T],Record,Acc);
format_fields([{[_|F],[Ns|EType]}],Record,Acc) ->
    ["    {'" ++ atom_to_list(F) ++ "'," ++ "[{'xsi:type',\"" ++ makeup:format_attr_name([Ns|EType]) ++ "\"}],[makeup_" ++ atom_to_list(Ns) ++ ":encode_" ++ atom_to_list(EType) ++ "(Rec#"++ Record ++ ".'" ++ atom_to_list(F) ++ "')]}"|Acc];
format_fields([{[_|F],[Ns|EType]}|T],Record,Acc) ->
    format_fields(T,Record,["    {'" ++ atom_to_list(F) ++ "'," ++ "[{'xsi:type',\"" ++ makeup:format_attr_name([Ns|EType]) ++ "\"}],[makeup_" ++ atom_to_list(Ns) ++ ":encode_" ++ atom_to_list(EType) ++ "(Rec#"++ Record ++ ".'" ++ atom_to_list(F) ++ "')]},\n"|Acc]).

fields_def([],_Types) ->
    io:format("Empty def\n",[]),
    [];
fields_def({extension,Base,Def},Types) ->
    {value, {_,BaseDef}} = lists:keysearch(Base, 1, Types),
    fields_def(BaseDef,Types) ++ fields_def(Def,Types);

fields_def({sequence,{_Min,_Max},Els}, _) ->
    get_fields(Els,[]);

fields_def(_Other, _) -> [].


get_fields([],Acc) -> lists:reverse(Acc);
get_fields([{element,{_EMin,_EMax},NsType}|T],Acc) ->
    get_fields(T,[NsType|Acc]);
get_fields([_Other|T],Acc) ->
    get_fields(T,Acc).

gen_decode(Fd,[_Ns|Type]) ->
    FunctionName = "decode_" ++ atom_to_list(Type),
    Record = "'" ++ atom_to_list(Type) ++ "'",
    io:format(Fd, "\n\n%% ~s : soap body => ~s()\n",[FunctionName,atom_to_list(Type)]),
    io:format(Fd, FunctionName ++ "([],_,_,_) -> #" ++ Record ++ "{};\n",[]),
    io:format(Fd, FunctionName ++ "([{_,Attrs,Values}], Body,EnvAttrs,Ns2Mod) ->\n",[]),
    io:format(Fd, "    Fields = record_info(fields," ++ Record ++ "),\n"
	      "    {_,Rec} = lists:foldl(fun(F,{Idx,Rec}) ->\n"
	      "        {value, {_,Attr,Val0}} = lists:keysearch(F,1,Values),\n"
	      "        {TypeAttr,Val} = case lists:keysearch(href,1,Attr) of\n"
	      "        {value, {_,[$#|ID]}} -> find_record(Body,ID);\n"
	      "        _ ->\n"
	      "            {value, {_,TA}} = lists:keysearch([xsi|type],1,Attr),\n"
	      "            {TA,Val0}\n"
	      "        end,\n"
	      "        ValAttr = case Val of [{_,A,_}] -> A; _ -> Attr end,\n"
	      "        [NsStr,Type] = string:tokens(TypeAttr,\":\"),\n"
	      "        {value, {_,NsUri}} = lists:keysearch([xmlns|list_to_atom(NsStr)],1,ValAttr++Attrs++EnvAttrs),\n"
	      "        Mod = Ns2Mod(NsUri),\n"
	      "        Fun = list_to_atom(\"decode_\" ++ Type),\n"
	      "        {Idx+1,setelement(Idx,Rec,Mod:Fun(Val,Body,Attrs++EnvAttrs,Ns2Mod))}\n"
	      "    end,{2,#" ++ Record ++ "{}},Fields),\n"
	      "    Rec.",[]).


gen_hrl(Fd,Types,Opts) ->
    gen_types(Fd, Types, Types, Opts).

gen_types(_,[],_,_) -> ok;
gen_types(Fd, [{[_Ns|Type]=Tag,Def}|Ts],Types,Opts) ->
    RecordName = case get_opt(hrl_ns, Opts, false) of
		     true -> makeup:format_tag_name(Tag);
		     false -> atom_to_list(Type)
		 end,
    io:format(Fd, "-record('~s',{",[RecordName]),
    gen_def(Fd, Def, Types),
    io:format(Fd, "}).\n\n",[]),
    gen_types(Fd,Ts,Types,Opts).

gen_def(Fd, {extension,Base,Def},Types) ->
    {value, {_,BaseDef}} = lists:keysearch(Base, 1, Types),
    gen_def(Fd,BaseDef,Types),
    io:format(Fd, ",",[]),
    gen_def(Fd,Def,Types);

gen_def(Fd, {sequence,{_Min,_Max},Els}, _) ->
    gen_fields(Fd,Els);

gen_def(Fd, Other, _) ->
    io:format(Fd, "%% ~p\n",[Other]).

gen_fields(_Fd,[]) ->
	io:format("warn: empty fields\n",[]),
	ok;
gen_fields(Fd,[{element,{_EMin,_EMax},[_|EType]}]) ->
    io:format(Fd,"\n    '~s'",[atom_to_list(EType)]);
gen_fields(Fd,[{element,{_EMin,_EMax},[_|EType]}|T]) ->
    io:format(Fd,"\n    '~s',",[atom_to_list(EType)]),
    gen_fields(Fd,T);
gen_fields(Fd,[Other]) ->
    io:format(Fd,"\n   %%~p",[Other]);
gen_fields(Fd,[Other|T]) ->
    io:format(Fd,"\n   %%~p",[Other]),
    gen_fields(Fd,T).

get_opt(Opt, Opts, Default) ->
    case lists:keysearch(Opt, 1, Opts) of
	false -> Default;
	{value, {_, Value}} -> Value
    end.


xml_xsd(XML) ->
    XSD = xsd_schema(XML),
    {ok,XSD}.

xml_makeup(XML) ->
    {ok,XSD} = xml_xsd(XML),
    xsd_makeup(XSD).

xml_makeup(XML, Opts) ->
    {ok,XSD} = xml_xsd(XML),
    xsd_makeup(XSD, Opts).

xsd_makeup(XSD) ->
    xsd_makeup(XSD,[]).

xsd_makeup(XSD,Opts) ->
    St = mk(XSD, #xsd_state{},Opts),
    {ok,{St#xsd_state.types, St#xsd_state.elements}}.


load("http://"++_HostPath, _Opts) ->
    {error, not_implemented};    
load("https://"++_HostPath, _Opts) ->
    {error, not_implemented};    
load(File, Opts) ->
    file_makeup(File, Opts).


%%
%% Built-in type encode decode
%%
encode_string(undefined) -> [];
encode_string(X) when is_list(X) -> [X].
encode_int(undefined) -> [];
encode_int(X) when is_integer(X) -> [integer_to_list(X)].
encode_byte(undefined) -> [];
encode_byte(X) when is_integer(X) -> [integer_to_list(X)].
encode_short(undefined) -> [];
encode_short(X) when is_integer(X) -> [integer_to_list(X)].
encode_boolean(undefined) -> ["false"];
encode_boolean(true) -> ["true"];
encode_boolean(false) -> ["false"].

decode_string(X,_,_,_) -> decode_string(X).
decode_int(X,_,_,_) -> decode_int(X).
decode_byte(X,_,_,_) -> decode_byte(X).
decode_short(X,_,_,_) -> decode_short(X).
decode_boolean(X,_,_,_) -> decode_boolean(X).

decode_string([]) -> [];
decode_string([X]) when is_list(X) -> X.
decode_int([X]) when is_list(X) -> list_to_integer(X);
decode_int([]) -> 0.
decode_byte([X]) when is_list(X) -> list_to_integer(X);
decode_byte([]) -> 0.
decode_short([X]) when is_list(X) -> list_to_integer(X);
decode_short([]) -> 0.
decode_boolean(["true"]) -> true;
decode_boolean(["false"]) -> false.

%%
%% Makeup:
%%
%%   Generate makeup_element description for
%%   elements defined in the schema.
%%

mk(XSD,St0,Opts) ->
    Target = XSD#xsd_schema.targetNamespace,
    Ns = attr_ns(Target, XSD#xsd_schema.xattributes ++ Opts),
    St = St0#xsd_state { tns = Target, ns  = Ns },
    io:format("Target=~p, Ns=~p\n", [Target, Ns]),
    %% scan declaraions
    lists:foreach(
      fun(E) ->
	      if is_record(E,xsd_include) ->
		      io:format("include: ~s\n", 
				[E#xsd_include.schemaLocation]);
		 is_record(E,xsd_import) ->
		      io:format("import: ~s ~s\n", 
				[E#xsd_import.namespace,
				 E#xsd_import.schemaLocation]);
		 is_record(E,xsd_redefine) ->
		      io:format("redefine: ~p\n", 
				[E#xsd_redefine.content]);
		 is_record(E,xsd_annotation) ->
		      io:format("annotation: ~p\n", 
				[E#xsd_annotation.content])
	      end
      end, XSD#xsd_schema.decl),
    %% scan elems
    {_,St1} =
	lists:foldl(
	  fun(E,{_,St1}) ->
		  if is_record(E,xsd_simpleType) ->
			  mk_simpleType(E,St1);
		     is_record(E,xsd_complexType) ->
			  mk_complexType(E,St1);
		     is_record(E,xsd_attribute) ->
			  mk_attribute(E, St1);
		     is_record(E,xsd_element) ->
			  mk_element(E, St1);
		     is_record(E,xsd_attributeGroup) ->
			  mk_attributeGroup(E,St1);
		     is_record(E,xsd_group) ->
			  mk_group(E,St1);
		     is_record(E,xsd_notation) ->
			  mk_notation(E,St1)
		  end
	  end,{undefined,St}, XSD#xsd_schema.content),
    St1.

mk_element(E, St) ->
    C = E#xsd_element.content,
    Name = E#xsd_element.name,
    if C == undefined ->
	    if E#xsd_element.name == undefined ->
		    RefName = E#xsd_element.ref,
		    NameNs = [St#xsd_state.ns|list_to_atom(RefName)],
		    {{element,min_max(E),{ref,NameNs}},St};
	       true ->
		    {Nm,St1} = add_element(Name,{ref,E#xsd_element.type}, St),
		    {{element,min_max(E),Nm}, St1}
	    end;
       is_record(C,xsd_simpleType) ->
	    {T,St1} = mk_simpleType(C,St),
	    {Nm,St2} = add_element(E#xsd_element.name, T, St1),
	    {{element,min_max(E),Nm}, St2};
       is_record(C,xsd_complexType) ->
	    {T,St1} = mk_complexType(C,St),
	    {Nm,St2} = add_element(E#xsd_element.name, T, St1),
	    {{element,min_max(E),Nm}, St2}
    end.

mk_attribute(E, St) ->
    C = E#xsd_attribute.content,
    if C == undefined ->
	    {undefined,
	     add_attribute(E#xsd_attribute.name,
			   E#xsd_attribute.type,
			   E#xsd_attribute.use,
			   E#xsd_attribute.default,
			   E#xsd_attribute.fixed,
			   E#xsd_attribute.form, St)};
       true ->
	    {T,St1} = mk_simpleType(C, St),
	    {T,
	     add_attribute(E#xsd_attribute.name,
			   T,
			   E#xsd_attribute.use,
			   E#xsd_attribute.default,
			   E#xsd_attribute.fixed,
			   E#xsd_attribute.form, St1)}
    end.


mk_attributeGroup(_E, St) ->
    %% FIXME:
    {undefined, St}.

mk_notation(_E, St) ->
    %% FIXME:
    {undefined, St}.

%%
%% restriction|list|union
%%
mk_simpleType(E,St) ->
    Name = E#xsd_simpleType.name,
    C = E#xsd_simpleType.content,
    {Type,St1} = 
	if is_record(C, xsd_restriction) ->
		mk_restriction(C, St);
	   is_record(C, xsd_list) ->
		mk_list(C, St);
	   is_record(C, xsd_union) ->
		mk_union(C, St)
	end,
    {Type, add_type(Name, Type, St1)}.

%%
%% simpleContent|complexContent|all|choice|sequence|group|(attrDecl)*
%%
mk_complexType(E,St) ->
    Name = E#xsd_complexType.name,
    C = E#xsd_complexType.content,
    {Type,St1}
	= if C == undefined ->
		  case E#xsd_complexType.abstract of
		      "true" ->
			  {{abstract,E#xsd_complexType.name},St};
		      _ ->
			  {{ref,E#xsd_complexType.name},St}
		  end;
	     is_record(C, xsd_complexContent) ->
		  mk_complexContent(C,St);
	     is_record(C, xsd_simpleContent) ->
		  mk_simpleContent(C,St);
	     is_record(C, xsd_all) ->
		  mk_all(C,St);
	     is_record(C, xsd_choice) ->
		  mk_choice(C,St);
	     is_record(C, xsd_sequence) ->
		  mk_sequence(C,St);
	     is_record(C, xsd_group) ->
		  mk_group(C,St)
	  end,
    {Type, add_type(Name, Type, St1)}.

%% Make: simple Content = restriction | extension
mk_simpleContent(E, St) ->
    C = E#xsd_simpleContent.content,
    if is_record(C, xsd_restriction) ->
	    mk_restriction(C,St);
       is_record(C, xsd_extension) ->
	    mk_extension(C, St)
    end.

%% Make complex Content = restriction | extension
mk_complexContent(E,St) ->
    C = E#xsd_complexContent.content,
    if is_record(C, xsd_restriction) ->
	    mk_restriction(C,St);
       is_record(C, xsd_extension) ->
	    mk_extension(C, St)
    end.

%% Generate sequence  E1 , E2 , E3 ... , En
mk_sequence(E, St) ->
    {Ts,St1} = 
	lists:foldl(
	  fun(C,{Ts,St1}) when is_record(C, xsd_element) ->
		  {T,St2} = mk_element(C, St1),
		  {[T|Ts], St2};
	     (C,{Ts,St1}) when is_record(C, xsd_group) ->
		  {T,St2} = mk_group(C, St1),
		  {[T|Ts], St2};
	     (C,{Ts,St1}) when is_record(C, xsd_choice) ->
		  {T,St2} = mk_choice(C, St1),
		  {[T|Ts], St2};
	     (C,{Ts,St1}) when is_record(C, xsd_sequence) ->
		  {T,St2} = mk_sequence(C, St1),
		  {[T|Ts], St2};
	     (C,{Ts,St1}) when is_record(C, xsd_any) ->
		  {T,St2} = mk_any(C, St1),
		  {[T|Ts], St2}
	  end, {[],St}, E#xsd_sequence.content),
    %% ADD id,minOccurs,maxOccurs,
    {{sequence, min_max(E), lists:reverse(Ts)}, St1}.

%% Generate choice  E1 | E2 | E3 ... | En
mk_choice(E, St) ->
    {Ts,St1} = 
	lists:foldl(
	  fun(C,{Ts,St1}) when is_record(C, xsd_element) ->
		  {T,St2} = mk_element(C, St1),
		  {[T|Ts], St2};
	     (C,{Ts,St1}) when is_record(C, xsd_group) ->
		  {T,St2} = mk_group(C, St1),
		  {[T|Ts], St2};
	     (C,{Ts,St1}) when is_record(C, xsd_any) ->
		  {T,St2} = mk_any(C, St1),
		  {[T|Ts], St2};
	     (C,{Ts,St1}) ->
		  {T,St2} = mk_cs(C, St1),
		  {[T|Ts], St2}
	  end, {[],St}, E#xsd_choice.content),
    %% ADD id,minOccurs,maxOccurs,
    {{choice,min_max(E),lists:reverse(Ts)}, St1}.

mk_group(E, St) ->
    if E#xsd_group.content == undefined ->
	    {{group,min_max(E),{ref,E#xsd_group.ref}},St};
       true ->
	    {T,St1} = mk_mgs(E#xsd_group.content, St),
	    {{group,min_max(E),T}, St1}
    end.

%% Generate all  E1 & E2 & E3 ... & En
mk_all(E, St) ->
    {Ts,St1} = 
	lists:foldl(
	  fun(C,{Ts,St1}) when is_record(C, xsd_element) ->
		  {T,St2} = mk_element(C, St1),
		  {[T|Ts], St2}
	  end, {[],St}, E#xsd_all.content),
    {{all,min_max(E),lists:reverse(Ts)}, St1}.

%% Generate: any  .  (=ANY)
mk_any(E, St) ->
    %% FIXME  minOccurs,maxOccurs,id
    {{any,min_max(E)}, St}.

%% A list type   i.e  E+ | E* 
mk_list(E, St) ->
    if E#xsd_union.content == undefined ->
	    {{list,{ref,E#xsd_list.itemType}}, St};
       true ->
	    {Type,St1} = mk_simpleType(E#xsd_list.content,St),
	    {{list,Type}, St1}
    end.

%% A union type
mk_union(E, St) ->
    if E#xsd_union.content == [] ->
	    {{union,{ref,E#xsd_union.memberTypes}}, St};
       is_list(E#xsd_union.content) ->
	    {Ts,St1} = 
		lists:foldl(
		  fun(C,{Ts,St1}) when is_record(C, xsd_simpleType) ->
			  {T,St2} = mk_simpleType(C, St1),
			  {[T|Ts], St2}
		  end,{[],St}, E#xsd_union.content),
	    {{union,lists:reverse(Ts)}, St1}
    end.

%% Generate restriction: (simpleType | mgs | group)?
%% FIXME: add atributes
mk_restriction(E, St) ->
    C = E#xsd_restriction.content,
    if C == undefined ->
	    {{restriction,E#xsd_restriction.base}, St};
       is_record(C, xsd_simpleType) ->
	    {T, St1} = mk_simpleType(C, St),
	    {{restriction,T}, St1};
       is_record(C, xsd_group) ->
	    {G, St1} = mk_group(C, St),
	    {{restriction,G}, St1};
       true ->
	    {T, St1} = mk_mgs(C, St),
	    {{restriction,T}, St1}
    end.

%% extension = (mgs | group) ?
mk_extension(E, St) ->
    C = E#xsd_extension.content,
    if C == undefined ->
	    {{extension,makeup:name_ns(E#xsd_extension.base,true),undefined},St};
       is_record(C, xsd_group) ->
	    {G, St1} = mk_group(C, St),
	    {{extension,makeup:name_ns(E#xsd_extension.base,true),G}, St1};
       true ->
	    {T, St1} = mk_mgs(C, St),
	    {{extension,makeup:name_ns(E#xsd_extension.base,true),T}, St1}
    end.


%% Generate mgs: all | choice | sequence == all | cs
mk_mgs(E,St) when is_record(E, xsd_all) ->
    mk_all(E, St);
mk_mgs(E,St) ->
    mk_cs(E,St).

%% Generate cs: choice | sequence
mk_cs(E,St) when is_record(E, xsd_choice) ->
    mk_choice(E, St);
mk_cs(E,St) when is_record(E, xsd_sequence) ->
    mk_sequence(E, St).

mk_annotation(undefined) ->
    "";
mk_annotation(C) ->
    lists:map(fun(A) when is_record(A, xsd_appinfo) ->
		case A#xsd_appinfo.info of
		    [{'#PCDATA',Data}] ->
			mk_comment(Data);
		    Xml ->
			mk_comment_xml(Xml)
		end;
	   (A) when is_record(A, xsd_documentation) ->
		case A#xsd_documentation.doc of
		    [{'#PCDATA',Data}] ->
			mk_comment(Data);
		    Xml ->
			mk_comment_xml(Xml)
		end
	end, C#xsd_annotation.content).

mk_comment(Data) ->
    lists:map(fun(Line) -> ["%% ", Line, "\n"] end,
	string:tokens(Data, "\n")).

mk_comment_xml(Data) ->
    Data1 = makeup:format(Data),
    mk_comment(lists:flatten(Data1)).

min_max(E) when is_record(E,xsd_element) ->
    min_max(E#xsd_element.minOccurs, E#xsd_element.maxOccurs);
min_max(E) when is_record(E,xsd_sequence) ->
    min_max(E#xsd_sequence.minOccurs, E#xsd_sequence.maxOccurs);
min_max(E) when is_record(E,xsd_choice) ->
    min_max(E#xsd_choice.minOccurs, E#xsd_choice.maxOccurs);
min_max(E) when is_record(E,xsd_group) ->
    min_max(E#xsd_group.minOccurs, E#xsd_group.maxOccurs);
min_max(E) when is_record(E,xsd_all) ->
    min_max(E#xsd_all.minOccurs, E#xsd_all.maxOccurs);
min_max(E) when is_record(E,xsd_any) ->
    min_max(E#xsd_any.minOccurs, E#xsd_any.maxOccurs).

min_max(undefined,undefined)   -> {1,1};
min_max("0",undefined)         -> {0,1};
min_max(undefined,"unbounded") -> {1,unbounded};
min_max("0","unbounded")       -> {0,unbounded};
min_max(Min,"unbounded")       -> {list_to_integer(Min),unbounded};
min_max(Min,Max)               -> {list_to_integer(Min),list_to_integer(Max)}.


add_type(undefined, _Type, St) ->
    St;
add_type(Name, Type, St) ->
    NameNs = [St#xsd_state.ns | list_to_atom(Name)],
    Ts = St#xsd_state.types,
    St#xsd_state { types = [{NameNs,Type} | Ts]}.

add_element(Name, Type, St) ->
    NameNs = [St#xsd_state.ns | list_to_atom(Name)],
    Es = St#xsd_state.elements,
    {NameNs,St#xsd_state { elements = [{NameNs,Type} | Es]}}.

%% FIXME: Form ? qualified | unqualified 
%%  Attribute always belong to the same namespace as the element
%%
add_attribute(undefined, _Type, _Use, _Default, _Fixed, _Form, St) ->
    St;
add_attribute(Name, Type, Use, Default, Fixed, _Form, St) ->
    NameNs = [St#xsd_state.ns | list_to_atom(Name)],
    A = 
	case Use of
	    "prohibited" ->
		if Default == undefined ->
			#makeup_attribute { name   = NameNs,
					    type   = Type,
					    status = prohibited,
					    value  = Fixed };
		   true ->
			#makeup_attribute { name   = NameNs,
					    type   = Type,
					    status = prohibited,
					    value  = Default }
		end;
	    "optional" -> 
		#makeup_attribute { name   = NameNs,
				    type   = Type,
				    status = optional,
				    value  = Default };
	    "required" -> 
		#makeup_attribute { name   = NameNs,
				    type   = Type,
				    status = required,
				    value  = Default };
	    undefined ->
		if Fixed == undefined ->
			#makeup_attribute { name = NameNs,
					    type  = Type,
					    status = default,
					    value = Default };

		   true ->
			#makeup_attribute { name  = NameNs,
					    type  = Type,
					    status = fixed,
					    value  = Default }
		end
	end,
    As = St#xsd_state.attributes,
    St#xsd_state { attributes = [A|As]}.


%% <!ELEMENT %schema; ((%include; | %import; | %redefine; | %annotation;)*,
%%                    ((%simpleType; | %complexType;
%%                      | %element; | %attribute;
%%                      | %attributeGroup; | %group;
%%                      | %notation; ),
%%                     (%annotation;)*)* )>

xsd_schema({[Ns|schema],As,Cs}) ->
    XSD = ?ATTRIBUTES(xsd_schema, As),
    {Decls,Cs1} = xsd_schema_decl(Cs,Ns,[]),
    {Elems,[]} = xsd_schema_elems(Cs1,Ns,[]),
    XSD#xsd_schema { decl = Decls, content = Elems}.

%% (%include; | %import; | %redefine; | %annotation;)*
xsd_schema_decl(Cs, Ns, Decl) ->
    case Cs of
	[{[Ns|include],As,CCs}|Cs1] ->
	    X = xsd_include(CCs,Ns,As),
	    xsd_schema_decl(Cs1,Ns,[X|Decl]);
	[{[Ns|import],As,CCs}|Cs1] ->
	    X = xsd_import(CCs,Ns,As),
	    xsd_schema_decl(Cs1,Ns,[X|Decl]);
	[{[Ns|redefine],As,CCs}|Cs1] ->
	    X = xsd_redefine(CCs,Ns,As),
	    xsd_schema_decl(Cs1,Ns,[X|Decl]);
	[{[Ns|annotation],As,CCs}|Cs1] ->
	    X = xsd_annotation(CCs,Ns,As),
	    xsd_schema_decl(Cs1,Ns,[X|Decl]);
	_ ->
	    {lists:reverse(Decl), Cs}
    end.
%%
%% ( (<simpleType> | <complexType> | <element> | <attribute> |
%%    <attributeGroup> | <group> | <notation> ), <annotation>* )*
%%
xsd_schema_elems(Cs,Ns,Elems) ->
    case Cs of
	[{[Ns|simpleType],As,CCs}|Cs1] ->
	    X = xsd_simpleType(CCs,Ns,As),
	    {A,Cs2} = xsd_annotation_list(Cs1,Ns,[]),
	    X2 = X#xsd_simpleType { post_annotation = A },
	    xsd_schema_elems(Cs2,Ns,[X2|Elems]);

	[{[Ns|complexType],As,CCs}|Cs1] ->
	    X = xsd_complexType(CCs,Ns,As),
	    {A,Cs2} = xsd_annotation_list(Cs1,Ns,[]),
	    X2 = X#xsd_complexType { post_annotation = A },
	    xsd_schema_elems(Cs2,Ns,[X2|Elems]); 

	[{[Ns|element],As,CCs}|Cs1] ->
	    X = xsd_element(CCs,Ns,As),
	    {A,Cs2} = xsd_annotation_list(Cs1,Ns,[]),
	    X2 = X#xsd_element { post_annotation = A },
	    xsd_schema_elems(Cs2,Ns,[X2|Elems]); 

	[{[Ns|attribute],As,CCs}|Cs1] ->
	    X = xsd_attribute(CCs,Ns,As),
	    {A,Cs2} = xsd_annotation_list(Cs1,Ns,[]),
	    X2 = X#xsd_attribute { post_annotation = A },
	    xsd_schema_elems(Cs2,Ns,[X2|Elems]); 

	[{[Ns|attributeGroup],As,CCs}|Cs1] ->
	    X = xsd_attributeGroup(CCs,Ns,As),
	    {A,Cs2} = xsd_annotation_list(Cs1,Ns,[]),
	    X2 = X#xsd_attributeGroup { post_annotation = A },
	    xsd_schema_elems(Cs2,Ns,[X2|Elems]); 

	[{[Ns|group],As,CCs}|Cs1] ->
	    X = xsd_group(CCs,Ns,As),
	    {A,Cs2} = xsd_annotation_list(Cs1,Ns,[]),
	    X2 = X#xsd_group { post_annotation = A },
	    xsd_schema_elems(Cs2,Ns,[X2|Elems]); 

	[{[Ns|notation],As,CCs}|Cs1] ->
	    X = xsd_notation(CCs,Ns,As),
	    {A,Cs2} = xsd_annotation_list(Cs1,Ns,[]),
	    X2 = X#xsd_notation { post_annotation = A },
	    xsd_schema_elems(Cs2,Ns,[X2|Elems]); 
	_ ->
	    {lists:reverse(Elems), Cs}
    end.
%%
%% <!ELEMENT %simpleType;
%%        ((%annotation;)?, (%restriction; | %list; | %union;))>
%%

xsd_simpleType_list(Cs,Ns) ->
    xsd_simpleType_list(Cs,Ns,[]).

xsd_simpleType_list(Cs,Ns,Acc) ->
    case Cs of
	[{[Ns|simpleType],As,CCs}|Cs1] ->
	    T = xsd_simpleType(CCs,Ns,As),
	    xsd_simpleType_list(Cs1,Ns,[T|Acc]);
	_ ->
	    {lists:reverse(Acc),Cs}
    end.

xsd_simpleType_opt(Cs,Ns) ->
    case Cs of
	[{[Ns|simpleType],As,CCs}|Cs1] ->
	    T = xsd_simpleType(CCs,Ns,As),
	    {T,Cs1};
	_ ->
	    {undefined,Cs}
    end.

xsd_simpleType(Cs,Ns,As) ->
    {A,Cs1} = xsd_annotation_opt(Cs,Ns),
    XSD = ?ATTRIBUTES(xsd_simpleType, As),
    case Cs1 of
	[{[Ns|restriction],As1,CCs}] ->
	    X = xsd_restriction(CCs,Ns,As1),
	    XSD#xsd_simpleType { annotation = A, content = X };
	[{[Ns|list],As1,CCs}] ->
	    X = xsd_list(CCs,Ns,As1),
	    XSD#xsd_simpleType { annotation = A, content = X };
	[{[Ns|union],As1,CCs}] ->
	    X = xsd_union(CCs,Ns,As1),
	    XSD#xsd_simpleType { annotation = A,content = X }
    end.
%%
%% <complexType> ::= (<annotation>?,
%%                     (<simpleContent>|<complexContent>|<particleAndAttrs>))
xsd_complexType_opt(Cs, Ns) ->
    case Cs of
	[{[Ns|complexType],As,CCs}|Cs1] ->
	    T = xsd_complexType(CCs,Ns,As),
	    {T,Cs1};
	_ ->
	    {undefined,Cs}
    end.

xsd_complexType(Cs,Ns,As) ->
    {A,Cs1} = xsd_annotation_opt(Cs,Ns),
    XSD = ?ATTRIBUTES(xsd_complexType, As),
    case Cs1 of
	[{[Ns|simpleContent],As1,Cs2}] ->
	    X = xsd_simpleContent(Cs2,Ns,As1),
	    XSD#xsd_complexType { annotation=A, content = X};

	[{[Ns|complexContent],As1,Cs2}] ->
	    X = xsd_complexContent(Cs2,Ns,As1),
	    XSD#xsd_complexType { annotation=A, content = X};

	%% ( all | choice | sequence| group ) ? , 
	%%    (attribute|attributeGroup)* (anyAttribute)?
	[{[Ns|all],As1,Cs2} | Cs3] ->
	    X = xsd_all(Cs2,Ns,As1),
	    {Decls,[]} = xsd_attrDecls(Cs3,Ns),
	    XSD#xsd_complexType { annotation=A, content = X, attrDecls = Decls};

	[{[Ns|choice],As1,Cs2} | Cs3] ->
	    X = xsd_choice(Cs2,Ns,As1),
	    {Decls,[]} = xsd_attrDecls(Cs3,Ns),
	    XSD#xsd_complexType { annotation=A, content = X, attrDecls = Decls};

	[{[Ns|sequence],As1,Cs2} | Cs3] ->
	    X = xsd_sequence(Cs2,Ns,As1),
	    {Decls,[]} = xsd_attrDecls(Cs3,Ns),
	    XSD#xsd_complexType { annotation=A, content = X, attrDecls = Decls};

	[{[Ns|group],As1,Cs2} | Cs3] ->
	    X = xsd_group(Cs2,Ns,As1),
	    {Decls,[]} = xsd_attrDecls(Cs3,Ns),
	    XSD#xsd_complexType { annotation=A, content = X,attrDecls = Decls };

	[{[Ns|attribute],_,_}|_] ->
	    {Decls,[]} = xsd_attrDecls(Cs1,Ns),
	    XSD#xsd_complexType { annotation=A,attrDecls=Decls };

	[{[Ns|attributeGroup],_,_}|_] ->
	    {Decls,[]} = xsd_attrDecls(Cs1,Ns),
	    XSD#xsd_complexType { annotation=A,attrDecls=Decls};

	[{[Ns|anyAttribute],_,_}|_] ->
	    {Decls,[]} = xsd_attrDecls(Cs1,Ns),
	    XSD#xsd_complexType { annotation=A, attrDecls=Decls };
	%% since attrDecls may be empty
	[] ->
	    XSD#xsd_complexType { annotation=A }
    end.


						%complexContent; ((%annotation;)?, (%restriction;|%extension;))>
xsd_complexContent(Cs,Ns,As) ->
    {A,Cs1} = xsd_annotation_opt(Cs,Ns),
    XSD = ?ATTRIBUTES(xsd_complexContent, As),
    case Cs1 of
	[{[Ns|restriction],As1,CCs}] ->
	    X = xsd_restriction(CCs,Ns,As1),
	    XSD#xsd_complexContent { annotation = A, content = X };
	[{[Ns|extension],As1,CCs}] ->
	    X = xsd_extension(CCs,Ns,As1),
	    XSD#xsd_complexContent { annotation = A, content = X }
    end.

						% simpleContent; ((%annotation;)?, (%restriction;|%extension;))
xsd_simpleContent(Cs,Ns,As) ->
    {A,Cs1} = xsd_annotation_opt(Cs,Ns),
    XSD = ?ATTRIBUTES(xsd_simpleContent, As),
    case Cs1 of
	[{[Ns|restriction],As1,CCs}] ->
	    X = xsd_restriction(CCs,Ns,As1),
	    XSD#xsd_simpleContent { annotation = A, content = X};
	[{[Ns|extension],As1,CCs}] ->
	    X = xsd_extension(CCs,Ns,As1),
	    XSD#xsd_simpleContent { annotation = A, content = X }
    end.
%%
%% <extension> ::=  (<annotation>?, <particleAndAttrs>)
xsd_extension(Cs,Ns,As) ->
    {A,Cs1} = xsd_annotation_opt(Cs,Ns),
    XSD = ?ATTRIBUTES(xsd_extension, As),
    case Cs1 of
	[{[Ns|all],As1,Cs2} | Cs3] ->
	    X = xsd_all(Cs2,Ns,As1),
	    {Decls,[]} = xsd_attrDecls(Cs3,Ns),
	    XSD#xsd_extension { annotation=A, content = X, attrDecls = Decls};

	[{[Ns|choice],As1,Cs2} | Cs3] ->
	    X = xsd_choice(Cs2,Ns,As1),
	    {Decls,[]} = xsd_attrDecls(Cs3,Ns),
	    XSD#xsd_extension { annotation=A, content = X, attrDecls = Decls};

	[{[Ns|sequence],As1,Cs2} | Cs3] ->
	    X = xsd_sequence(Cs2,Ns,As1),
	    {Decls,[]} = xsd_attrDecls(Cs3,Ns),
	    XSD#xsd_extension { annotation=A, content = X, attrDecls = Decls};

	[{[Ns|group],As1,Cs2} | Cs3] ->
	    X = xsd_group(Cs2,Ns,As1),
	    {Decls,[]} = xsd_attrDecls(Cs3,Ns),
	    XSD#xsd_extension { annotation=A, content = X,attrDecls = Decls };

	[{[Ns|attribute],_,_}|_] ->
	    {Decls,[]} = xsd_attrDecls(Cs1,Ns),
	    XSD#xsd_extension { annotation=A,attrDecls=Decls };

	[{[Ns|attributeGroup],_,_}|_] ->
	    {Decls,[]} = xsd_attrDecls(Cs1,Ns),
	    XSD#xsd_extension { annotation=A,attrDecls=Decls};

	[{[Ns|anyAttribute],_,_}|_] ->
	    {Decls,[]} = xsd_attrDecls(Cs1,Ns),
	    XSD#xsd_extension { annotation=A, attrDecls=Decls };
	%% since attrDecls may be empty
	[] ->
	    XSD#xsd_extension { annotation=A }
    end.

%%
%% <attrDecls> ::= (<attribute>|<attributeGroup>)*, <anyAttribute>?
%%
xsd_attrDecls(Cs,Ns) ->
    xsd_attrDecls(Cs,Ns,[]).

xsd_attrDecls(Cs,Ns,Acc) ->
    case Cs of
	[{[Ns|attribute],As,CCs}|Cs1] ->
	    A = xsd_attribute(CCs,Ns,As),
	    xsd_attrDecls(Cs1, Ns, [A|Acc]);
	[{[Ns|attributeGroup],As,CCs}|Cs1] ->
	    A = xsd_attributeGroup(CCs,Ns,As),
	    xsd_attrDecls(Cs1, Ns, [A|Acc]);
	[{[Ns|anyAttribute],As,CCs}|Cs1] ->
	    A = xsd_anyAttribute(CCs,Ns,As),
	    {lists:reverse([A|Acc]),Cs1};
	_ ->
	    {lists:reverse(Acc),Cs}
    end.
%%
%% <!ELEMENT %attribute; ((%annotation;)?, (%simpleType;)?)>
%%
xsd_attribute(Cs, Ns, As) ->
    {A,Cs1} = xsd_annotation_opt(Cs,Ns),
    {Type,[]} = xsd_simpleType_opt(Cs1,Ns),
    XSD = ?ATTRIBUTES(xsd_attribute, As),
    XSD#xsd_attribute { annotation = A, content = Type }.


%% annotation? attrDecls
xsd_attributeGroup(Cs,Ns,As) ->
    {A,Cs1} = xsd_annotation_opt(Cs,Ns),
    {Decls,[]} = xsd_attrDecls(Cs1,Ns),
    XSD = ?ATTRIBUTES(xsd_attributeGroup, As),
    XSD#xsd_attributeGroup { annotation=A, decls = Decls }.

%%
%% <!ELEMENT %element; ((%annotation;)?, (%complexType;| %simpleType;)?,
%%                     (%unique; | %key; | %keyref;)*)>
xsd_elements(Cs, Ns) ->
    xsd_elements(Cs, Ns, []).

xsd_elements(Cs, Ns, Acc) ->
    case Cs of
	[{[Ns|element],As,CCs}|Cs1] ->
	    E = xsd_element(CCs,Ns,As),
	    xsd_elements(Cs1,Ns,[E|Acc]);
	_ ->
	    {lists:reverse(Acc), Cs}
    end.

xsd_element(Cs, Ns, As) ->
    {A,Cs1} = xsd_annotation_opt(Cs,Ns),
    {T,Cs2} = case xsd_simpleType_opt(Cs1,Ns) of
		  {undefined,_} ->
		      xsd_complexType_opt(Cs1,Ns);
		  Res -> Res
	      end,
    {R,[]} = xsd_key_list(Cs2,Ns),
    XSD = ?ATTRIBUTES(xsd_element, As),
    XSD#xsd_element { annotation = A,content = T,key_list = R }.

xsd_group(Cs, Ns, As) ->
    {A,Cs1} = xsd_annotation_opt(Cs,Ns),
    {Content,[]} = xsd_mgs(Cs1,Ns),
    XSD = ?ATTRIBUTES(xsd_group, As),
    XSD#xsd_group { annotation=A, content = Content }.

%% <!ELEMENT %all; ((%annotation;)?, (%element;)*)>
xsd_all(Cs,Ns,As) ->
    {A,Cs1} = xsd_annotation_opt(Cs,Ns),
    {Es,[]} = xsd_elements(Cs1,Ns),
    XSD = ?ATTRIBUTES(xsd_all, As),
    XSD#xsd_all { annotation = A, content = Es }.

%% <!ELEMENT %choice; ((%annotation;)?, (%element;| %group;| %cs; | %any;)*)>
xsd_choice(Cs,Ns,As) ->
    {A,Cs1} = xsd_annotation_opt(Cs,Ns),
    {ChoiceList,[]} = xsd_choice_list(Cs1,Ns),
    XSD = ?ATTRIBUTES(xsd_choice, As),
    XSD#xsd_choice { annotation = A, content = ChoiceList }.

xsd_choice_list(Cs, Ns) ->
    xsd_choice_list(Cs, Ns, []).

xsd_choice_list(Cs, Ns, Acc) ->
    case Cs of
	[{[Ns|element],As,CCs}|Cs1] ->
	    Choice = xsd_element(CCs,Ns,As),
	    xsd_choice_list(Cs1, Ns, [Choice|Acc]);
	[{[Ns|group],As,CCs}|Cs1] ->
	    Choice = xsd_group(CCs,Ns,As),
	    xsd_choice_list(Cs1, Ns, [Choice|Acc]);
	[{[Ns|choice],As,CCs}|Cs1] ->
	    Choice = xsd_choice(CCs,Ns,As),
	    xsd_choice_list(Cs1, Ns, [Choice|Acc]);
	[{[Ns|sequence],As,CCs}|Cs1] ->
	    Choice = xsd_sequence(CCs,Ns,As),
	    xsd_choice_list(Cs1, Ns, [Choice|Acc]);
	[{[Ns|any],As,CCs}|Cs1] ->
	    Choice = xsd_any(CCs,Ns,As),
	    xsd_choice_list(Cs1, Ns, [Choice|Acc]);
	_ ->
	    {lists:reverse(Acc),Cs}
    end.


%% <!ELEMENT %sequence; ((%annotation;)?, (%element;| %group;| %cs; | %any;)*)>
xsd_sequence(Cs,Ns,As) ->
    {A,Cs1} = xsd_annotation_opt(Cs,Ns),
    {ChoiceList,[]} = xsd_sequence_list(Cs1,Ns),
    XSD = ?ATTRIBUTES(xsd_sequence, As),
    XSD#xsd_sequence { annotation = A, content = ChoiceList }.

xsd_sequence_list(Cs, Ns) ->
    xsd_sequence_list(Cs, Ns, []).

xsd_sequence_list(Cs, Ns, Acc) ->
    case Cs of
	[{[Ns|element],As,CCs}|Cs1] ->
	    X = xsd_element(CCs,Ns,As),
	    xsd_sequence_list(Cs1, Ns, [X|Acc]);
	[{[Ns|group],As,CCs}|Cs1] ->
	    X = xsd_group(CCs,Ns,As),
	    xsd_sequence_list(Cs1, Ns, [X|Acc]);
	[{[Ns|choice],As,CCs}|Cs1] ->
	    X = xsd_choice(CCs,Ns,As),
	    xsd_sequence_list(Cs1, Ns, [X|Acc]);
	[{[Ns|sequence],As,CCs}|Cs1] ->
	    X = xsd_sequence(CCs,Ns,As),
	    xsd_sequence_list(Cs1, Ns, [X|Acc]);
	[{[Ns|any],As,CCs}|Cs1] ->
	    X = xsd_any(CCs,Ns,As),
	    xsd_sequence_list(Cs1, Ns, [X|Acc]);
	_ ->
	    {lists:reverse(Acc),Cs}
    end.


%% <!ELEMENT %any; (%annotation;)?>
xsd_any(Cs,Ns,As) ->
    {A,[]} = xsd_annotation_opt(Cs,Ns),
    XSD = ?ATTRIBUTES(xsd_any, As),
    XSD#xsd_any { annotation = A }.
%%
%%  <anyAttribute> ::= <annotation>?
%%
xsd_anyAttribute(Cs,Ns,As) ->
    {A,[]} = xsd_annotation_opt(Cs,Ns),
    XSD = ?ATTRIBUTES(xsd_anyAttribute, As),
    XSD#xsd_anyAttribute { annotation = A }.

%%
%% <mgs> ::= <all> | <choice> | <sequence>
%%
xsd_mgs(Cs, Ns) ->
    case Cs of
	[{[Ns|all],As1,Cs1} | Cs2] ->
	    {xsd_all(Cs1,Ns,As1), Cs2};

	[{[Ns|choice],As1,Cs1} | Cs2] ->
	    {xsd_choice(Cs1,Ns,As1), Cs2};

	[{[Ns|sequence],As1,Cs1} | Cs2] ->
	    {xsd_sequence(Cs1,Ns,As1),Cs2};
	_ ->
	    {undefined,Cs}
    end.

%% choice | sequence
xsd_cs(Cs, Ns) ->
    case Cs of
	[{[Ns|choice],As1,Cs1} | Cs2] ->
	    {xsd_choice(Cs1,Ns,As1),Cs2};
	[{[Ns|sequence],As1,Cs1} | Cs2] ->
	    {xsd_sequence(Cs1,Ns,As1),Cs2};
	_ ->
	    {undefined,Cs}
    end.


xsd_key_list(Cs, Ns) ->
    xsd_key_list(Cs, Ns, []).

xsd_key_list(Cs, Ns, Acc) ->
    case Cs of
	[{[Ns|unique],As,CCs} | Cs1] ->
	    K = xsd_unique(CCs,Ns,As),
	    xsd_key_list(Cs1,Ns,[K|Acc]);
	[{[Ns|key],As,CCs} | Cs1] ->
	    K = xsd_key(CCs,Ns,As),
	    xsd_key_list(Cs1,Ns,[K|Acc]);
	[{[Ns|keyref],As,CCs} | Cs1] ->
	    K = xsd_keyref(CCs,Ns,As),
	    xsd_key_list(Cs1,Ns,[K|Acc]);
	_ ->
	    {lists:reverse(Acc), Cs}
    end.

xsd_unique(Cs,Ns,As) ->
    {A,Cs1} = xsd_annotation_opt(Cs,Ns),
    {S,Cs2} = xsd_selector_req(Cs1,Ns),
    {Fs,[]} = xsd_field_list(Cs2,Ns),
    XSD = ?ATTRIBUTES(xsd_unique, As),
    XSD#xsd_unique { annotation = A, selector = S, fields = Fs }.

xsd_key(Cs,Ns,As) ->
    {A,Cs1} = xsd_annotation_opt(Cs,Ns),
    {S,Cs2} = xsd_selector_req(Cs1,Ns),
    {Fs,[]} = xsd_field_list(Cs2,Ns),
    XSD = ?ATTRIBUTES(xsd_key, As),
    XSD#xsd_key { annotation = A, selector = S, fields = Fs }.

xsd_keyref(Cs,Ns,As) ->
    {A,Cs1} = xsd_annotation_opt(Cs,Ns),
    {S,Cs2} = xsd_selector_req(Cs1,Ns),
    {Fs,[]} = xsd_field_list(Cs2,Ns),
    XSD = ?ATTRIBUTES(xsd_keyref, As),
    XSD#xsd_keyref { annotation = A, selector = S, fields = Fs }.

xsd_selector_req(Cs1,Ns) ->
    [{[Ns|selector],As,CCs}|Cs2] = Cs1,
    {xsd_selector(CCs,Ns,As), Cs2}.

xsd_selector(Cs,Ns,As) ->
    {A,[]} = xsd_annotation_opt(Cs,Ns),
    XSD = ?ATTRIBUTES(xsd_selector, As),
    XSD#xsd_selector { annotation = A}.

xsd_field_list(Cs,Ns) ->
    xsd_field_list(Cs,Ns,[]).

xsd_field_list(Cs,Ns,Acc) ->
    case Cs of
	[{[Ns|field],As,CCs}|Cs1] ->
	    F = xsd_field(CCs,Ns,As),
	    xsd_field_list(Cs1,Ns,[F|Acc]);
	_ ->
	    {lists:reverse(Acc),Cs}
    end.

xsd_field(Cs,Ns,As) ->
    {A,[]} = xsd_annotation_opt(Cs,Ns),
    XSD = ?ATTRIBUTES(xsd_field, As),
    XSD#xsd_field { annotation = A}.

xsd_include(Cs,Ns,As) ->
    {A,[]} = xsd_annotation_opt(Cs,Ns),
    XSD = ?ATTRIBUTES(xsd_include, As),
    XSD#xsd_include { annotation = A}.


xsd_import(Cs,Ns,As) ->
    {A,[]} = xsd_annotation_opt(Cs,Ns),
    XSD = ?ATTRIBUTES(xsd_import, As),
    XSD#xsd_import { annotation = A}.


xsd_redefine(Cs,Ns,As) ->
    {ReDef,[]} = xsd_redefine_list(Cs,Ns),
    XSD = ?ATTRIBUTES(xsd_redefine, As),
    XSD#xsd_redefine { content = ReDef }.

xsd_redefine_list(Cs, Ns) ->
    xsd_redefine_list(Cs, Ns,[]).

xsd_redefine_list(Cs, Ns, Acc) ->    
    case Cs of
	[{[Ns|annotation],As,CCs}|Cs1] ->
	    X = xsd_annotation(CCs,Ns,As),
	    xsd_redefine_list(Cs1,Ns,[X|Acc]);

	[{[Ns|simpleType],As,CCs}|Cs1] ->
	    X = xsd_simpleType(CCs,Ns,As),
	    xsd_redefine_list(Cs1,Ns,[X|Acc]);

	[{[Ns|complexType],As,CCs}|Cs1] ->
	    X = xsd_complexType(CCs,Ns,As),
	    xsd_redefine_list(Cs1,Ns,[X|Acc]); 

	[{[Ns|attribute],As,CCs}|Cs1] ->
	    X = xsd_attribute(CCs,Ns,As),
	    xsd_redefine_list(Cs1,Ns,[X|Acc]); 

	[{[Ns|attributeGroup],As,CCs}|Cs1] ->
	    X = xsd_attributeGroup(CCs,Ns,As),
	    xsd_redefine_list(Cs1,Ns,[X|Acc]); 

	[{[Ns|group],As,CCs}|Cs1] ->
	    X = xsd_group(CCs,Ns,As),
	    xsd_redefine_list(Cs1,Ns,[X|Acc]);
	_ ->
	    {lists:reverse(Acc), Cs}
    end.

%% %notation; (%annotation;)?
xsd_notation(Cs,Ns,As) ->
    {A,[]} = xsd_annotation_opt(Cs,Ns),
    XSD = ?ATTRIBUTES(xsd_notation, As),
    XSD#xsd_notation { annotation = A}.

%% (annotation)* 
xsd_annotation_list(Cs, Ns, Acc) ->
    case Cs of
	[{[Ns|annotation],As,CCs}|Cs1] ->
	    X = xsd_annotation(CCs,Ns,As),
	    xsd_annotation_list(Cs1,Ns,[X|Acc]);
	_ ->
	    {lists:reverse(Acc), Cs}
    end.

%% (annotation)?
xsd_annotation_opt(Cs,Ns) ->
    case Cs of
	[{[Ns|annotation],As,CCs}|Cs1] ->
	    {xsd_annotation(CCs,Ns,As),Cs1};
	_ ->
	    {undefined,Cs}
    end.

xsd_annotation(Cs,Ns,As) ->
    {Content,[]} = xsd_annotation_1(Cs,Ns,[]),
    #xsd_annotation { ?XATTRIBUTES = As, content = Content }.

xsd_annotation_1(Cs,Ns,Acc) ->
    case Cs of
	[{[Ns|appinfo],As,Any}|Cs1] ->
	    App0 = ?ATTRIBUTES(xsd_appinfo, As),
	    App = App0#xsd_appinfo { info = Any },
	    xsd_annotation_1(Cs1,Ns,[App|Acc]);

	[{[Ns|documentation],As,Any}|Cs1] ->
	    Doc0 = ?ATTRIBUTES(xsd_documentation, As),
	    Doc = Doc0#xsd_documentation { doc = Any },
	    xsd_annotation_1(Cs1,Ns,[Doc|Acc]);
	_ ->
	    {lists:reverse(Acc),Cs}
    end.

%% <restriction> ::=  
%%   <annotation>?,(<restriction1>|(<simpleType>?,<facet>*)),<attrDecls>
%%
%% <restriction1> ::= (<mgs>|<group>)?
xsd_restriction(Cs,Ns,As) ->
    {A,Cs1} = xsd_annotation_opt(Cs,Ns),
    XSD = ?ATTRIBUTES(xsd_restriction, As),
    case Cs1 of
	%% restriction1
	[{[Ns|all],As1,CCs} | Cs2] ->
	    X = xsd_all(CCs,Ns,As1),
	    {Decls,[]} = xsd_attrDecls(Cs2,Ns),
	    XSD#xsd_restriction { annotation=A,content=X,attrDecls=Decls};

	[{[Ns|choice],As1,CCs} | Cs2] ->
	    X = xsd_choice(CCs,Ns,As1),
	    {Decls,[]} = xsd_attrDecls(Cs2,Ns),
	    XSD#xsd_restriction { annotation=A,content=X,attrDecls=Decls};

	[{[Ns|sequence],As1,CCs}|Cs2] ->
	    X = xsd_sequence(CCs,Ns,As1),
	    {Decls,[]} = xsd_attrDecls(Cs2,Ns),
	    XSD#xsd_restriction { annotation=A,content=X,attrDecls=Decls};

	[{[Ns|group],As1,CCs}|Cs2] ->
	    X = xsd_group(CCs,Ns,As1),
	    {Decls,[]} = xsd_attrDecls(Cs2,Ns),
	    XSD#xsd_restriction { annotation=A,content=X,attrDecls=Decls};

	[{[Ns|simpleType],As1,CCs}|Cs2] ->
	    X = xsd_simpleType(CCs,Ns,As1),
	    {F,Cs3} = xsd_facet_list(Cs2,Ns),
	    {Ds,[]} = xsd_attrDecls(Cs3,Ns),
	    XSD#xsd_restriction { annotation=A,content=X,facets=F,attrDecls=Ds};
	_ ->
	    {F,Cs2} = xsd_facet_list(Cs1,Ns),
	    {Ds,[]} = xsd_attrDecls(Cs2,Ns),
	    XSD#xsd_restriction { annotation=A,facets=F,attrDecls=Ds}
    end.

%% <list> ::= <annotation>?, <simpleType>?
xsd_list(Cs,Ns,As) ->
    {A,Cs1} = xsd_annotation_opt(Cs,Ns),
    XSD = ?ATTRIBUTES(xsd_list, As),
    {X,[]} = xsd_simpleType_opt(Cs1,Ns),
    XSD#xsd_list { annotation=A, content=X }.

%% <union> ::= <annotation>? ,<simpleType>*
xsd_union(Cs,Ns,As) ->
    {A,Cs1} = xsd_annotation_opt(Cs,Ns),
    XSD = ?ATTRIBUTES(xsd_union, As),
    {Xs,[]} = xsd_simpleType_list(Cs1,Ns),
    XSD#xsd_union { annotation=A, content=Xs }.


%%
%% <facet> ::= <minInclusive> | <minExclusive> |
%%             <maxInclusive> | <maxExclusive> |
%%             <totalDigits>  | <fractionDigits> |
%%            <<pattern> | <enumeration>
%%             <whiteSpace> | <length> | <maxLength> | <minLength>
%%

xsd_facet_list(Cs, Ns) ->
    xsd_facet_list(Cs, Ns, []).

xsd_facet_list(Cs, Ns, Acc) ->
    case Cs of
	[{[Ns|minInclusive],As,CCs}|Cs1] ->
	    F = xsd_facet(CCs,Ns,As,xsd_minInclusive),
	    xsd_facet_list(Cs1, Ns, [F|Acc]);
	[{[Ns|minExclusive],As,CCs}|Cs1] ->
	    F = xsd_facet(CCs,Ns,As,xsd_minExclusive),
	    xsd_facet_list(Cs1, Ns, [F|Acc]);
	[{[Ns|maxInclusive],As,CCs}|Cs1] ->
	    F = xsd_facet(CCs,Ns,As,xsd_maxInclusive),
	    xsd_facet_list(Cs1, Ns, [F|Acc]);
	[{[Ns|maxExclusive],As,CCs}|Cs1] ->
	    F = xsd_facet(CCs,Ns,As,xsd_maxExclusive),
	    xsd_facet_list(Cs1, Ns, [F|Acc]);
	[{[Ns|totalDigits],As,CCs}|Cs1] ->
	    F = xsd_facet(CCs,Ns,As,xsd_totalDigits),
	    xsd_facet_list(Cs1, Ns, [F|Acc]);
	[{[Ns|fractionDigits],As,CCs}|Cs1] ->
	    F = xsd_facet(CCs,Ns,As,xsd_fractionDigits),
	    xsd_facet_list(Cs1, Ns, [F|Acc]);
	[{[Ns|pattern],As,CCs}|Cs1] ->
	    F = xsd_facet(CCs,Ns,As,xsd_pattern),
	    xsd_facet_list(Cs1, Ns, [F|Acc]);
	[{[Ns|enumeration],As,CCs}|Cs1] ->
	    F = xsd_facet(CCs,Ns,As,xsd_enumeration),
	    xsd_facet_list(Cs1, Ns, [F|Acc]);
	[{[Ns|whiteSpace],As,CCs}|Cs1] ->
	    F = xsd_facet(CCs,Ns,As,xsd_whiteSpace),
	    xsd_facet_list(Cs1, Ns, [F|Acc]);
	[{[Ns|length],As,CCs}|Cs1] ->
	    F = xsd_facet(CCs,Ns,As,xsd_length),
	    xsd_facet_list(Cs1, Ns, [F|Acc]);
	[{[Ns|maxLength],As,CCs}|Cs1] ->
	    F = xsd_facet(CCs,Ns,As,xsd_maxLength),
	    xsd_facet_list(Cs1, Ns, [F|Acc]);
	[{[Ns|minLength],As,CCs}|Cs1] ->
	    F = xsd_facet(CCs,Ns,As,xsd_minLength),
	    xsd_facet_list(Cs1, Ns, [F|Acc]);
	_ ->
	    {lists:reverse(Acc), Cs}
    end.

xsd_facet(Cs,Ns,As,Name) ->
    {A,[]} = xsd_annotation_opt(Cs,Ns),
    XSD = ?ATTRIBUTES(xsd_facet, As),
    XSD1 = XSD#xsd_facet { annotation = A },
    setelement(1, XSD1, Name).  %% Change name

%%
%% Attrbute scanner
%%
attributes(Fields, Xsd, As) ->
    %% ?dbg("construct: ~s from ~p\n", [element(1,Xsd), As]),
    attributes(Fields, 2, Xsd, As).

attributes([?XATTRIBUTES|_], I, Xsd, As) ->
    setelement(I, Xsd, As);
attributes([F|Fs], I, Xsd, As) ->
    Spec = element(I, Xsd),
    case keysearch_and_delete(F,1,As) of
	false ->
	    case Spec of
		required ->
		    exit({required,F,element(1,Xsd)});
		implied ->
		    attributes(Fs,I+1,setelement(I,Xsd,undefined),As);
		{default,V} ->
		    attributes(Fs,I+1,setelement(I,Xsd,V),As);
		{fixed,V} ->
		    attributes(Fs,I+1,setelement(I,Xsd,V),As)
	    end;
	{value,{_,V},As1} ->
	    case Spec of
		required ->
		    attributes(Fs,I+1,setelement(I,Xsd,V),As1);
		implied ->
		    attributes(Fs,I+1,setelement(I,Xsd,V),As1);
		{default,_} ->
		    attributes(Fs,I+1,setelement(I,Xsd,V),As1);
		{fixed,V} ->
		    exit({fixed,F,element(1,Xsd)})
	    end
    end;
attributes([], _I, Xsd, _As) ->
    Xsd.

%% search and delete from list
keysearch_and_delete(Key,Pos,List) ->
    keysearch_and_delete(Key,Pos,List,[]).

keysearch_and_delete(Key,Pos,[E|Es],Acc) when element(Pos,E) == Key ->
    {value,E,lists:reverse(Acc)++Es};
keysearch_and_delete(Key,Pos,[E|Es],Acc) ->
    keysearch_and_delete(Key,Pos,Es,[E|Acc]);
keysearch_and_delete(_Key,_Pos,[],_Acc) ->
    false.

attr(Name,As) ->
    case lists:keysearch(Name,1,As) of
	{value,{_,Value}} ->
	    Value;
	false -> undefined
    end.

attr_am(Name,As) ->
    case lists:keysearch(Name,1,As) of
	{value,{_,Value}} ->
	    list_to_atom(Value);
	false -> undefined
    end.

%% Search for namespace tag given namespace.
attr_ns(Val,[{[xmlns|Ns],Val}|_]) -> Ns;
attr_ns(Val,[_|As]) -> attr_ns(Val,As);
attr_ns(_Val, []) -> undefined.
