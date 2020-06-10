%%% File    : makeup_wsdl.erl
%%% Author  : Tony Rogvall <tony@iMac.local>
%%% Description : WSDL generator
%%% Created : 18 Nov 2005 by Tony Rogvall <tony@iMac.local>

-module(makeup_wsdl).

-rcsid("$Id: makeup_wsdl.erl,v 1.6 2009/05/19 19:21:56 sean Exp $\n").

-vsn("$Revision: 1.6 $ ").

-compile(export_all).

-import(lists, [reverse/1, append/1, foreach/2, map/2, foldr/3]).

-include("../include/makeup.hrl").
-include("../include/makeup_xsd.hrl").

-ifdef(debug).
-define(dbg(Fmt,As), io:format((Fmt),(As))).
-else.
-define(dbg(Fmt,As), ok).
-endif.

-record(wsdl,
	{
	  name,
	  ns,               %% namespace tag
	  tns,              %% Target name space
	  attributes,       %% definition attributes
	  types     = [],   %% Xsd - Type declarations
	  elements  = [],   %% Xsd - Element definitions
	  messages  = [],
	  portTypes = [],
	  bindings  = [],
	  services  = []
	 }).

-record(wsdl_message,
	{
	  name,
	  parts = []  %% [#wsdl_part]
	 }).

-record(wsdl_part,
	{
	  name,     %% requried
	  element,  %% optional
	  type      %% optional
	 }).

-record(wsdl_portType,
	{
	  name,
	  operations = []
	 }).

-record(wsdl_port_operation,
	{
	  name,     %% operation name
	  input,    %% message name
	  output,   %% message name
	  fault     %% message name
	 }).

-record(wsdl_binding,
	{
	  name,          %% Binding name
	  type,          %% PortType name
	  binding,       %% SoapBinding
	  operations=[]  %% [#wsdl_bind_operation]
	 }).

-record(wsdl_bind_operation,
	{
	  name,         %% same as wsdl_port_operation name
	  action,       %% SoapAction
	  input,        %% SoapBody
	  output,       %% SoapBody
	  fault         %% 
	 }).

-record(wsdl_service,
	{
	  name,
	  ports = []
	 }).

-record(wsdl_port,
	{
	  name,      %% portType name
	  binding,   %% bidning name
	  address    %% soap address...
	 }).

file(File) ->
    makeup:file(File, [{space,normalize},
		       {ns, true},
		       {start, definitions}
		      ]).

compile(File) ->
    compile(File,[]).

compile(File,Opts) ->
    case file(File) of
	{ok, Doc} ->
	    [XML] = Doc#makeup_document.content,
	    WSDL = wsdl(XML),
	    io:format("WSDL=~p\n", [WSDL]),
	    generate_cli(WSDL, Opts);
	Error ->
	    Error
    end.

wsdl({[wsdl|T],As,Defs}) when is_atom(T) ->
    wsdl({T,As,Defs});

wsdl({definitions,As,Defs}) ->
    Target = attr(targetNamespace,As),
    Ns = attr_ns(Target, As),
    NsName = case attr_am(name,As) of
		 undefined -> undefined;
		 Name -> [Ns|Name]
	     end,
    wsdl_defs(Defs, #wsdl { name=NsName, ns=Ns, tns=Target, attributes=As }).

wsdl_defs([{[wsdl|T],As,X}|Defs], WSDL) when is_atom(T) ->
    wsdl_defs([{T,As,X}|Defs], WSDL);

wsdl_defs([{types,_,Schemas0}|Defs], WSDL) when is_list(Schemas0) ->

    AddNsFun = fun({Part0,As,Def} = _P, Ns, F) ->
		       Part = if is_atom(Part0) -> [Ns|Part0];
				 true -> Part0
			      end,
		       {Part,As,map(fun(D) -> F(D,Ns,F) end,Def)}
	       end,
    Schemas = lists:map(fun({_,As,_} = Schema) -> case lists:keysearch(xmlns,1,As) of
						      {value,{_,StrNs}} ->
							  case lists:keysearch(StrNs,2,WSDL#wsdl.attributes) of
							      {value, {[xmlns|Ns],_}} -> AddNsFun(Schema,Ns,AddNsFun);
							      _ -> Schema
							  end;
						      _ -> Schema
						  end
			end, Schemas0),
    {Types, Elements} = lists:foldl(fun(XML,{AT,AE}) -> {ok, {RT,RE}} = makeup_xsd:xml_makeup(XML, WSDL#wsdl.attributes), {AT++RT,AE++RE} end, {[],[]},Schemas),
    Es = WSDL#wsdl.elements,
    Ts = WSDL#wsdl.types,
    wsdl_defs(Defs, WSDL#wsdl { elements = Es ++ Elements,
				types    = Ts ++ Types });
wsdl_defs([{import,As,_}|Defs], WSDL) ->
    NameSpace = attr(namespace, As),
    Location  = attr(location, As),
    Opts = [{[xmlns|attr_ns(NameSpace, WSDL#wsdl.attributes)], NameSpace}],
    io:format("Opts=~p\n", [Opts]),
    {ok,{Types,Elements}} = makeup_xsd:load(Location,Opts),
    Es = WSDL#wsdl.elements,
    Ts = WSDL#wsdl.types,
    wsdl_defs(Defs, WSDL#wsdl { elements = Es ++ Elements,
				types    = Ts ++ Types });
wsdl_defs([{message,As,Ms0}|Defs], WSDL) ->
    Ms = map(fun({[wsdl|part],As1,PDefs}) -> {part,As1,PDefs}; (X) -> X end, Ms0),
    Parts = map(fun({part,As1,_}) ->
			#wsdl_part { name=attr(name,As1),
				     element=attr(element,As1),
				     type=attr(type,As1)}
		end, Ms),
    Name = attr_am(name,As),
    Message = #wsdl_message { name=[WSDL#wsdl.ns|Name],
			      parts=Parts },
    Messages = WSDL#wsdl.messages ++ [Message],
    wsdl_defs(Defs, WSDL#wsdl { messages = Messages});
wsdl_defs([{portType,As,Operations}|Defs], WSDL) ->
    Ops = wsdl_port_operations(Operations,WSDL,[]),
    Name = attr_am(name,As),
    PortType = #wsdl_portType { name=[WSDL#wsdl.ns|Name],
				operations=Ops},
    PortTypes = WSDL#wsdl.portTypes ++	[PortType],
    wsdl_defs(Defs, WSDL#wsdl { portTypes = PortTypes}); 
wsdl_defs([{binding,As,Operations}|Defs], WSDL) ->
    {Bind,Ops} = wsdl_bind_operations(Operations,undefined,WSDL,[]),
    Name = attr_am(name,As),
    Binding = #wsdl_binding { name=[WSDL#wsdl.ns|Name],
			      type=attr(type,As),
			      binding=Bind,
			      operations=Ops },
    Bindings = WSDL#wsdl.bindings ++[Binding],
    wsdl_defs(Defs, WSDL#wsdl { bindings = Bindings });
wsdl_defs([{service,As,Serv0}|Defs], WSDL) ->
    Serv = map(fun({[wsdl|port],As1,Cs}) -> {port,As1,Cs}; (X) -> X end, Serv0),
    Ports = foldr(fun({port,As1,Cs},Acc) ->
			  Name1 = attr_am(name,As1),
			  [#wsdl_port { name=[WSDL#wsdl.ns|Name1],
					binding=attr(binding,As1),
					address=Cs } | Acc];
		     ({documentation,_,_},Acc) ->
			  Acc
		  end, [], Serv),
    Name = attr_am(name, As),
    Services = WSDL#wsdl.services ++
	[#wsdl_service { name=[WSDL#wsdl.ns|Name],
			 ports=Ports }],
    wsdl_defs(Defs, WSDL#wsdl { services = Services });
wsdl_defs([{documentation,_,_}|Defs], WSDL) ->
    wsdl_defs(Defs, WSDL);
wsdl_defs([Unknown|Defs], WSDL) ->
    io:format("Unknown def ~p\n", [Unknown]),
    wsdl_defs(Defs, WSDL);
wsdl_defs([], WSDL) ->    
    WSDL.

wsdl_port_operations([{[wsdl|Op],As,Cs}|Ops],WSDL,Acc) ->
    wsdl_port_operations([{Op,As,Cs}|Ops],WSDL,Acc);
wsdl_port_operations([{operation,As,Cs}|Ops],WSDL,Acc) ->
    {Input,Output,Fault} = wsdl_port_op(Cs,undefined,undefined,undefined),
    Name = attr_am(name,As),
    Op = #wsdl_port_operation { name=[WSDL#wsdl.ns|Name],
				input = opt_name_ns(Input),
				output = opt_name_ns(Output),
				fault = opt_name_ns(Fault)
			       },
    wsdl_port_operations(Ops,WSDL,[Op|Acc]);
wsdl_port_operations([{documentation,_,_}|Ops],WSDL,Acc) ->
    wsdl_port_operations(Ops,WSDL,Acc);
wsdl_port_operations([Op|Ops],WSDL,Acc) ->
    io:format("OPERATION? ~p\n", [Op]),
    wsdl_port_operations(Ops,WSDL,Acc);
wsdl_port_operations([],_WSDL,Acc) ->
    reverse(Acc).

wsdl_port_op([{[wsdl|Op0],As,_Cs}|Op],_Input,Output,Fault) ->
    wsdl_port_op([{Op0,As,_Cs}|Op],_Input,Output,Fault);
wsdl_port_op([{input,As,_Cs}|Op],_Input,Output,Fault) ->
    wsdl_port_op(Op,attr(message,As),Output,Fault);
wsdl_port_op([{output,As,_Cs}|Op],Input,_Output,Fault) ->		       
    wsdl_port_op(Op,Input,attr(message,As),Fault);
wsdl_port_op([{fault,As,_Cs}|Op],Input,Output,_Fault) ->
    %% FIXME: name attribute
    wsdl_port_op(Op,Input,Output,attr(message,As));
wsdl_port_op([{documentation,_,_}|Op],Input,Output,Fault) ->
    wsdl_port_op(Op,Input,Output,Fault);
wsdl_port_op([],Input,Output,Fault) ->
    {Input,Output,Fault}.

wsdl_bind_operations([{[wsdl|Op],As,Cs}|Ops],Binding,WSDL,Acc) ->
    wsdl_bind_operations([{Op,As,Cs}|Ops],Binding,WSDL,Acc);
wsdl_bind_operations([{operation,As,Cs}|Ops],Binding,WSDL,Acc) ->
    {Action,Input,Output,Fault} = wsdl_bind_op(Cs,undefined,undefined,
					       undefined,undefined),
    Name = attr_am(name,As),
    wsdl_bind_operations(Ops,
			 Binding,WSDL,
			 [#wsdl_bind_operation { name=[WSDL#wsdl.ns|Name],
						 action=Action,
						 input = Input,
						 output = Output,
						 fault = Fault
						} |
			  Acc]);
wsdl_bind_operations([{documentation,_,_}|Ops],Binding,WSDL,Acc) ->
    wsdl_bind_operations(Ops,Binding,WSDL,Acc);
wsdl_bind_operations([{Tag=[_|binding],As,Cs}|Ops],_Binding,WSDL,Acc) ->
    wsdl_bind_operations(Ops,{Tag,As,Cs},WSDL,Acc);
wsdl_bind_operations([Op|Ops],Binding,WSDL,Acc) ->
    io:format("OPERATION? ~p\n", [Op]),
    wsdl_bind_operations(Ops,Binding,WSDL,Acc);
wsdl_bind_operations([],Binding,_WSDL,Acc) ->
    {Binding,reverse(Acc)}.

wsdl_bind_op([{[wsdl|Op],As,Input}|Cs],Action,_Input,Output,Fault) ->   
    wsdl_bind_op([{Op,As,Input}|Cs],Action,_Input,Output,Fault);
wsdl_bind_op([{input,_,Input}|Cs],Action,_Input,Output,Fault) ->   
    wsdl_bind_op(Cs,Action,Input,Output,Fault);
wsdl_bind_op([{output,_,Output}|Cs],Action,Input,_Output,Fault) ->
    wsdl_bind_op(Cs,Action,Input,Output,Fault);
wsdl_bind_op([{fault,_,Fault}|Cs],Action,Input,Output,_Fault) ->
    wsdl_bind_op(Cs,Action,Input,Output,Fault);
wsdl_bind_op([{Tag=[_|operation],As,Cs1}|Cs],_Action,Input,Output,Fault) ->
    wsdl_bind_op(Cs,{Tag,As,Cs1},Input,Output,Fault);
wsdl_bind_op([{documentation,_,_}|Cs],Action,Input,Output,Fault) ->
    wsdl_bind_op(Cs,Action,Input,Output,Fault);
wsdl_bind_op([],Action,Input,Output,Fault) ->
    {Action,Input,Output,Fault}.

opt_name_ns(undefined) -> undefined;
opt_name_ns(Name) -> makeup:name_ns(Name,true).


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


attr_ns(Val,[{[xmlns|Ns],Val}|_]) -> Ns;
attr_ns(Val,[_|As]) -> attr_ns(Val,As);
attr_ns(_Val, []) -> undefined.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Generate client side call interface
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

generate_cli(WSDL, Opts) ->

    HrlFileName = case WSDL#wsdl.name of
		      [_|WsdlName] -> lists:concat([WsdlName,".hrl"]);
		      undefined -> "webservice_1.hrl"
		  end,

    {ok,HrlFd} = file:open(HrlFileName, [write]),
	{GenTypes,GenElements} = gen_coder_type_elements(WSDL),
    HrlRes = makeup_xsd:gen_hrl(HrlFd, GenTypes,Opts),
    io:format("Wrote file:~s:~p\n",[HrlFileName,HrlRes]),
    file:close(HrlFd),
	io:format("gen coders types: ~p\n~p\n",[WSDL#wsdl.types,WSDL#wsdl.elements]),
	io:format("coder elements~p\n",[gen_coder_type_elements(WSDL)]),
    _CodeRes = makeup_xsd:gen_coders(HrlFileName,GenTypes,GenElements,WSDL#wsdl.attributes,Opts),
    foreach(
      fun(Service) ->
	      [_|ServiceName] = Service#wsdl_service.name,
	      Mod = lists:concat([ServiceName,"_ports"]),
	      FileName = Mod++".erl",
	      {ok,Fd} = file:open(FileName, [write]),
	      io:format(Fd, "-module(~p).\n\n", [list_to_atom(Mod)]),
	      io:format(Fd, "-include_lib(\"soap/include/soap.hrl\").\n\n",[]),
	      io:format(Fd, "-compile(export_all).\n\n", []),
	      io:format(Fd,
			"find_rec(Rec, Data) ->\n"
			"    lists:foldl(fun({[_|R],_,_}=Res,Acc) when R == Rec -> Res;\n"
			"            ({R,_,_}=Res,Acc) when R == Rec -> Res;\n"
			"            (_,Acc) -> Acc end, undefined, Data).\n\n",[]),
	      io:format(Fd,ns2mod(WSDL#wsdl.attributes,[]),[]),
	      Res = foreach(
		      fun(Port) ->
			      gen_cli_port(Fd, Port, WSDL, Opts)
		      end, Service#wsdl_service.ports),
	      file:close(Fd),
	      io:format("Wrote file:~s:~p\n",[FileName,Res]),
	      Res
      end,
      WSDL#wsdl.services).

gen_cli_port(Fd, Port, WSDL, Opts) ->
    BindingName = makeup:name_ns(Port#wsdl_port.binding,true),
    case lists:keysearch(BindingName,
			 #wsdl_binding.name,
			 WSDL#wsdl.bindings) of
	{value,Binding} ->
	    PortName = makeup:name_ns(Binding#wsdl_binding.type, true),
	    case lists:keysearch(PortName,
				 #wsdl_portType.name,
				 WSDL#wsdl.portTypes) of
		{value,PortType} ->
		    gen_cli_port(Fd, Port, PortType, Binding, WSDL, Opts);
		false ->
		    io:format("Port ~s not found\n",
			      [Port#wsdl_port.name])
	    end;
	false ->
	    io:format("Binding ~s not found\n",
		      [Port#wsdl_port.binding])
    end.


gen_cli_port(Fd, Port, PortType, _Binding, WSDL, Opts) ->
    [{[_|address],As,_}] = Port#wsdl_port.address,
    {Location,Args0} = case attr(location,Opts) of
			   undefined -> {["\"",attr(location,As),"\""],[]};
			   dynamic -> {"Url",[{none,none,"Url"}]};
			   L -> {["\"",L,"\""],[]}
		       end,
    io:format("Location=~p\n", [Location]),
    foreach(
      fun(#wsdl_port_operation{name=[_|Name], 
			       input =Input,
			       output =Output}) ->
	      io:format(Fd, "%%\n%% ~p : ~p => ~p\n%%\n", 
			[Name,
			 makeup:format_tag_name(Input),
			 makeup:format_tag_name(Output)]),
	      case {lists:keysearch(Input, #wsdl_message.name, WSDL#wsdl.messages),
		    lists:keysearch(Output, #wsdl_message.name, WSDL#wsdl.messages)} of
		  {{value,Message},{value,Response}} ->
		      io:format("Input=~p Outpu=~p\n", [Message,Response]),
		      Args = 
			  map(
			    fun(#wsdl_part {name=N,type=undefined,element=E}) ->
				    T = element_type(E,WSDL),
				    {N,T,erlify(N)};
			       (#wsdl_part {name=N,type=T,element=_E}) ->
				    T1 = makeup:name_ns(T,true),
				    {N,T1,erlify(N)}
			    end, Message#wsdl_message.parts),
		      io:format(Fd, "~p(", [Name]),
		      ArgList = map_arg(fun({_,_,A}) -> A end, ",", Args0 ++ Args),
		      io:format(Fd, "~s) ->\n", [ArgList]),
		      io:format("ArgList=~p\n", [ArgList]),
		      Body = soap_body(Name,Args,WSDL#wsdl.tns,WSDL#wsdl.attributes),
		      ResultHandler = result_handler(Response,"ResultBody",["EnvAttrs","fun ns2mod/1"]),
		      io:format("Body=~p\n", [Body]),
		      io:format(Fd, 
				"  SoapMsg =\n"
				"     #soap_msg { ns=\"SOAP-ENV\","
				"body="++Body++"},\n"
				"  SoapReq = \n"
				"     #soap_req { url="++Location++
				", msg=SoapMsg,opts=[split_ns] },\n"
				"  Res = soap:rpc(SoapReq),\n"
				"  case Res of\n"
				"  {ok, #soap_resp{msg = #soap_msg{body=ResultBody,env_attrs=EnvAttrs}}} -> \n" ++ ResultHandler ++ ";\n"
				"  Error -> Error\n"
				"  end.\n"
				" ", []);
		  _ ->
		      io:format("In/Out Message ~p:~p not found\n", [Input,Output])
	      end
      end, PortType#wsdl_portType.operations).

gen_coder_type_elements(#wsdl{types = Types, elements = Elements, messages = Msgs}) ->
	Parts = lists:flatten([Parts || #wsdl_message{parts = Parts} <- Msgs]),
	PartElements = [makeup:name_ns(E,true) || #wsdl_part{element = E} <- Parts, E =/= undefined],
	ETypes = lists:filter(fun(X) when element(1,element(2,X)) == ref -> false;({N,_}) -> lists:member(N,PartElements) end, Elements),
	GenElements = lists:filter(fun(X) -> not lists:member(X,ETypes) end, Elements),
	{Types ++ ETypes,GenElements}.

soap_body(Name,Args,TargetNs,Attrs0) ->
    Attrs = case lists:keymember([xmlns|xsi],1,Attrs0) of
		false -> [{[xmlns|xsi],"http://www.w3.org/2001/XMLSchema-instance"}|Attrs0];
		_ -> Attrs0
	    end,
    ["[{","'ns1:",atom_to_list(Name),"',[",
     "{'xmlns:ns1',\"",TargetNs,"\"},",
     "{'SOAP-ENV:encodingStyle',\"http://schemas.xmlsoap.org/soap/encoding/\"},",
     soap_ns(TargetNs,Attrs,[]), %% Would be nice if we could put this in the envelope attributes
     "],",
     "[",
     map_arg(
       fun({N,T,A}) ->
	       make_arg(N,T,A)
       end, ",", Args), "]}]"].

ns2mod([],Acc) -> lists:reverse([".\n"|map_arg(fun(A) -> A end, ";\n",Acc)]);
ns2mod([{[xmlns|Ns],Uri}|T],Acc) -> ns2mod(T,[["ns2mod(\"",Uri,"\") -> makeup_", atom_to_list(Ns)]|Acc]);
ns2mod([_|T],Acc) -> ns2mod(T,Acc).

soap_ns(_TargetNs,[],Acc) -> map_arg(fun(A) -> A end, ",",lists:reverse(Acc));
soap_ns(TargetNs,[{[xmlns|_Ns],TargetNs}|T],Acc) -> soap_ns(TargetNs,T,Acc);
soap_ns(TargetNs,[{[xmlns|Ns],OtherNs}|T],Acc) -> soap_ns(TargetNs,T,[["{'xmlns:",atom_to_list(Ns),"',\"",OtherNs,"\"}"]|Acc]);
soap_ns(TargetNs,[_H|T],Acc) -> soap_ns(TargetNs,T,Acc).

result_handler(#wsdl_message{parts = []},_,_) -> ["none"];
result_handler(#wsdl_message{parts = [#wsdl_part{type = T, element = E}=Part]}=Msg,ResultName,ExtraAttrs) when T == undefined ->
	result_handler(Msg#wsdl_message{parts = [Part#wsdl_part{type = E}]},ResultName,ExtraAttrs);
result_handler(#wsdl_message{name = [_Ns|MsgName], parts = [#wsdl_part{name = Name, type = T}=Part]},ResultName,ExtraAttrs) ->
	io:format("result_handler ~p\n",[Part]),
	FindMsg = ["    {_,_,Message} = find_rec(",atom_to_list(MsgName),",",ResultName,"),\n"],
    FindPart = ["   {_,Attr,Part} = find_rec(",Name,",Message),\n"],
    Lookup = ["    DecData = case lists:keysearch(href,1,Attr) of\n",
	      "        {value, {_,[$#|ID]}} -> lists:filter(fun({_,A,_}) -> lists:member({id,ID},A) end,", ResultName, ");\n",
	      "        _ -> Part\n"
	      "    end,\n",
	      "    ", result_handler_make(makeup:name_ns(T,true),map_arg(fun(A) -> A end, ",", ["DecData",ResultName|ExtraAttrs]))],
    FindMsg ++ FindPart ++ Lookup.

result_handler_make([Ns|Rec],ResultName) ->
    ["makeup_",atom_to_list(Ns),":decode_",atom_to_list(Rec),"(",ResultName,")"].

make_arg(Name,Type,Arg) ->
    TypeName = makeup:format_tag_name(Type),
    ["{",Name,",[{'xsi:type',\"",TypeName,"\"}], ",type_convert(Type,Arg),"}"].

type_convert(Type, A) ->
    case Type of
	[xsd|T] -> %% FIXME: MUST check xsd name tag is correct
	    ["makeup_xsd:encode_",atom_to_list(T),"(",A,")"];
	[Ns|T] ->
	    ["makeup_",atom_to_list(Ns),":encode_",atom_to_list(T),"(",A,")"]
    end.


element_type(ElementName, WSDL) ->
    Name = makeup:name_ns(ElementName, true),
    case lists:keysearch(Name, 1, WSDL#wsdl.elements) of
	false ->
	    Name;
	{_,Type=[_|_]} ->
	    Type;
	{_,_ComplexType} ->
	    Name
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Generate server side stub
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
generate_srv(WSDL) ->
    foreach(
      fun(Service) ->
	      [_|ServiceName] = Service#wsdl_service.name,
	      Mod = lists:concat([ServiceName,"_srv"]),
	      FileName = Mod++".erl",
	      {ok,Fd} = file:open(FileName, [write]),
	      io:format(Fd, "-module(~p).\n\n", [list_to_atom(Mod)]),
	      Res = foreach(
		      fun(Port) ->
			      gen_srv_port(Fd, Port, WSDL)
		      end, Service#wsdl_service.ports),
	      file:close(Fd),
	      Res
      end,
      WSDL#wsdl.services).

gen_srv_port(Fd, Port, WSDL) ->
    case lists:keysearch(Port#wsdl_port.name,
			 #wsdl_portType.name,
			 WSDL#wsdl.portTypes) of
	{value,PortType} ->
	    case lists:keysearch(Port#wsdl_port.binding,
				 #wsdl_binding.name,
				 WSDL#wsdl.bindings) of
		{value,Binding} ->
		    gen_srv_port(Fd, PortType, Binding, WSDL);
		false ->
		    io:format("Binding ~s not found\n",
			      [Port#wsdl_port.binding])
	    end;
	false ->
	    io:format("Port ~s not found\n",
		      [Port#wsdl_port.name])
    end.



gen_srv_port(_Fd, _PortType, _Binding, _WSDL) ->
    ok.


map_arg(Fun, _Sep, [H]) ->
    [Fun(H)];
map_arg(Fun, Sep, [H|T]) ->
    [Fun(H),Sep | map_arg(Fun,Sep,T)];
map_arg(_Fun,_Sep,[]) ->
    [].

erlify([C|Cs]) ->
    if  C >= $A, C=<$Z ->
	    [C|erlify1(Cs)];
	C >= $a, C=<$z ->
	    [(C-$a)+$A | erlify1(Cs)];
	true ->
	    [$X|erlify1(Cs)]
    end.

erlify1([C|Cs]) ->
    if C >= $a, C =< $z ->   [C|erlify1(Cs)];
       C >= $A, C =< $Z ->   [C|erlify1(Cs)];
       C >= $0, C =< $9 ->   [C|erlify1(Cs)];
       C == $_ ->            [C|erlify1(Cs)];
       true -> [$_|erlify1(Cs)]
    end;
erlify1([]) ->
    [].
