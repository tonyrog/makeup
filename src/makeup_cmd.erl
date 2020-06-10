%%% File    : makeup_cmd.erl
%%% Author  : Tony Rogvall <tony@PBook.local>
%%% Description : Various command line tools
%%% Created : 21 Apr 2006 by Tony Rogvall <tony@PBook.local>

-module(makeup_cmd).

-import(lists, [reverse/1,map/2, foldl/3, foreach/2, member/2,append/1]).

-ifdef(debug).
-define(dbg(Fmt,As), io:format("~s: "++(Fmt),[?MODULE |(As)])).
-else.
-define(dbg(Fmt,As), ok).
-endif.

-include("../include/makeup.hrl").

-export([verify/0, verify/1]).
-export([config/0, config/1]).
-export([compile/0, compile/1]).
-export([wbxml/0,   wbxml/1]).
-export([xml2wbxml/0, xml2wbxml/1]).
-export([wbxml2xml/0, wbxml2xml/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Verify
%%  Options:
%%	-p <pubid>     PUBID
%%	-m <module>    dtd module
%%	-s <tag>       start tag
%%	-i             case insensitive
%%	-I             case sensitive
%%	-n             use namespace
%%      -v             validate document
%%	-o             pretty output
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

verify() ->
    verify_usage().

verify(Args) ->
    ?dbg("verify: Args=~p\n", [Args]),
    {File, Opts,XOpts} = verify_opts(Args, [],[]),
    ?dbg("makeup opts=~p\n", [Opts]),
    case makeup:file(File, Opts) of
	{ok,Doc} ->
	    case getopt(output,XOpts,false) of
		true ->
		    io:put_chars(makeup:format(Doc));
		false ->
		    ok
	    end,
	    halt(0);
	{error, Reason} ->
	    io:format("Error: ~p\n", [Reason]),
	    halt(1);
	Other ->
	    io:format("Error: ~p\n", [Other]),
	    halt(1)
    end.

verify_usage() ->
    io:format("usage: makeup verify [opts] <file>\n"
	      "  -p <pubid>     PUBID\n"
	      "  -m <module>    dtd module\n"
	      "  -s <tag>       start tag\n"
	      "  -i             case insensitive\n"
	      "  -I             case sensitive\n"
	      "  -n             use namespace\n"
	      "  -o             pretty output\n"
	     ),
    halt(1).

verify_opts(Args, Opts, XOpts) ->
    case Args of
	['pubid','\'\'','\'\'' | Args1] ->
	    verify_opts(Args1, Opts, XOpts);
	['pubid',ID | Args1] ->
	    ?dbg("pubid: ~p\n", [ID]),
	    PubID = arg(ID),
	    case makeup_dtd_srv:public_entry(PubID) of
		{ok,_Path,{_PUBID,_Url,PubOpts,DTD,_File}} ->
		    verify_opts(Args1, [{dtd,DTD}|Opts]++PubOpts, XOpts);
		_Error ->
		    io:format("error: pubid ~s not found\n",[PubID]),
		    halt(1)
	    end;
	['dtd', Mod | Args1] ->
	    verify_opts(Args1, [{dtd,Mod}|Opts], XOpts);
	['start',Tag | Args1] ->
	    verify_opts(Args1, [{start,Tag}|Opts], XOpts);
	['icase' | Args1] ->
	    verify_opts(Args1, [{icase,true}|Opts], XOpts);	    
	['case' | Args1] ->
	    verify_opts(Args1, [{icase,false}|Opts], XOpts);	    
	['namespace' | Args1] ->
	    verify_opts(Args1, [{ns,true}|Opts], XOpts);
	['validate' | Args1] ->
	    verify_opts(Args1, [{validate,true}|Opts], XOpts);
	['output' | Args1] ->
	    verify_opts(Args1, Opts, [{output,true}|XOpts]);
	['\'\'' | Args1] -> %% ignore special qarg (needed to pass PubID)
	    verify_opts(Args1, Opts, XOpts);
	[File] ->
	    {atom_to_list(File), reverse(Opts), XOpts};
	_ ->
	    verify_usage()
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Compile: makeup compile [options] <file>
%%   Compile a dtd or xsd file into an erlang source files
%%
%%      -o <dir>   Set output directory
%%	-m <name>  Set output module name
%%	-p <name>  Set output prefix name
%%	-i         Compile case sensitive tags (XML)
%%	-I         Compile case insensitive tags (HTML)
%%	-r         Compile to record format
%%	-a         Add attribute field to record format
%%	-n         Use XML namespace
%%
%%  file.dtd will generate file_dtd.erl [file_dtd.hrl]
%%  file.xsd will generate file_xsd.erl [file_xsd.hrl]
%%  file.wsdl will generate 
%%        <service>_cli.erl/<service>_srv.erl/<service>.hrl
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

compile() ->
    compile_usage().

compile(Args) ->
    ?dbg("compile: args=~p\n", [Args]),
    {File,Opts} = compile_opts(Args, []),
    ?dbg("compile: opts=~p\n", [Opts]),
    case filename:extension(File) of
	".dtd" ->
	    catch makeup_dtd_gen:compile(File, Opts);
	".xsd" ->
	    catch makeup_xsd:compile(File, Opts);
	".wsdl" ->
	    catch makeup_wsdl:compile(File, Opts);
	_ ->
	    %% fix me add options or default handling
	    io:format("makeup compile: error .xsd or .dtd file\n"),
	    halt(1)
    end,
    halt(0).


compile_opts(['outdir',Dir | Args], Opts) ->
    compile_opts(Args, [{outdir, atom_to_list(Dir)}|Opts]);
compile_opts(['mod', Mod | Args], Opts) ->
    compile_opts(Args, [{mod, atom_to_list(Mod)}|Opts]);
compile_opts(['out', File | Args], Opts) ->
    compile_opts(Args, [{out, atom_to_list(File)}|Opts]);
compile_opts(['outh', File | Args], Opts) ->
    compile_opts(Args, [{outh, atom_to_list(File)}|Opts]);
compile_opts(['case' | Args], Opts) ->
    compile_opts(Args, [{icase, false}|Opts]);
compile_opts(['icase' | Args], Opts) ->
    compile_opts(Args, [{icase, true}|Opts]);
compile_opts(['record' | Args], Opts) ->
    compile_opts(Args, [{record_style, true}|Opts]);
compile_opts(['namespace' | Args], Opts) ->
    compile_opts(Args, [{ns, true}|Opts]);
compile_opts(['prefix',Pfx | Args], Opts) ->
    compile_opts(Args, [{prefix, atom_to_list(Pfx)}|Opts]);
compile_opts(['attributes' | Args], Opts) ->
    compile_opts(Args, [{record_attributes,true}|Opts]);
compile_opts(['\'\'' | Args], Opts) ->
    compile_opts(Args, Opts);
compile_opts([pubid,_ID | Args], Opts) ->
    compile_opts(Args, Opts);
compile_opts([File], Opts) ->
    {atom_to_list(File), Opts};
compile_opts(_, _) ->
    compile_usage().

compile_usage() ->    
    io:format("usage: makeup compile [options] <file>\n"
	      "  -o <dir>   Set output directory\n"
	      "  -m <name>  Set output module name\n"
	      "  -p <name>  Set output prefix name\n"
	      "  -i         Compile case sensitive tags (XML)\n"
	      "  -I         Compile case insensitive tags (HTML)\n"
	      "  -r         Compile to record format\n"
	      "  -a         Add attribute field to record format\n"
	      "  -n         Use XML namespace\n"
	     ),
    halt(1).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% WBXML: makeup wbxml [options] <file>
%%   Compile a wbxml spec file into an erlang source files
%%
%%      -o <dir>   Set output directory
%%	-m <name>  Set output module name
%%	-p <name>  Set output prefix name
%%
%%  file.tab will generate [wbxml_]file.erl
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

wbxml() ->
    wbxml_usage().

wbxml(Args) ->
    ?dbg("wbxml: args=~p\n", [Args]),
    {File,Opts} = wbxml_opts(Args, []),
    ?dbg("wbxml: opts=~p\n", [Opts]),
    case filename:extension(File) of
	".tab" ->
	    catch makeup_wbxml_gen:compile(File, Opts);
	_ ->
	    %% fix me add options or default handling
	    io:format("makeup compile: error .tab expected\n"),
	    halt(1)
    end,
    halt(0).


wbxml_opts(['outdir',Dir | Args], Opts) ->
    wbxml_opts(Args, [{outdir, atom_to_list(Dir)}|Opts]);
wbxml_opts(['mod', Mod | Args], Opts) ->
    wbxml_opts(Args, [{mod, atom_to_list(Mod)}|Opts]);
wbxml_opts(['prefix',Pfx | Args], Opts) ->
    wbxml_opts(Args, [{prefix, atom_to_list(Pfx)}|Opts]);
wbxml_opts([pubid, '\'\'' | Args], Opts) ->
    wbxml_opts(Args, Opts);
wbxml_opts(['\'\'' | Args], Opts) ->
    wbxml_opts(Args, Opts);
wbxml_opts([File], Opts) ->
    {atom_to_list(File), Opts};
wbxml_opts(_, _) ->
    wbxml_usage().

wbxml_usage() ->    
    io:format("usage: makeup compile [options] <file>\n"
	      "  -o <dir>   Set output directory\n"
	      "  -m <name>  Set output module name\n"
	      "  -p <name>  Set output prefix name\n"
	     ),
    halt(1).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Config: makeup config [options] <file>
%%
%%  Emit configuration info
%%
%%   -m <pubid>     dtd module
%%   -d <pubid>     dtd file
%%   -o <pubid>     dtd options
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

config() ->
    config_usage().

config(Args) -> 
    ?dbg("config: Args=~p\n", [Args]),
    config1(Args).

config1([mod,ID | _Args]) ->
    PubID = arg(ID),
    case makeup_dtd_srv:public_mod(PubID) of
	{ok,Mod} ->
	    io:format("~s\n", [Mod]),
	    halt(0);
	_Error ->
	    halt(1)
    end;
config1([file,ID|_Args]) ->
    PubId = arg(ID),
    case makeup_dtd_srv:public_file(PubId) of
	{ok,File} ->
	    io:format("~s\n", [File]),
	    halt(0);
	_Error ->
	    halt(1)
    end;
config1([options,ID|_Args]) ->
    PubId = arg(ID),
    case makeup_dtd_srv:public_entry(PubId) of
	{ok,_Path,{_PubId,_Url,Opts,_Mod,_File}} ->
	    io:format("~s\n", 
		      [map(fun({icase,true}) -> "-I ";
			      ({icase,false}) -> "-i ";
			      (_) -> ""
			   end, Opts)]),
	    halt(0);
	_ ->
	    halt(1)
    end;
config1(_) ->
    config_usage().
    
config_usage() -> 
    io:format("usage: makeup config <opts>\n"
	      "  -m <pubid>     dtd module\n"
              "  -d <pubid>     dtd file\n"
              "  -o <pubid>     dtd options\n"),
    halt(1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% xml2wbxml
%%  Options:
%%	-p <pubid>     PUBID
%%	-m <module>    dtd module
%%      -w <module>    wbxml module [optional]
%%	-d             debug output
%%      -o             output file
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

xml2wbxml() ->
    xml2wbxml_usage().

xml2wbxml(Args) ->
    ?dbg("xml2wbxml: Args=~p\n", [Args]),
    {File,Opts,XOpts} = xml2wbxml_opts(Args, [],[]),
    case makeup:file(File, Opts) of
	{ok,Doc} ->
	    WBXMLMod = getopt(wbxml_mod, XOpts, undefined),
	    PubID = case Doc#makeup_document.doctype of
			[_Tag,"PUBLIC",ID|_] -> unquote(ID);
			_ -> getopt(pubid,XOpts,undefined)
		    end,
	    Ver = getopt(wbxml_ver, XOpts, {1,1}),
	    Encoding = if Doc#makeup_document.encoding == undefined ->
			       "utf-8";
			  true -> Doc#makeup_document.encoding
		       end,
	    Env = getopt_list(env, XOpts, []),
	    Bin = makeup_wbxml:encode(Doc#makeup_document.content,
				      [{pubid,PubID},
				       {version,Ver},
				       {charset,Encoding},
				       {env,Env},
				       {mod,WBXMLMod}]),
	    case getopt(output,XOpts,undefined) of
		undefined ->
		    io:put_chars(Bin);
		OFile ->
		    file:write_file(OFile, Bin)
	    end,
	    halt(0);
	{error, Reason} ->
	    io:format("Error: ~p\n", [Reason]),
	    halt(1);
	Other ->
	    io:format("Error: ~p\n", [Other]),
	    halt(1)
    end.

xml2wbxml_usage() ->
    io:format("usage: makeup xml2wbxml [opts] <file>\n"
	      "  -p <pubid>     PUBID\n"
	      "  -m <module>    dtd module\n"
	      "  -w <module>    wbxml module\n"
	      "  -o <file>      output file name\n"
	      "  -d             debug output\n"
	     ),
    halt(1).

xml2wbxml_opts(Args, Opts, XOpts) ->
    case Args of
	['pubid','\'\'','\'\'' | Args1] ->
	    xml2wbxml_opts(Args1, Opts, XOpts);
	['pubid',ID | Args1] ->
	    PubID = arg(ID),
	    case makeup_dtd_srv:public_mod(PubID) of
		{ok,Mod} ->
		    xml2wbxml_opts(Args1, 
				   [{dtd,Mod}|Opts],
				   [{pubid,PubID}|XOpts]);
		_Error ->
		    io:format("error: pubid ~s not found\n",[PubID]),
		    halt(1)
	    end;
	['dtd', Mod | Args1] ->
	    xml2wbxml_opts(Args1, [{dtd,Mod}|Opts], XOpts);
	['mod', Mod | Args1] ->
	    xml2wbxml_opts(Args1, Opts, [{wbxml_mod,Mod}|XOpts]);
	['ver', MajMin | Args1] ->
	    Ver = case catch string:tokens(atom_to_list(MajMin), ".") of
		      [Maj,Min] ->
			  case catch {list_to_integer(Maj), 
				      list_to_integer(Min) } of
			      {'EXIT',_} -> {1,1};
			      MM -> MM
			  end;
		      _ ->
			  {1,1}
		  end,
	    xml2wbxml_opts(Args1, Opts, [{wbxml_ver,Ver}|XOpts]);
	['output',File | Args1] ->
	    xml2wbxml_opts(Args1, Opts, [{output,atom_to_list(File)}|XOpts]);
	['env',KeyValue|Args1] ->
	    case string:tokens(atom_to_list(KeyValue),"=") of
		[Key,Value] ->
		    xml2wbxml_opts(Args1, Opts, [{env,{Key,Value}}|XOpts]);
		_ ->
		    xml2wbxml_opts(Args1, Opts, XOpts)
	    end;

	['\'\'' | Args1] -> %% ignore special qarg (needed to pass PubID)
	    xml2wbxml_opts(Args1, Opts, XOpts);
	['debug' | Args1] ->
	    xml2wbxml_opts(Args1, Opts, [{debug,true}|XOpts]);
	[File] ->
	    {atom_to_list(File), reverse(Opts), XOpts};
	_ ->
	    xml2wbxml_usage()
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% wbxml2xml
%%  Options:
%%	-p <pubid>     PUBID
%%      -w <module>    wbxml module [optional]
%%	-d             debug output
%%      -o             output file
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

wbxml2xml() ->
    wbxml2xml_usage().

wbxml2xml(Args) ->
    ?dbg("wbxml2xml: Args=~p\n", [Args]),
    {File,_Opts,XOpts} = wbxml2xml_opts(Args, [],[]),
    case file:read_file(File) of
	{ok,Bin} ->
	    WBXMLMod = getopt(wbxml_mod, XOpts, undefined),
	    Res = case getopt(debug, XOpts, false) of
		      false -> makeup_wbxml:decode(Bin, WBXMLMod);
		      true  -> makeup_wbxml:debug_decode(Bin, WBXMLMod)
		  end,
	    case Res of
		{ok,Doc} ->
		    Fmt = makeup:format(Doc, []),
		    case getopt(output,XOpts,undefined) of
			undefined ->
			    io:put_chars(Fmt);
			OFile ->
			    file:write_file(OFile, Fmt)
		    end,
		    halt(0);
		{error, Reason} ->
		    io:format("Error: ~p\n", [Reason]),
		    halt(1);
		Other ->
		    io:format("Error: ~p\n", [Other]),
		    halt(1)
	    end;
	{error,Reason} ->
	    io:format("Error: ~p\n", [Reason]),
	    halt(1)
    end.

wbxml2xml_usage() ->
    io:format("usage: makeup wbxml2xml [opts] <file>\n"
	      "  -w <module>    wbxml module\n"
	      "  -o <file>      output file name\n"
	      "  -d             debug output\n"
	     ),
    halt(1).

wbxml2xml_opts(Args, Opts, XOpts) ->
    case Args of
	['mod', Mod | Args1] ->
	    wbxml2xml_opts(Args1, Opts, [{wbxml_mod,Mod}|XOpts]);
	['output',File | Args1] ->
	    wbxml2xml_opts(Args1, Opts, [{output,atom_to_list(File)}|XOpts]);
	['debug' | Args1] ->
	    wbxml2xml_opts(Args1, Opts, [{debug,true}|XOpts]);
	['pubid','\'\'','\'\'' | Args1] ->
	    wbxml2xml_opts(Args1, Opts, XOpts);
	['pubid', _ID | Args1] ->
	    wbxml2xml_opts(Args1, Opts, XOpts);
	['\'\'' | Args1] -> %% ignore special qarg (needed to pass PubID)
	    wbxml2xml_opts(Args1, Opts, XOpts);
	[File] ->
	    {atom_to_list(File), reverse(Opts), XOpts};
	_ ->
	    wbxml2xml_usage()
    end.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Utils
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

arg(A) when is_atom(A) ->
    unquote(atom_to_list(A));
arg(Arg) when is_list(Arg) -> 
    uq(Arg).


unquote([$'|Cs]) ->
    case lists:reverse(Cs) of
	[$'|Cs1] -> uq(lists:reverse(Cs1));
	_ -> uq(Cs)
    end;    
unquote([$"|Cs]) ->
    case lists:reverse(Cs) of
	[$"|Cs1] -> uq(lists:reverse(Cs1));
	_ -> uq(Cs)
    end;
unquote(Cs) -> uq(Cs).

uq([$\\,C | Cs]) ->
    [C | uq(Cs)];
uq([C|Cs]) -> [C | uq(Cs)];
uq([]) -> [].


%% Get option value or default
getopt(Opt, [{Opt,Val}|_], _) ->
    Val;
getopt(Opt, [_|Opts], Def) ->
    getopt(Opt, Opts, Def);
getopt(_Opt, [], Def) ->
    Def.

getopt_list(Opt, Opts, Default) ->
    getopt_list(Opt, Opts, [], Default).

getopt_list(Opt, [{Opt,Value}|Opts], Acc, Default) ->
    getopt_list(Opt, Opts, [Value|Acc], Default);
getopt_list(Opt, [_|Opts], Acc, Default) ->
    getopt_list(Opt, Opts, Acc, Default);
getopt_list(_Opt, [], [], Default) ->
    Default;
getopt_list(_Opt, [], Acc, _Default) ->
    reverse(Acc).




    
