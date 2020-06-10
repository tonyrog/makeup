%%% File    : makeup.erl
%%% Author  : Tony Rogvall <tony@localhost.localdomain>
%%% Description : Reetrant tag /attribute scanner
%%% Created : 16 Feb 2004 by Tony Rogvall <tony@localhost.localdomain>

-module(makeup).

-compile(export_all).

-export([file/1, file/2, file/3]).
-export([ifile/2, istring/2, ioptions/2, ilevel/0]).
-export([string/1, string/2, string/3]).
-export([search/2, match/2]).

-export([format/1,format/2]).
-export([format_tag_name/1]).
-export([format_attr_name/1]).
-export([keyval/1]).

-import(lists, [reverse/1,map/2, foldl/3, foreach/2, member/2,append/1]).

-include("../include/makeup.hrl").

-define(dbgi(Fmt,As), io:format((Fmt),(As))).

-ifdef(debug).
-define(dbg(Fmt,As), io:format((Fmt),(As))).
-else.
-define(dbg(Fmt,As), ok).
-endif.

-define(DEFAULT_CBMOD, makeup_tags).

%%
%% Perf / check parser performance
%%
perf(File) ->
    perf(File,[]).
    
perf(File,Opts) ->
    {ok,Bin} = file:read_file(File),
    T0 = now(),
    {ok,_Doc} = makeup_scan:string(Bin,Opts),
    T1 = now(),
    {ok,timer:now_diff(T1,T0)}.

perff(File) ->
    perff(File,[]).

perff(File,Opts) ->
    T0 = now(),
    {ok,_Doc} = makeup_scan:file(File,Opts),
    T1 = now(),
    {ok,timer:now_diff(T1,T0)}.

%%
%% @spec (Name::filename()) -> ok
%% @equiv file(Name, makeup_tags, [])
%% @doc Makeup a file.
%%
file(File) ->
    makeup_scan:file(File,?DEFAULT_CBMOD, []).

%%
%% @spec (Name::filename(),CbMod::atom()) -> ok
%% @equiv file(Name, CbMod, [])
%% @doc Makeup a file using a custom callback module.
%%
file(File, CbMod) when is_atom(CbMod) ->
    makeup_scan:file(File,CbMod,[]);
file(File, Opts) when is_list(Opts) ->
    makeup_scan:file(File,?DEFAULT_CBMOD,Opts).

%% @spec file(filename(), callback(), option_list()) ->
%%   {ok,Document} | {error,Reason}
%%
%% @type filename() = //kernel/file:filename()
%% @type callback() = atom()
%% @type option_list() = [term()]
%%
%% @doc Reads a marked up file return a makeup_document
%%
%% <p>Options:
%% <dl>
%%  <dt>{@type {icase, boolean()@}}
%%  </dt>
%%  <dd>Ignore case. Specifies the cases sensitivity. This is handy when 
%%      scanning html files. Default is false since XML is the
%%      normal input.
%%  </dd>
%%  <dt>{@type {ins, boolean()@}}
%%  </dt>
%%  <dd>Ignore namespace. When ins is true then namespace will be
%%      stripped. A tag foo:bar will be return as bar.
%%  </dd>
%%  <dt>{@type {ns, boolean()@}}
%%  <dd>Use namespace. If ns is true the a namespace tag foo:bar is
%%      return as [foo|bar] otherwise it is return as 'foo:bar'. 
%%  </dd>
%%  </dt>
%%  <dt>{@type {encoding, string()@}}
%%    <dd>Set the character encoding used. Default is to automatically
%%        detect the encoding by checking the first bytes
%%        in the input. The auto detection scheme will find endian,
%%        utf-16/utf-32 and defaults to utf-8. The makeup_tags callback
%%        module (default) will also be used to detect 
%%        encoding found in xml header.
%%    </dd>
%% </dt>
%%  <dt>{@type {file, string()@}}
%%     <dd>Set the filename used in fault reporting. This can be usefule
%%         when the input filename is not the correct source file name.
%%     </dd>
%%  </dt>
%%
%%  <dt>{@type {chunk, integer()@}}
%%    <dd>Set scanner read chunk size. The default is 512</dd>
%%  </dt>
%%  <dt>{@type {line, integer()@}}
%%    <dd>Assign initial line number then the default 1</dd>
%%  </dt>
%% </dl></p>
%%

file(File,CbMod,Opts) ->
    makeup_scan:file(File,CbMod,Opts).

%%
%% Include a file
%%
ifile(File, CState) ->
    makeup_scan:ifile(File, CState).

%%
%% Scan String/Binary data
%%

string(Data) ->
    makeup_scan:string(Data,?DEFAULT_CBMOD,[]).

string(Data, CbMod) when is_atom(CbMod) ->
    makeup_scan:string(Data,CbMod,[]);
string(Data, Opts) when is_list(Opts) ->
    makeup_scan:string(Data,?DEFAULT_CBMOD,Opts).

string(Data,CbMod,Opts) ->
    makeup_scan:string(Data,CbMod,Opts).

search(Search, Xml) ->
    makeup_xpath:search(Search, Xml).

match(Pattern, Xml) ->
    makeup_xpath:match(Pattern, Xml).

%%
%% Include a string/binary
%%
istring(String, CState) ->
    makeup_scan:istring(String, CState).

%%
%% Called with client state, update options
%%
ioptions(Opts, CState) ->
    makeup_scan:ioptions(Opts, CState).

%%
%% Check the recursive call level
%%
ilevel() ->
    makeup_scan:ilevel().

%%
%% Formatting
%%
format(Xml) ->
    makeup_fmt:format(Xml).

format(Xml,Opts) ->
    makeup_fmt:format(Xml,Opts).

format_tag_name(Name) ->
    makeup_fmt:format_tag_name(Name).

format_attr_name(Name) ->
    makeup_fmt:format_attr_name(Name).

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

%% Split String (Key[=Value])* into {Key,Value} | Key
keyval(String) ->
    keyval(String, []).

keyval(Cs0, Acc) ->
    case drop_blanks(Cs0) of
	"" -> 
	    reverse(Acc);
	Cs1 ->
	    {Name,Cs2} = split_word(Cs1,[]),
	    Key = list_to_atom(Name),
	    case drop_blanks(Cs2) of
		[$=|Cs3] ->
		    case drop_blanks(Cs3) of
			[?QUOT|Cs4] ->
			    {Value,Cs5} = split_string(Cs4,?QUOT,[]),
			    keyval(Cs5, [{Key,Value}|Acc]);
			[?APOS|Cs4] ->
			    {Value,Cs5} = split_string(Cs4,?APOS,[]),
			    keyval(Cs5, [{Key,Value}|Acc]);
			Cs4 ->
			    {Value,Cs5} = split_word(Cs4,[]),
			    keyval(Cs5, [{Key,Value}|Acc])
		    end;
		Cs3 ->
		    keyval(Cs3, [Key | Acc])
	    end
    end.

split_string([Q|Cs],Q,Acc) ->
    {reverse(Acc), Cs};
split_string([C|Cs],Q,Acc) ->
    split_string(Cs, Q, [C|Acc]);
split_string([], _Q, Acc) ->
    {reverse(Acc), []}.
			    
split_word([C|Cs],Acc) ->
    if C == ?SP; C == ?TAB; C == ?CR; C == ?NL ->
	    {reverse(Acc), Cs};
       C==$= ->
	    {reverse(Acc), [$=|Cs]};	    
       true ->
	    split_word(Cs, [C|Acc])
    end;
split_word([], Acc) -> 
    {reverse(Acc),[]}.

drop_blanks(Cs=[C|Cs1]) ->
    if C == ?SP; C == ?TAB; C == ?CR; C == ?NL -> drop_blanks(Cs1);
       true -> Cs
    end;
drop_blanks([]) -> [].



    

    

	    
    

