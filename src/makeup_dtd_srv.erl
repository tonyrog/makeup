%%%-------------------------------------------------------------------
%%% File    : makeup_dtd_srv.erl
%%% Author  : Tony Rogvall <tony@bulldog.synap.se>
%%% Description : Server to keep track on DTD -> MODULE mapping
%%%
%%%    1a. PUBID  => Local File
%%%    1b. PUBID  => Compiled Module
%%%
%%%    2   SYSTEM => Absolute File
%%%    2   SYSTEM => Compiled Module
%%%
%%% Created :  1 Feb 2005 by Tony Rogvall <tony@bulldog.synap.se>
%%%-------------------------------------------------------------------
-module(makeup_dtd_srv).

-behaviour(gen_server).
%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------

-define(dbgi(Fmt,As), io:format((Fmt),(As))).

-ifdef(debug).
-define(dbg(Fmt,As), io:format((Fmt),(As))).
-ifdef(hard_debug).
-define(hard_dbg(Fmt,As), io:format((Fmt),(As))).
-else.
-define(hard_dbg(Fmt,As), ok).
-endif.

-else.
-define(dbg(Fmt,As), ok).
-define(hard_dbg(Fmt,As), ok).
-endif.

%%--------------------------------------------------------------------
%% External exports
-export([start_link/0, start/0, stop/0]).

-export([add_path/1]).
-export([system_mod/1, system_file/1, system_entry/1]).
-export([public_mod/1, public_file/1, public_entry/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% paths to locate system.map and public.map
-record(s,
	{
	  cache,      %% cached dtd/schema entries
	  paths = []  %% search paths
	 }).

-define(SERVER, ?MODULE).

%%====================================================================
%% External functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link/0
%% Description: Starts the server
%%--------------------------------------------------------------------
maybe_start() ->
    case whereis(?SERVER) of
	undefined -> start();
	_ -> ok
    end.
	     
start() ->
    gen_server:start({local,?SERVER}, ?MODULE, [], []).

start_link() ->
    gen_server:start_link({local,?SERVER}, ?MODULE, [], []).

stop() ->
    gen_server:call(?SERVER, stop).

add_path(Path) ->
    maybe_start(),
    gen_server:call(?SERVER, {add_path,Path}).

system_mod(Url) ->
    case system_entry(Url) of
	{ok,_Path,{Url,_Opts,Mod,_File}} ->
	    {ok,Mod};
	Error ->
	    Error
    end.

system_file(Url) ->
    case system_entry(Url) of
	{ok,Path,{Url,_Opts,_Mod,File}} ->
	    case filename:pathtype(File) of
		relative ->
		    {ok,filename:join(Path,File)};
		_ ->
		    {ok,File}
	    end;
	Error ->
	    Error
    end.

public_mod(PubID) ->
    case public_entry(PubID) of
	{ok,_Path,{_,_Url,_Opts,Mod,_File}} ->
	    if Mod == undefined ->
		    {error, no_dtd_module};
	       true ->
		    {ok, Mod}
	    end;
	Error ->
	    Error
    end.

public_file(PubID) ->
    case public_entry(PubID) of
	{ok,Path,{_,_Url,_Opts,_Mod,File}} ->
	    case filename:pathtype(File) of
		relative ->
		    {ok,filename:join(Path,File)};
		_ ->
		    {ok,File}
	    end;
	Error ->
	    Error
    end.

system_entry(Url) ->
    maybe_start(),
    gen_server:call(?SERVER, {system_entry, Url}).

public_entry(PubID) ->
    maybe_start(),
    gen_server:call(?SERVER, {public_entry, PubID}).

%%====================================================================
%% Server functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%%--------------------------------------------------------------------
init([]) ->
    Cache =  ets:new(cache, []),
    Path = code:priv_dir(makeup),
    Paths = case Path of
		{error,bad_name} -> 
		    ["."];
		Dir -> 
		    code:add_path(filename:join(Path, "ebin")),
		    [".", Dir]
	    end,
    {ok, #s{ cache=Cache, paths = Paths }}.

%%--------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_call({add_path,Path}, _From, S) ->
    Paths = S#s.paths ++ [Path],
    {reply,ok,S#s { paths = Paths}};

handle_call({system_entry,Url}, _From, S) ->
    {Reply,S1} = system_entry(Url,S),
    {reply,Reply,S1};

handle_call({public_entry,PubID}, _From, S) ->
    {Reply,S1} = public_entry(PubID,S),
    {reply,Reply,S1};

handle_call(stop, _From, S) ->
    {stop, normal, ok, S};
    
handle_call(_Request, _From, S) ->
    {reply, {error,bad_reuqest}, S}.

%%--------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

public_entry(PubID, S) ->
    file_entry(PubID, 1, public, "public.map", S).

system_entry(Url, S) ->
    file_entry(Url, 1, system, "system.map", S).

file_entry(Key,Pos,Type,FileName,S) ->
    case ets:lookup(S#s.cache,{Type,Key}) of
	[] ->
	    file_entry_search(Key,Pos,Type,FileName,S#s.paths,S);
	[{_,Path,Ent}] ->
	    {{ok,Path,Ent}, S}
    end.

file_entry_search(Key,Pos,Type,FileName,[Path|Ps],S) ->
    AbsPath=filename:absname(Path),
    File = filename:join(AbsPath,FileName),
    ?dbg("Searching: ~p for ~p\n", [File,Key]),
    case file:consult(File) of
	{ok,List} ->
	    Found =
		lists:foldr(
		  fun(Ent,Found) ->
			  Key1 = element(Pos,Ent),
			  case ets:lookup(S#s.cache, {Type,Key1}) of
			      [] ->
				  ets:insert(S#s.cache,{{Type,Key1},AbsPath,Ent}),
				  ?dbg("Added ~s ~s to cache\n", [Type,Key1]);
			      _ ->
				  ok
			  end,
			  if Found==undefined, Key==Key1 -> Ent;
			     true -> Found
			  end
		  end, undefined, List),
	    if Found == undefined ->
		    file_entry_search(Key,Pos,Type,FileName,Ps,S);
	       true ->
		    {{ok,AbsPath,Found},S}
	    end;
	_Error ->
	    file_entry_search(Key,Pos,Type,FileName,Ps,S)
    end;
file_entry_search(_Key,_Pos,_Type,_FileName,[],S) ->
    ?dbg("NOT Found\n",[]),
    {{error, not_found},S}.


	
