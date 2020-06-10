%%% File    : makeup.hrl
%%% Author  : Tony Rogvall <tony@bix.hemma.se>
%%% Description : makeup functions
%%% Created : 28 Jan 2005 by Tony Rogvall <tony@bix.hemma.se>

-ifndef(MAKEUP_HRL).
-define(MAKEUP_HRL, true).

%% With Tag | [Ns|Tag]
-define(is_tag(T), is_atom((T)); is_atom(hd((T))),is_atom(tl((T)))).
-define(is_upper(C), (C)>=$A, C=<$Z).
-define(is_lower(C), (C)>=$a, C=<$z).
-define(is_digit(C), (C)>=$0, C=<$9).
-define(is_xdigit(C), (C)>=$0, C=<$9;
	(C) >= $A, (C) =< $F; (C) >= $a, (C) =< $f).

-ifndef(NL).
-define(NL,   $\n).
-endif.

-ifndef(CR).
-define(CR,   $\r).
-endif.

-ifndef(TAB).
-define(TAB,  $\t).
-endif.

-ifndef(SP).
-define(SP,   $\s).
-endif.

-define(LT,   $<).
-define(GT,   $>).
-define(AMP,  $&).
-define(APOS, $\').     %% '
-define(QUOT, $\").     %% "
-define(LBRACKET, $\[).
-define(RBRACKET, $\]).
-define(LPAREN, $\().
-define(RPAREN, $\)).
-define(LCBRACKET, $\{).
-define(RCBRACKET, $\}).
-define(DOT, $.).

%% makeup:file() return with standard action
-record(makeup_document,
	{
	  is_valid,      %% is validated
	  is_xml,        %% xml declataion found
	  encoding,      %% the encoding used
	  doctype,       %% !DOCTYPE [attributes]
	  xml_version,   %% normally 1.0 but 1.1 is on it's way
	  wbxml_version, %% WBXML version if used
	  wbxml_module,  %% WBXML codepage module used
	  content
	 }).


%% <!ELEMENT <name> ['-'|'O'] ['-'|'O'] <rule> >
-record(makeup_element,
	{
	  name,             %% Tag name
	  def,              %% Tokens
	  start=mandatory,  %% mandatory | optional
	  stop=mandatory,   %% mandatory | optional
	  rule,             %% Regexp rule (FIXME should be nested element or type)
	  include=[],       %% [R]
	  exclude=[],       %% [R]
	  attr = []         %% [#makeup_attribute]
	 }).


%% <!ATTLIST <name> <attlist>>
-record(makeup_attlist,
	{
	  name,  %% Element name
	  def    %% Tokens
	 }).

%% <!ENTITY <name> <def>>
-record(makeup_entity,
	{
	  name,  %% Entity name
	  type,  %% inline,public,cdata
	  def    %% Tokens | Value
	 }).

%% Parse attribute for dtd_element
-record(makeup_attribute,
	{
	  name,
	  type,   %% {enum,Es} | TypeName....
	  status, %% optional,required,default|fixed
	  value   %% defined if status=default|fixed
	 }).

%% Finite automata edge
-record(fa_edge,
	{
	  id,          %% edge id  (typically an atom)
	  action=[],   %% action list when present
	  source,      %% edge source
	  target,      %% edge target
	  data=[]      %% optional data field
	 }).

%% Finite automata state
-record(fa_state,
	{
	  id,           %% State id
	  edges=[],     %% [#fa_edge]
	  accept=false  %% Accepting state
	 }).

-record(fa,
	{
	  type,         %% nfa or dfa
	  init,         %% initial state
	  states=[]     %% [#fa_state]
	 }).

-endif.
