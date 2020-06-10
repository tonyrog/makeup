%%% File    : makeup_xsd.hrl
%%% Author  : Tony Rogvall <tony@PBook.local>
%%% Description : XMLSchema records
%%% Created :  4 Apr 2006 by Tony Rogvall <tony@PBook.local>

-define(IMPLIED(F),  F = implied).
-define(REQUIRED(F), F = required).
-define(FIXED(F,V),  F = {fixed,(V)}).
-define(DEFAULT(F,V),  F = {default,(V)}).
-define(XATTRIBUTES,   xattributes).

%%
%% <!ELEMENT %schema; ((%include; | %import; | %redefine; | %annotation;)*,
%%                    ((%simpleType; | %complexType;
%%                      | %element; | %attribute;
%%                      | %attributeGroup; | %group;
%%                      | %notation; ),
%%                     (%annotation;)*)* )>
%%
-record(xsd_schema,
	{
	  ?IMPLIED(targetNamespace),
	  ?IMPLIED(version),
	  ?FIXED(nds,"http://www.w3.org/2001/XMLSchema"),
	  ?IMPLIED(xmlns),
	  ?DEFAULT(finalDefault,""),
	  ?DEFAULT(blockDefault,""),
	  ?IMPLIED(id),
	  ?DEFAULT(elementFormDefault,"unqualified"),
	  ?DEFAULT(attributeFormDefault,"unqualified"),
	  ?IMPLIED(lang),
	  ?XATTRIBUTES,
	  %% elements
	  decl,       %% include | import | redefine | annotation
	  content     %% [simpleType|complexType|element|
	              %%  attribute|attributeGroup|group|notation]
	 }).

%%
%% <!ELEMENT %complexType; ((%annotation;)?,
%%                         (%simpleContent;|%complexContent;|
%%                          %particleAndAttrs;))>
%%
%%  <particleAndAttrs> ::=  ((<mgs> | <group>)?, <attrDecls>)
%%  <attrDecls>   ::= (<attribute>|<attributeGroup>)*,<anyAttribute>?
%%
%%
-record(xsd_complexType,
	{
	  ?IMPLIED(name),
	  ?IMPLIED(id),
	  ?IMPLIED(abstract),
	  ?IMPLIED(final),
	  ?IMPLIED(block),
	  ?DEFAULT(mixed,"false"), %% mix of text and elements
	  ?XATTRIBUTES,
	  %% elemetns
	  annotation,
	  content,
	  attrDecls,
	  post_annotation
	 }).

%% <!ELEMENT %complexContent; ((%annotation;)?, (%restriction;|%extension;))>
-record(xsd_complexContent,
	{
	  ?IMPLIED(mixed),  %% mix of text and elements
	  ?IMPLIED(id),
	  ?XATTRIBUTES,
	  %% elements
	  annotation,
	  content
	 }).

%% <!ELEMENT %simpleContent; ((%annotation;)?, (%restriction;|%extension;))>
-record(xsd_simpleContent,
	{
	  ?IMPLIED(id),
	  ?XATTRIBUTES,
	  %% elementes
	  annotation,
	  content
	 }).

%% <!ELEMENT %extension; ((%annotation;)?, (%particleAndAttrs;))>
-record(xsd_extension,
	{
	  ?REQUIRED(base),
	  ?IMPLIED(id),
	  ?XATTRIBUTES,
	  annotation,
	  content,     %% (mgs | group)?
	  attrDecls    %% attrDecls
	 }).


%% <!ELEMENT %element; ((%annotation;)?, (%complexType;| %simpleType;)?,
%%                      (%unique; | %key; | %keyref;)*)>
-record(xsd_element,
	{
	  ?IMPLIED(name),
	  ?IMPLIED(id),
	  ?IMPLIED(ref),
	  ?IMPLIED(type),
	  ?IMPLIED(minOccurs),
	  ?IMPLIED(maxOccurs),
	  ?IMPLIED(nillable),
	  ?IMPLIED(substitutionGroup),
	  ?IMPLIED(abstract),
	  ?IMPLIED(final),
	  ?IMPLIED(block),
	  ?IMPLIED(default),
	  ?IMPLIED(fixed),
	  ?IMPLIED(form),
	  ?XATTRIBUTES,
	  %% 
	  annotation,
	  content,   %% simpleType | complexType | undefined
	  %% [ unique | key | keyref ]
	  key_list,
	  post_annotation
	 }).

%% <!ELEMENT %group; ((%annotation;)?,(%mgs;)?)>
-record(xsd_group,
	{
	  ?IMPLIED(name),
	  ?IMPLIED(ref),
	  ?IMPLIED(minOccurs),
	  ?IMPLIED(maxOccurs),
	  ?IMPLIED(id),
	  ?XATTRIBUTES,
	  annotation,
	  content,
	  post_annotation
	 }).

%% <!ELEMENT %all; ((%annotation;)?, (%element;)*)>
-record(xsd_all,
	{
	  ?IMPLIED(minOccurs),    %% (1)
	  ?IMPLIED(maxOccurs),    %% (1),
	  ?IMPLIED(id),
	  ?XATTRIBUTES,
	  annotation,
	  content
	 }).

%% <!ELEMENT %choice; ((%annotation;)?, (%element;| %group;| %cs; | %any;)*)>
-record(xsd_choice,
	{
	  ?IMPLIED(minOccurs),
	  ?IMPLIED(maxOccurs),
	  ?IMPLIED(id),
	  ?XATTRIBUTES,
	  annotation,
	  content
	 }).
%%
%% <!ELEMENT %sequence; ((%annotation;)?, (%element;| %group;| %cs; | %any;)*)>
%%
-record(xsd_sequence,
	{
	  ?IMPLIED(minOccurs),
	  ?IMPLIED(maxOccurs),
	  ?IMPLIED(id),
	  ?XATTRIBUTES,
	  annotation,
	  content
	 }).

%% <!ELEMENT %any; (%annotation;)?>
-record(xsd_any,
	{
	  ?DEFAULT(namespace, "##any"),
	  ?DEFAULT(processContents,"strict"),
	  ?DEFAULT(minOccurs, "1"),
	  ?DEFAULT(maxOccurs, "1"),
	  ?IMPLIED(id),
	  ?XATTRIBUTES,
	  annotation
	 }).

%% <!ELEMENT %anyAttribute; (%annotation;)?>
-record(xsd_anyAttribute,
	{
	  ?DEFAULT(namespace, "##any"),
	  ?DEFAULT(processContents,"strict"),
	  ?IMPLIED(id),
	  ?XATTRIBUTES,
	  annotation
	 }).

%% <!ELEMENT %attribute; ((%annotation;)?, (%simpleType;)?)>
-record(xsd_attribute,
	{
	  ?IMPLIED(name),
	  ?IMPLIED(id),
	  ?IMPLIED(ref),
	  ?IMPLIED(type),
	  ?IMPLIED(use),
	  ?IMPLIED(default),
	  ?IMPLIED(fixed),
	  ?IMPLIED(form),
	  ?XATTRIBUTES,
	  %% elements
	  annotation,
	  content,
	  post_annotation
	 }).
%%
%% <!ELEMENT %attributeGroup; ((%annotation;)?,
%%                       (%attribute; | %attributeGroup;)*,
%%                       (%anyAttribute;)?) >
-record(xsd_attributeGroup,
	{
	  ?IMPLIED(name),
	  ?IMPLIED(id),
	  ?IMPLIED(ref),
	  ?XATTRIBUTES,
	  %%
	  annotation,
	  decls,
	  post_annotation
	 }).


%% <!ELEMENT %unique; ((%annotation;)?, %selector;, (%field;)+)>
-record(xsd_unique,
	{
	  ?REQUIRED(name),
	  ?IMPLIED(id),
	  ?XATTRIBUTES,
	  %% 
	  annotation,
	  selector,
	  fields
	 }).

%% <!ELEMENT %key;    ((%annotation;)?, %selector;, (%field;)+)>
-record(xsd_key,
	{
	  ?REQUIRED(name),
	  ?IMPLIED(id),
	  ?XATTRIBUTES,
	  %%
	  annotation,
	  selector,
	  fields
	 }).

%% <!ELEMENT %keyref; ((%annotation;)?, %selector;, (%field;)+)>
-record(xsd_keyref,
	{
	  ?REQUIRED(name),
	  ?REQUIRED(refer),
	  ?IMPLIED(id),
	  ?XATTRIBUTES,
	  %%
	  annotation,
	  selector,
	  fields
	 }).

%% <!ELEMENT %selector; ((%annotation;)?)>
-record(xsd_selector,
	{
	  ?REQUIRED(xpath),
	  ?IMPLIED(id),
	  ?XATTRIBUTES,
	  %% 
	  annotation
	 }).

%% <!ELEMENT %field; ((%annotation;)?)>
-record(xsd_field,
	{
	  ?REQUIRED(xpath),
	  ?IMPLIED(id),
	  ?XATTRIBUTES,
	  %%
	  annotation 
	 }).

%% <!ELEMENT %include; (%annotation;)?>
-record(xsd_include,
	{
	  ?REQUIRED(schemaLocation),
	  ?IMPLIED(id),
	  ?XATTRIBUTES,
	  annotation
	 }).

%% <!ELEMENT %import; (%annotation;)?>
-record(xsd_import,
	{
	  ?IMPLIED(namespace),
	  ?IMPLIED(schemaLocation),
	  ?IMPLIED(id),
	  ?XATTRIBUTES,
	  annotation
	 }).

%% <!ELEMENT %redefine; (%annotation; | %simpleType; | %complexType; |
%%                      %attributeGroup; | %group;)*>
-record(xsd_redefine,
	{
	  ?REQUIRED(schemaLocation),
	  ?IMPLIED(id),
	  ?XATTRIBUTES,
	  content
	 }).

%% <!ELEMENT %notation; (%annotation;)?>
-record(xsd_notation,
	{
	  ?REQUIRED(name),
	  ?IMPLIED(id),
	  ?REQUIRED(public),
	  ?IMPLIED(system),
	  ?XATTRIBUTES,
	  annotation,
	  post_annotation
	 }).

%% <!ELEMENT %annotation; (%appinfo; | %documentation;)*>
-record(xsd_annotation,
	{
	  ?XATTRIBUTES,
	  content   %% (appinfo | documentation)*
	 }).

%% <!ELEMENT %appinfo; ANY>   <!-- too restrictive -->
-record(xsd_appinfo,
	{
	  ?IMPLIED(source),
	  ?IMPLIED(id),
	  ?XATTRIBUTES,
	  info
	  }).

%% <!ELEMENT %documentation; ANY>   <!-- too restrictive -->
-record(xsd_documentation,
	{
	  ?IMPLIED(source),
	  ?IMPLIED(id),
	  ?IMPLIED(lang),
	  ?XATTRIBUTES,
	  doc 
	 }).

%% from datatypes.dtd
-record(xsd_simpleType,
	{
	  ?IMPLIED(name),
	  ?IMPLIED(final),
	  ?IMPLIED(id),
	  ?XATTRIBUTES,
	  %% element
	  annotation,
	  content,   %% restriction|list|union
	  post_annotation
	 }).

-record(xsd_restriction,
	{
	  ?IMPLIED(base),
	  ?IMPLIED(id),
	  ?XATTRIBUTES,
	  %% element
	  annotation,
	  content,     %% (mgs|group) | (simpleType?, facet*)
	  facets,      %% facet*
	  attrDecls    %% attrDecls
	 }).

-record(xsd_list,
	{
	  ?IMPLIED(itemType),
	  ?IMPLIED(id),
	  ?XATTRIBUTES,
	  %% element
	  annotation,
	  content     %% simpleType?
	 }).


-record(xsd_union,
	{
	  ?IMPLIED(id),
	  ?IMPLIED(memberTypes),
	  ?XATTRIBUTES,
	  %% element
	  annotation,
	  content     %% simpleType*
	 }).

-define(facetAttr, 
	?REQUIRED(value),
	?IMPLIED(id)
       ).
-define(fixedAttr, 
	?IMPLIED(fixed)          %% %boolean;
       ).

%% Actually a record template ....
-record(xsd_facet,
	{
	  ?facetAttr,
	  ?fixedAttr,
	  ?XATTRIBUTES,
	  %%
	  annotation
	 }).

-record(xsd_maxExclusive,
	{
	  ?facetAttr,
	  ?fixedAttr,
	  ?XATTRIBUTES,
	  %%
	  annotation
	 }).
	  
-record(xsd_minExclusive,
	{
	  ?facetAttr,
	  ?fixedAttr,
	  ?XATTRIBUTES,
	  %%
	  annotation
	 }).

-record(xsd_maxInclusive,
	{
	  ?facetAttr,
	  ?fixedAttr,
	  ?XATTRIBUTES,
	  %%
	  annotation
	 }).

-record(xsd_minInclusive,
	{
	  ?facetAttr,
	  ?fixedAttr,
	  ?XATTRIBUTES,
	  %%
	  annotation
	 }).

-record(xsd_totalDigits,
	{
	  ?facetAttr,
	  ?fixedAttr,
	  ?XATTRIBUTES,
	  %%
	  annotation
	 }).

-record(xsd_fractionDigits,
	{
	  ?facetAttr,
	  ?fixedAttr,
	  ?XATTRIBUTES,
	  %%
	  annotation
	 }).

-record(xsd_length,
	{
	  ?facetAttr,
	  ?fixedAttr,
	  ?XATTRIBUTES,
	  %%
	  annotation
	 }).

-record(xsd_minLength,
	{
	  ?facetAttr,
	  ?fixedAttr,
	  ?XATTRIBUTES,
	  %%
	  annotation
	 }).

-record(xsd_maxLength,
	{
	  ?facetAttr,
	  ?fixedAttr,
	  ?XATTRIBUTES,
	  %%
	  annotation
	 }).

-record(xsd_enumeration,
	{
	  ?facetAttr,
	  ?XATTRIBUTES,
	  %%
	  annotation
	 }).

-record(xsd_whiteSpace,
	{
	  ?facetAttr,
	  ?fixedAttr,
	  ?XATTRIBUTES,
	  %%
	  annotation
	 }).

-record(xsd_pattern,
	{
	  ?facetAttr,
	  ?XATTRIBUTES,
	  %%
	  annotation
	 }).
