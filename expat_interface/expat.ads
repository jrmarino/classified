--  Copyright (c) 2010, John R. Marino
--  All rights reserved.
--
--  Permission to use, copy, modify, and/or distribute this software for any
--  purpose with or without fee is hereby granted, provided that the above
--  copyright notice and this permission notice appear in all copies.
--
--  THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
--  WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
--  MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
--  ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
--  WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
--  ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
--  OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.


with System;
with Interfaces.C.Strings;

package expat is

   subtype XML_Bool  is Interfaces.C.unsigned_char;
   subtype XML_Char  is Interfaces.C.char;
   subtype XML_LChar is Interfaces.C.char;
   subtype XML_Index is Interfaces.C.long;
   subtype XML_Size  is Interfaces.C.unsigned_long;
   subtype size_t    is Interfaces.C.size_t;   --  <iso/stdlib_iso.h>

   subtype Access_Void is System.Address;
   subtype Access_XML_Char is Interfaces.C.Strings.chars_ptr;
   subtype Access_XML_Char_Array is Interfaces.C.Strings.chars_ptr_array;
   subtype XML_Parser is Access_Void;  --  expat.h:25

   --   The XML_Status enum gives the possible return values for several
   --   API functions.  The preprocessor #defines are included so this
   --   stanza can be added to code that still needs to support older
   --   versions of Expat 1.95.x:
   --   #ifndef XML_STATUS_OK
   --   #define XML_STATUS_OK    1
   --   #define XML_STATUS_ERROR 0
   --   #endif
   --   Otherwise, the #define hackery is quite ugly and would have been
   --   dropped.


   type XML_Status is (
      XML_STATUS_ERROR,
      XML_STATUS_OK,
      XML_STATUS_SUSPENDED);
   pragma Convention (C, XML_Status);  --  expat.h:45


   type XML_Error is (
      XML_ERROR_NONE,
      XML_ERROR_NO_MEMORY,
      XML_ERROR_SYNTAX,
      XML_ERROR_NO_ELEMENTS,
      XML_ERROR_INVALID_TOKEN,
      XML_ERROR_UNCLOSED_TOKEN,
      XML_ERROR_PARTIAL_CHAR,
      XML_ERROR_TAG_MISMATCH,
      XML_ERROR_DUPLICATE_ATTRIBUTE,
      XML_ERROR_JUNK_AFTER_DOC_ELEMENT,
      XML_ERROR_PARAM_ENTITY_REF,
      XML_ERROR_UNDEFINED_ENTITY,
      XML_ERROR_RECURSIVE_ENTITY_REF,
      XML_ERROR_ASYNC_ENTITY,
      XML_ERROR_BAD_CHAR_REF,
      XML_ERROR_BINARY_ENTITY_REF,
      XML_ERROR_ATTRIBUTE_EXTERNAL_ENTITY_REF,
      XML_ERROR_MISPLACED_XML_PI,
      XML_ERROR_UNKNOWN_ENCODING,
      XML_ERROR_INCORRECT_ENCODING,
      XML_ERROR_UNCLOSED_CDATA_SECTION,
      XML_ERROR_EXTERNAL_ENTITY_HANDLING,
      XML_ERROR_NOT_STANDALONE,
      XML_ERROR_UNEXPECTED_STATE,
      XML_ERROR_ENTITY_DECLARED_IN_PE,
      XML_ERROR_FEATURE_REQUIRES_XML_DTD,
      XML_ERROR_CANT_CHANGE_FEATURE_ONCE_PARSING,
      XML_ERROR_UNBOUND_PREFIX,
      XML_ERROR_UNDECLARING_PREFIX,
      XML_ERROR_INCOMPLETE_PE,
      XML_ERROR_XML_DECL,
      XML_ERROR_TEXT_DECL,
      XML_ERROR_PUBLICID,
      XML_ERROR_SUSPENDED,
      XML_ERROR_NOT_SUSPENDED,
      XML_ERROR_ABORTED,
      XML_ERROR_FINISHED,
      XML_ERROR_SUSPEND_PE,
      XML_ERROR_RESERVED_PREFIX_XML,
      XML_ERROR_RESERVED_PREFIX_XMLNS,
      XML_ERROR_RESERVED_NAMESPACE_URI);
   pragma Convention (C, XML_Error);  --  expat.h:54


   subtype XML_Content_Type is Interfaces.C.unsigned;
   XML_CTYPE_EMPTY  : constant XML_Content_Type := 1;
   XML_CTYPE_ANY    : constant XML_Content_Type := 2;
   XML_CTYPE_MIXED  : constant XML_Content_Type := 3;
   XML_CTYPE_NAME   : constant XML_Content_Type := 4;
   XML_CTYPE_CHOICE : constant XML_Content_Type := 5;
   XML_CTYPE_SEQ    : constant XML_Content_Type := 6;  --  expat.h:101


   type XML_Content_Quant is (
      XML_CQUANT_NONE,
      XML_CQUANT_OPT,
      XML_CQUANT_REP,
      XML_CQUANT_PLUS);
   pragma Convention (C, XML_Content_Quant);  --  expat.h:110


   --   If type == XML_CTYPE_EMPTY or XML_CTYPE_ANY, then quant will be
   --   XML_CQUANT_NONE, and the other fields will be zero or NULL.
   --   If type == XML_CTYPE_MIXED, then quant will be NONE or REP and
   --   numchildren will contain number of elements that may be mixed in
   --   and children point to an array of XML_Content cells that will be
   --   all of XML_CTYPE_NAME type with no quantification.
   --   If type == XML_CTYPE_NAME, then the name points to the name, and
   --   the numchildren field will be zero and children will be NULL. The
   --   quant fields indicates any quantifiers placed on the name.
   --   CHOICE and SEQ will have name NULL, the number of children in
   --   numchildren and children will point, recursively, to an array
   --   of XML_Content cells.
   --   The EMPTY, ANY, and MIXED types will only occur at top level.


   type XML_cp is record
      c_type      : aliased XML_Content_Type;          --  expat.h:138
      quant       : aliased XML_Content_Quant;         --  expat.h:139
      name        : Access_XML_Char;                  --  expat.h:140
      numchildren : aliased Interfaces.C.unsigned;     --  expat.h:141
      children    : access  XML_cp;                    --  expat.h:142
   end record;
   subtype XML_Content is XML_cp;


   pragma Convention (C_Pass_By_Copy, XML_cp);  --  expat.h:137


   --  This is called for an element declaration. See above for
   --  description of the model argument. It's the caller's responsibility
   --  to free model when finished with it.


   type XML_ElementDeclHandler is
      access procedure (
         userData : Access_Void;
         name     : Access_XML_Char;
         model    : access XML_Content);  --  expat.h:152
   pragma Convention (C, XML_ElementDeclHandler);


   procedure XML_SetElementDeclHandler (
      parser : XML_Parser;
      eldecl : XML_ElementDeclHandler);  --  expat.h:155
   pragma Import (C, XML_SetElementDeclHandler, "XML_SetElementDeclHandler");


   --  The Attlist declaration handler is called for *each* attribute. So
   --  a single Attlist declaration with multiple attributes declared will
   --  generate multiple calls to this handler. The "default" parameter
   --  may be NULL in the case of the "#IMPLIED" or "#REQUIRED"
   --  keyword. The "isrequired" parameter will be true and the default
   --  value will be NULL in the case of "#REQUIRED". If "isrequired" is
   --  true and default is non-NULL, then this is a "#FIXED" default.


   type XML_AttlistDeclHandler is
      access procedure (
         userData   : Access_Void;
         elname     : Access_XML_Char;
         attname    : Access_XML_Char;
         att_type   : Access_XML_Char;
         dflt       : Access_XML_Char;
         isrequired : Interfaces.C.int);  --  expat.h:172
   pragma Convention (C, XML_AttlistDeclHandler);


   procedure XML_SetAttlistDeclHandler (
      parser  : XML_Parser;
      attdecl : XML_AttlistDeclHandler);  --  expat.h:175
   pragma Import (C, XML_SetAttlistDeclHandler, "XML_SetAttlistDeclHandler");


   --  The XML declaration handler is called for *both* XML declarations
   --  and text declarations. The way to distinguish is that the version
   --  parameter will be NULL for text declarations. The encoding
   --  parameter may be NULL for XML declarations. The standalone
   --  parameter will be -1, 0, or 1 indicating respectively that there
   --  was no standalone parameter in the declaration, that it was given
   --  as no, or that it was given as yes.


   type XML_XmlDeclHandler is
      access procedure (
         userData   : Access_Void;
         version    : Access_XML_Char;
         encoding   : Access_XML_Char;
         standalone : Interfaces.C.int);  --  expat.h:189
   pragma Convention (C, XML_XmlDeclHandler);


   procedure XML_SetXmlDeclHandler (
      parser  : XML_Parser;
      xmldecl : XML_XmlDeclHandler);  --  expat.h:192
   pragma Import (C, XML_SetXmlDeclHandler, "XML_SetXmlDeclHandler");


   type XML_Memory_Handling_Suite is record
      malloc_fcn  : access function (
                      size : size_t
                    ) return Access_Void;  --  expat.h:197
      realloc_fcn : access function (
                      ptr  : Access_Void;
                      size : size_t
                    ) return Access_Void;  --  expat.h:198
      free_fcn    : access procedure (
                       ptr : Access_Void
                    );  --  expat.h:199
   end record;
   pragma Convention (C_Pass_By_Copy, XML_Memory_Handling_Suite);


   --  Constructs a new parser; encoding is the encoding specified by the
   --  external protocol or NULL if there is none specified.


   function XML_ParserCreate (encoding : Access_XML_Char) return XML_Parser;
   pragma Import (C, XML_ParserCreate, "XML_ParserCreate");

   --  Constructs a new parser and namespace processor.  Element type
   --  names and attribute names that belong to a namespace will be
   --  expanded; unprefixed attribute names are never expanded; unprefixed
   --  element type names are expanded only if there is a default
   --  namespace. The expanded name is the concatenation of the namespace
   --  URI, the namespace separator character, and the local part of the
   --  name.  If the namespace separator is '\0' then the namespace URI
   --  and the local part will be concatenated without any separator.
   --  It is a programming error to use the separator '\0' with namespace
   --  triplets (see XML_SetReturnNSTriplet).


   function XML_ParserCreateNS (encoding           : Access_XML_Char;
                                namespaceSeparator : XML_Char)
   return XML_Parser;
   pragma Import (C, XML_ParserCreateNS, "XML_ParserCreateNS");


   --  Constructs a new parser using the memory management suite referred to
   --  by memsuite. If memsuite is NULL, then use the standard library memory
   --  suite. If namespaceSeparator is non-NULL it creates a parser with
   --  namespace processing as described above. The character pointed at
   --  will serve as the namespace separator.
   --  All further memory operations used for the created parser will come from
   --  the given suite.


   function XML_ParserCreate_MM (
      encoding           : Access_XML_Char;
      memsuite           : access constant XML_Memory_Handling_Suite;
      namespaceSeparator : Access_XML_Char)
   return XML_Parser;  --  expat.h:233
   pragma Import (C, XML_ParserCreate_MM, "XML_ParserCreate_MM");


   --  Prepare a parser object to be re-used.  This is particularly
   --  valuable when memory allocation overhead is disproportionatly high,
   --  such as when a large number of small documnents need to be parsed.
   --  All handlers are cleared from the parser, except for the
   --  unknownEncodingHandler. The parser's external state is re-initialized
   --  except for the values of ns and ns_triplets.
   --  Added in Expat 1.95.3.


   function XML_ParserReset (parser   : XML_Parser;
                             encoding : Access_XML_Char) return XML_Bool;
   pragma Import (C, XML_ParserReset, "XML_ParserReset");

   --  atts is array of name/value pairs, terminated by 0;
   --  names and values are 0 terminated.


   type XML_StartElementHandler is
      access procedure (
         userData : in out Access_Void;
         name     : in     Access_XML_Char;
         atts     : in     Access_XML_Char_Array);  --  expat.h:254
   pragma Convention (C, XML_StartElementHandler);


   type XML_EndElementHandler is
      access procedure (
         userData : in out Access_Void;
         name     : in     Access_XML_Char);
   pragma Convention (C, XML_EndElementHandler);


   --  s is not 0 terminated.
   type XML_CharacterDataHandler is
      access procedure (
         userData : in out Access_Void;
         s        : in     Access_XML_Char;
         len      : in     size_t);  --  expat.h:263
   pragma Convention (C, XML_CharacterDataHandler);


   --  target and data are 0 terminated
   type XML_ProcessingInstructionHandler is
      access procedure (
         userData : Access_Void;
         target   : Access_XML_Char;
         data     : Access_XML_Char);  --  expat.h:269
   pragma Convention (C, XML_ProcessingInstructionHandler);


   --  data is 0 terminated
   type XML_CommentHandler is
      access procedure (
         userData : Access_Void;
         data     : Access_XML_Char);
   pragma Convention (C, XML_CommentHandler);


   type XML_StartCdataSectionHandler is
      access procedure (
         userData : Access_Void);  --  expat.h:275
   pragma Convention (C, XML_StartCdataSectionHandler);


   type XML_EndCdataSectionHandler is
      access procedure (
         userData : Access_Void);  --  expat.h:276
   pragma Convention (C, XML_EndCdataSectionHandler);

   --  This is called for any characters in the XML document for which
   --  there is no applicable handler.  This includes both characters that
   --  are part of markup which is of a kind that is not reported
   --  (comments, markup declarations), or characters that are part of a
   --  construct which could be reported but for which no handler has been
   --  supplied. The characters are passed exactly as they were in the XML
   --  document except that they will be encoded in UTF-8 or UTF-16.
   --  Line boundaries are not normalized. Note that a byte order mark
   --  character is not passed to the default handler. There are no
   --  guarantees about how characters are divided between calls to the
   --  default handler: for example, a comment might be split between
   --  multiple calls.


   type XML_DefaultHandler is
      access procedure (
         userData : Access_Void;
         s        : Access_XML_Char;
         len      : Interfaces.C.int);  --  expat.h:293
   pragma Convention (C, XML_DefaultHandler);


   --  This is called for the start of the DOCTYPE declaration, before
   --  any DTD or internal subset is parsed.


   type XML_StartDoctypeDeclHandler is
      access procedure (
         userData            : Access_Void;
         doctypeName         : Access_XML_Char;
         sysid               : Access_XML_Char;
         pubid               : Access_XML_Char;
         has_internal_subset : Interfaces.C.int);  --  expat.h:303
   pragma Convention (C, XML_StartDoctypeDeclHandler);


   --  This is called for the start of the DOCTYPE declaration when the
   --  closing > is encountered, but after processing any external subset.


   type XML_EndDoctypeDeclHandler is
      access procedure (userData : Access_Void);
   pragma Convention (C, XML_EndDoctypeDeclHandler);


   --  This is called for entity declarations. The is_parameter_entity
   --  argument will be non-zero if the entity is a parameter entity, zero
   --  otherwise.
   --  For internal entities (<!ENTITY foo "bar">), value will
   --  be non-NULL and systemId, publicID, and notationName will be NULL.
   --  The value string is NOT nul-terminated; the length is provided in
   --  the value_length argument. Since it is legal to have zero-length
   --  values, do not use this argument to test for internal entities.
   --  For external entities, value will be NULL and systemId will be
   --  non-NULL. The publicId argument will be NULL unless a public
   --  identifier was provided. The notationName argument will have a
   --  non-NULL value only for unparsed entity declarations.
   --  Note that is_parameter_entity can't be changed to XML_Bool, since
   --  that would break binary compatibility.


   type XML_EntityDeclHandler is
      access procedure (
         userData            : Access_Void;
         entityName          : Access_XML_Char;
         is_parameter_entity : Interfaces.C.int;
         value               : Access_XML_Char;
         value_length        : Interfaces.C.int;
         base                : Access_XML_Char;
         systemId            : Access_XML_Char;
         publicId            : Access_XML_Char;
         notationName        : Access_XML_Char);  --  expat.h:338
   pragma Convention (C, XML_EntityDeclHandler);


   procedure XML_SetEntityDeclHandler (
      parser  : XML_Parser;
      handler : XML_EntityDeclHandler);  --  expat.h:341
   pragma Import (C, XML_SetEntityDeclHandler, "XML_SetEntityDeclHandler");


   --  This is called for a declaration of notation.  The base argument is
   --  whatever was set by XML_SetBase. The notationName will never be
   --  NULL.  The other arguments can be.


   type XML_NotationDeclHandler is
      access procedure (
         userData     : Access_Void;
         notationName : Access_XML_Char;
         base         : Access_XML_Char;
         systemId     : Access_XML_Char;
         publicId     : Access_XML_Char);  --  expat.h:370
   pragma Convention (C, XML_NotationDeclHandler);


   type XML_UnparsedEntityDeclHandler is
      access procedure (
         userData     : Access_Void;
         entityName   : Access_XML_Char;
         base         : Access_XML_Char;
         systemId     : Access_XML_Char;
         publicId     : Access_XML_Char;
         notationName : Access_XML_Char);
   pragma Convention (C, XML_UnparsedEntityDeclHandler);


   --  When namespace processing is enabled, these are called once for
   --  each namespace declaration. The call to the start and end element
   --  handlers occur between the calls to the start and end namespace
   --  declaration handlers. For an xmlns attribute, prefix will be
   --  NULL.  For an xmlns="" attribute, uri will be NULL.


   type XML_StartNamespaceDeclHandler is
      access procedure (
         userData : Access_Void;
         prefix   : Access_XML_Char;
         uri      : Access_XML_Char);  --  expat.h:381
   pragma Convention (C, XML_StartNamespaceDeclHandler);


   type XML_EndNamespaceDeclHandler is
         access procedure (
            userData : Access_Void;
            prefix   : Access_XML_Char);  --  expat.h:385
   pragma Convention (C, XML_EndNamespaceDeclHandler);


   --  This is called if the document is not standalone, that is, it has an
   --  external subset or a reference to a parameter entity, but does not
   --  have standalone="yes". If this handler returns XML_STATUS_ERROR,
   --  then processing will not continue, and the parser will return a
   --  XML_ERROR_NOT_STANDALONE error.
   --  If parameter entity parsing is enabled, then in addition to the
   --  conditions above this handler will only be called if the referenced
   --  entity was actually read.


   type XML_NotStandaloneHandler is
      access function (userData : Access_Void)
      return Interfaces.C.int;  --  expat.h:396
   pragma Convention (C, XML_NotStandaloneHandler);


   --  This is called for a reference to an external parsed general
   --  entity.  The referenced entity is not automatically parsed.  The
   --  application can parse it immediately or later using
   --  XML_ExternalEntityParserCreate.
   --  The parser argument is the parser parsing the entity containing the
   --  reference; it can be passed as the parser argument to
   --  XML_ExternalEntityParserCreate.  The systemId argument is the
   --  system identifier as specified in the entity declaration; it will
   --  not be NULL.
   --  The base argument is the system identifier that should be used as
   --  the base for resolving systemId if systemId was relative; this is
   --  set by XML_SetBase; it may be NULL.
   --  The publicId argument is the public identifier as specified in the
   --  entity declaration, or NULL if none was specified; the whitespace
   --  in the public identifier will have been normalized as required by
   --  the XML spec.
   --  The context argument specifies the parsing context in the format
   --  expected by the context argument to XML_ExternalEntityParserCreate;
   --  context is valid only until the handler returns, so if the
   --  referenced entity is to be parsed later, it must be copied.
   --  context is NULL only when the entity is a parameter entity.
   --  The handler should return XML_STATUS_ERROR if processing should not
   --  continue because of a fatal error in the handling of the external
   --  entity.  In this case the calling parser will return an
   --  XML_ERROR_EXTERNAL_ENTITY_HANDLING error.
   --  Note that unlike other handlers the first argument is the parser,
   --   not userData.


   type XML_ExternalEntityRefHandler is
      access function (
         parser   : XML_Parser;
         context  : Access_XML_Char;
         base     : Access_XML_Char;
         systemId : Access_XML_Char;
         publicId : Access_XML_Char) return Interfaces.C.int;  --  expat.h:437
   pragma Convention (C, XML_ExternalEntityRefHandler);


   --  This is called in two situations:
   --  1) An entity reference is encountered for which no declaration
   --     has been read *and* this is not an error.
   --  2) An internal entity reference is read, but not expanded, because
   --     XML_SetDefaultHandler has been called.
   --  Note: skipped parameter entities in declarations and skipped general
   --        entities in attribute values cannot be reported, because
   --        the event would be out of sync with the reporting of the
   --        declarations or attribute values


   type XML_SkippedEntityHandler is
      access procedure (
         userData            : Access_Void;
         entityName          : Access_XML_Char;
         is_parameter_entity : Interfaces.C.int);  --  expat.h:452
   pragma Convention (C, XML_SkippedEntityHandler);


   --  This structure is filled in by the XML_UnknownEncodingHandler to
   --  provide information to the parser about encodings that are unknown
   --  to the parser.
   --  The map[b] member gives information about byte sequences whose
   --  first byte is b.
   --  If map[b] is c where c is >= 0, then b by itself encodes the
   --  Unicode scalar value c.
   --  If map[b] is -1, then the byte sequence is malformed.
   --  If map[b] is -n, where n >= 2, then b is the first byte of an
   --  n-byte sequence that encodes a single Unicode scalar value.
   --  The data member will be passed as the first argument to the convert
   --  function.
   --  The convert function is used to convert multibyte sequences; s will
   --  point to a n-byte sequence where map[(unsigned char)*s] == -n.  The
   --  convert function must return the Unicode scalar value represented
   --  by this byte sequence or -1 if the byte sequence is malformed.
   --  The convert function may be NULL if the encoding is a single-byte
   --  encoding, that is if map[b] >= -1 for all bytes b.
   --  When the parser is finished with the encoding, then if release is
   --  not NULL, it will call release passing it the data member; once
   --  release has been called, the convert function will not be called again.
   --  Expat places certain restrictions on the encodings that are supported
   --  using this mechanism.
   --  1. Every ASCII character that can appear in a well-formed XML document,
   --     other than the characters
   --     $@\^`{}~
   --     must be represented by a single byte, and that byte must be the
   --     same byte that represents that character in ASCII.
   --  2. No character may require more than 4 bytes to encode.
   --  3. All characters encoded must have Unicode scalar values <=
   --     0xFFFF, (i.e., characters that would be encoded by surrogates in
   --     UTF-16 are  not allowed).  Note that this restriction doesn't
   --     apply to the built-in support for UTF-8 and UTF-16.
   --  4. No Unicode character may be encoded by more than one distinct
   --     sequence of bytes.
   --
   --  skipped anonymous struct anon_45


   type XML_Encoding_map_array is array (0 .. 255) of aliased Interfaces.C.int;
   type XML_Encoding is record
      map     : aliased XML_Encoding_map_array;  --  expat.h:507
      data    : Access_Void;
      convert : access function (
                   data : Access_Void;
                   s    : Interfaces.C.Strings.chars_ptr)
                return Interfaces.C.int;
      release : access procedure (data : Access_Void);
   end record;
   pragma Convention (C_Pass_By_Copy, XML_Encoding);  --  expat.h:511


   --  This is called for an encoding that is unknown to the parser.
   --  The encodingHandlerData argument is that which was passed as the
   --  second argument to XML_SetUnknownEncodingHandler.
   --  The name argument gives the name of the encoding as specified in
   --  the encoding declaration.
   --  If the callback can provide information about the encoding, it must
   --  fill in the XML_Encoding structure, and return XML_STATUS_OK.
   --  Otherwise it must return XML_STATUS_ERROR.
   --  If info does not describe a suitable encoding, then the parser will
   --  return an XML_UNKNOWN_ENCODING error.


   type XML_UnknownEncodingHandler is
      access function (
         encodingHandlerData : Access_Void;
         name                : Access_XML_Char;
         info                : access XML_Encoding) return Interfaces.C.int;
   pragma Convention (C, XML_UnknownEncodingHandler);


   procedure XML_SetElementHandler
     (parser : XML_Parser;
      start  : XML_StartElementHandler;
      c_end  : XML_EndElementHandler);  --  expat.h:534
   pragma Import (C, XML_SetElementHandler, "XML_SetElementHandler");


   procedure XML_SetStartElementHandler (
      parser  : XML_Parser;
      handler : XML_StartElementHandler);  --  expat.h:539
   pragma Import (C, XML_SetStartElementHandler, "XML_SetStartElementHandler");


   procedure XML_SetEndElementHandler (
      parser  : XML_Parser;
      handler : XML_EndElementHandler);
   pragma Import (C, XML_SetEndElementHandler, "XML_SetEndElementHandler");


   procedure XML_SetCharacterDataHandler (
      parser  : XML_Parser;
      handler : XML_CharacterDataHandler);  --  expat.h:547
   pragma Import (C, XML_SetCharacterDataHandler,
                    "XML_SetCharacterDataHandler");


   procedure XML_SetProcessingInstructionHandler (
      parser  : XML_Parser;
      handler : XML_ProcessingInstructionHandler);  --  expat.h:551
   pragma Import (C, XML_SetProcessingInstructionHandler,
                    "XML_SetProcessingInstructionHandler");


   procedure XML_SetCommentHandler (
      parser  : XML_Parser;
      handler : XML_CommentHandler);
   pragma Import (C, XML_SetCommentHandler, "XML_SetCommentHandler");


   procedure XML_SetCdataSectionHandler
     (parser : XML_Parser;
      start  : XML_StartCdataSectionHandler;
      c_end  : XML_EndCdataSectionHandler);  --  expat.h:558
   pragma Import (C, XML_SetCdataSectionHandler, "XML_SetCdataSectionHandler");


   procedure XML_SetStartCdataSectionHandler (
      parser : XML_Parser;
      start  : XML_StartCdataSectionHandler);
   pragma Import (C, XML_SetStartCdataSectionHandler,
                    "XML_SetStartCdataSectionHandler");


   procedure XML_SetEndCdataSectionHandler (
      parser : XML_Parser;
      c_end  : XML_EndCdataSectionHandler);
   pragma Import (C, XML_SetEndCdataSectionHandler,
                    "XML_SetEndCdataSectionHandler");


   --  This sets the default handler and also inhibits expansion of
   --  internal entities. These entity references will be passed to the
   --  default handler, or to the skipped entity handler, if one is set.


   procedure XML_SetDefaultHandler (
      parser  : XML_Parser;
      handler : XML_DefaultHandler);  --  expat.h:575
   pragma Import (C, XML_SetDefaultHandler, "XML_SetDefaultHandler");


   --  This sets the default handler but does not inhibit expansion of
   --  internal entities.  The entity reference will not be passed to the
   --  default handler.


   procedure XML_SetDefaultHandlerExpand (
      parser  : XML_Parser;
      handler : XML_DefaultHandler);  --  expat.h:583
   pragma Import (C, XML_SetDefaultHandlerExpand,
                    "XML_SetDefaultHandlerExpand");


   procedure XML_SetDoctypeDeclHandler (
      parser : XML_Parser;
      start  : XML_StartDoctypeDeclHandler);  --  expat.h:587
   pragma Import (C, XML_SetDoctypeDeclHandler, "XML_SetDoctypeDeclHandler");


   procedure XML_SetStartDoctypeDeclHandler (
      parser : XML_Parser;
      start  : XML_StartDoctypeDeclHandler);  --  expat.h:592
   pragma Import (C, XML_SetStartDoctypeDeclHandler,
                    "XML_SetStartDoctypeDeclHandler");


   procedure XML_SetEndDoctypeDeclHandler (
      parser : XML_Parser;
      c_end  : XML_EndDoctypeDeclHandler);
   pragma Import (C, XML_SetEndDoctypeDeclHandler,
                    "XML_SetEndDoctypeDeclHandler");


   procedure XML_SetUnparsedEntityDeclHandler (
      parser  : XML_Parser;
      handler : XML_UnparsedEntityDeclHandler);  --  expat.h:600
   pragma Import (C, XML_SetUnparsedEntityDeclHandler,
                    "XML_SetUnparsedEntityDeclHandler");


   procedure XML_SetNotationDeclHandler (
      parser  : XML_Parser;
      handler : XML_NotationDeclHandler);  --  expat.h:604
   pragma Import (C, XML_SetNotationDeclHandler, "XML_SetNotationDeclHandler");


   procedure XML_SetNamespaceDeclHandler (
      parser : XML_Parser;
      start  : XML_StartNamespaceDeclHandler;
      c_end  : XML_EndNamespaceDeclHandler);
   pragma Import (C, XML_SetNamespaceDeclHandler,
                    "XML_SetNamespaceDeclHandler");


   procedure XML_SetStartNamespaceDeclHandler (
      parser : XML_Parser;
      start  : XML_StartNamespaceDeclHandler);  --  expat.h:613
   pragma Import (C, XML_SetStartNamespaceDeclHandler,
                    "XML_SetStartNamespaceDeclHandler");


   procedure XML_SetEndNamespaceDeclHandler (
      parser : XML_Parser;
      c_end  : XML_EndNamespaceDeclHandler);
   pragma Import (C, XML_SetEndNamespaceDeclHandler,
                    "XML_SetEndNamespaceDeclHandler");


   procedure XML_SetNotStandaloneHandler (
      parser  : XML_Parser;
      handler : XML_NotStandaloneHandler);
   pragma Import (C, XML_SetNotStandaloneHandler,
                    "XML_SetNotStandaloneHandler");


   procedure XML_SetExternalEntityRefHandler (
      parser  : XML_Parser;
      handler : XML_ExternalEntityRefHandler);  --  expat.h:625
   pragma Import (C, XML_SetExternalEntityRefHandler,
                    "XML_SetExternalEntityRefHandler");


   --  If a non-NULL value for arg is specified here, then it will be
   --  passed as the first argument to the external entity ref handler
   --  instead of the parser object.


   procedure XML_SetExternalEntityRefHandlerArg (
      parser : XML_Parser;
      arg    : Access_Void);
   pragma Import (C, XML_SetExternalEntityRefHandlerArg,
                    "XML_SetExternalEntityRefHandlerArg");


   procedure XML_SetSkippedEntityHandler (
      parser  : XML_Parser;
      handler : XML_SkippedEntityHandler);  --  expat.h:637
   pragma Import (C, XML_SetSkippedEntityHandler,
                    "XML_SetSkippedEntityHandler");


   procedure XML_SetUnknownEncodingHandler (
      parser              : XML_Parser;
      handler             : XML_UnknownEncodingHandler;
      encodingHandlerData : Access_Void);  --  expat.h:641
   pragma Import (C, XML_SetUnknownEncodingHandler,
                    "XML_SetUnknownEncodingHandler");


   --  This can be called within a handler for a start element, end
   --  element, processing instruction or character data.  It causes the
   --  corresponding markup to be passed to the default handler.

   procedure XML_DefaultCurrent (parser : XML_Parser);  --  expat.h:650
   pragma Import (C, XML_DefaultCurrent, "XML_DefaultCurrent");

   --  If do_nst is non-zero, and namespace processing is in effect, and
   --  a name has a prefix (i.e. an explicit namespace qualifier) then
   --  that name is returned as a triplet in a single string separated by
   --  the separator character specified when the parser was created: URI
   --  + sep + local_name + sep + prefix.
   --  If do_nst is zero, then namespace information is returned in the
   --  default manner (URI + sep + local_name) whether or not the name
   --  has a prefix.
   --  Note: Calling XML_SetReturnNSTriplet after XML_Parse or
   --    XML_ParseBuffer has no effect.


   procedure XML_SetReturnNSTriplet (parser : XML_Parser;
                                     do_nst : Interfaces.C.int);
   pragma Import (C, XML_SetReturnNSTriplet, "XML_SetReturnNSTriplet");


   --  This value is passed as the userData argument to callbacks.
   procedure XML_SetUserData (parser   : XML_Parser;
                              userData : Access_Void);
   pragma Import (C, XML_SetUserData, "XML_SetUserData");


   --  Returns the last value set by XML_SetUserData or NULL.
   --  This is equivalent to supplying an encoding argument to
   --  XML_ParserCreate. On success XML_SetEncoding returns non-zero,
   --  zero otherwise.
   --  Note: Calling XML_SetEncoding after XML_Parse or XML_ParseBuffer
   --        has no effect and returns XML_STATUS_ERROR.


   function XML_SetEncoding (parser   : XML_Parser;
                             encoding : Access_XML_Char) return XML_Status;
   pragma Import (C, XML_SetEncoding, "XML_SetEncoding");


   --  If this function is called, then the parser will be passed as the
   --  first argument to callbacks instead of userData.  The userData will
   --  still be accessible using XML_GetUserData.


   procedure XML_UseParserAsHandlerArg (parser : XML_Parser);  --  expat.h:690
   pragma Import (C, XML_UseParserAsHandlerArg, "XML_UseParserAsHandlerArg");


   --  If useDTD == XML_TRUE is passed to this function, then the parser
   --  will assume that there is an external subset, even if none is
   --  specified in the document. In such a case the parser will call the
   --  externalEntityRefHandler with a value of NULL for the systemId
   --  argument (the publicId and context arguments will be NULL as well).
   --  Note: For the purpose of checking WFC: Entity Declared, passing
   --     useDTD == XML_TRUE will make the parser behave as if the document
   --     had a DTD with an external subset.
   --  Note: If this function is called, then this must be done before
   --     the first call to XML_Parse or XML_ParseBuffer, since it will
   --     have no effect after that.  Returns
   --     XML_ERROR_CANT_CHANGE_FEATURE_ONCE_PARSING.
   --  Note: If the document does not have a DOCTYPE declaration at all,
   --     then startDoctypeDeclHandler and endDoctypeDeclHandler will not
   --     be called, despite an external subset being parsed.
   --  Note: If XML_DTD is not defined when Expat is compiled, returns
   --     XML_ERROR_FEATURE_REQUIRES_XML_DTD.


   function XML_UseForeignDTD (parser : XML_Parser;
                               useDTD : XML_Bool) return XML_Error;
   pragma Import (C, XML_UseForeignDTD, "XML_UseForeignDTD");


   --  Sets the base to be used for resolving relative URIs in system
   --  identifiers in declarations.  Resolving relative identifiers is
   --  left to the application: this value will be passed through as the
   --  base argument to the XML_ExternalEntityRefHandler,
   --  XML_NotationDeclHandler and XML_UnparsedEntityDeclHandler. The base
   --  argument will be copied.  Returns XML_STATUS_ERROR if out of memory,
   --  XML_STATUS_OK otherwise.


   function XML_SetBase (parser : XML_Parser;
                         base   : Access_XML_Char) return XML_Status;
   pragma Import (C, XML_SetBase, "XML_SetBase");

   function XML_GetBase (parser : XML_Parser) return Access_XML_Char;
   pragma Import (C, XML_GetBase, "XML_GetBase");


   --  Returns the number of the attribute/value pairs passed in last call
   --  to the XML_StartElementHandler that were specified in the start-tag
   --  rather than defaulted. Each attribute/value pair counts as 2; thus
   --  this correspondds to an index into the atts array passed to the
   --  XML_StartElementHandler.


   function XML_GetSpecifiedAttributeCount (parser : XML_Parser)
   return Interfaces.C.int;  --  expat.h:735
   pragma Import (C, XML_GetSpecifiedAttributeCount,
                    "XML_GetSpecifiedAttributeCount");


   --  Returns the index of the ID attribute passed in the last call to
   --  XML_StartElementHandler, or -1 if there is no ID attribute.  Each
   --  attribute/value pair counts as 2; thus this correspondds to an
   --  index into the atts array passed to the XML_StartElementHandler.


   function XML_GetIdAttributeIndex (parser : XML_Parser)
   return Interfaces.C.int;  --  expat.h:743
   pragma Import (C, XML_GetIdAttributeIndex, "XML_GetIdAttributeIndex");


   --  Parses some input. Returns XML_STATUS_ERROR if a fatal error is
   --  detected.  The last call to XML_Parse must have isFinal true; len
   --  may be zero for this call (or any other).
   --  Though the return values for these functions has always been
   --  described as a Boolean value, the implementation, at least for the
   --  1.95.x series, has always returned exactly one of the XML_Status
   --  values.


   function XML_Parse (
      parser  : XML_Parser;
      s       : Interfaces.C.Strings.chars_ptr;
      len     : Interfaces.C.int;
      isFinal : Interfaces.C.int) return XML_Status;  --  expat.h:755
   pragma Import (C, XML_Parse, "XML_Parse");


   function XML_GetBuffer (parser : XML_Parser; len : Interfaces.C.int)
   return Access_Void;  --  expat.h:758
   pragma Import (C, XML_GetBuffer, "XML_GetBuffer");


   function XML_ParseBuffer (
      parser  : XML_Parser;
      len     : Interfaces.C.int;
      isFinal : Interfaces.C.int) return XML_Status;  --  expat.h:761
   pragma Import (C, XML_ParseBuffer, "XML_ParseBuffer");


   --  Stops parsing, causing XML_Parse() or XML_ParseBuffer() to return.
   --  Must be called from within a call-back handler, except when aborting
   --  (resumable = 0) an already suspended parser. Some call-backs may
   --  still follow because they would otherwise get lost. Examples:
   --  - endElementHandler() for empty elements when stopped in
   --    startElementHandler(),
   --  - endNameSpaceDeclHandler() when stopped in endElementHandler(),
   --  and possibly others.
   --  Can be called from most handlers, including DTD related call-backs,
   --  except when parsing an external parameter entity and resumable != 0.
   --  Returns XML_STATUS_OK when successful, XML_STATUS_ERROR otherwise.
   --  Possible error codes:
   --  - XML_ERROR_SUSPENDED: when suspending an already suspended parser.
   --  - XML_ERROR_FINISHED: when the parser has already finished.
   --  - XML_ERROR_SUSPEND_PE: when suspending while parsing an external PE.
   --  When resumable != 0 (true) then parsing is suspended, that is,
   --  XML_Parse() and XML_ParseBuffer() return XML_STATUS_SUSPENDED.
   --  Otherwise parsing is aborted, that is, XML_Parse() and XML_ParseBuffer()
   --  return XML_STATUS_ERROR with error code XML_ERROR_ABORTED.
   --  *Note*:
   --  This will be applied to the current parser instance only, that is, if
   --  there is a parent parser then it will continue parsing when the
   --  externalEntityRefHandler() returns. It is up to the implementation of
   --  the externalEntityRefHandler() to call XML_StopParser() on the parent
   --  parser (recursively), if one wants to stop parsing altogether.
   --  When suspended, parsing can be resumed by calling XML_ResumeParser().


   function XML_StopParser (parser    : XML_Parser;
                            resumable : XML_Bool) return XML_Status;
   pragma Import (C, XML_StopParser, "XML_StopParser");


   --  Resumes parsing after it has been suspended with XML_StopParser().
   --  Must not be called from within a handler call-back. Returns same
   --  status codes as XML_Parse() or XML_ParseBuffer().
   --  Additional error code XML_ERROR_NOT_SUSPENDED possible.
   --  *Note*:  This must be called on the most deeply nested child parser
   --  instance first, and on its parent parser only after the child parser
   --  has finished, to be applied recursively until the document entity's
   --  parser is restarted.  That is, the parent parser will not resume by
   --  itself and it is up to the application to call XML_ResumeParser()
   --  on it at the appropriate moment.


   function XML_ResumeParser (parser : XML_Parser) return XML_Status;
   pragma Import (C, XML_ResumeParser, "XML_ResumeParser");


   type TXML_Parsing is
     (XML_INITIALIZED,
      XML_PARSING,
      XML_FINISHED,
      XML_SUSPENDED);
   pragma Convention (C, TXML_Parsing);  --  expat.h:812


   type XML_ParsingStatus is record
      parsing     : aliased TXML_Parsing;  --  expat.h:820
      finalBuffer : aliased XML_Bool;  --  expat.h:821
   end record;
   pragma Convention (C_Pass_By_Copy, XML_ParsingStatus);  --  expat.h:822


   --  skipped anonymous struct anon_46

   --  Returns status of parser with respect to being initialized, parsing,
   --  finished, or suspended and processing the final buffer.
   --  XXX XML_Parse() and XML_ParseBuffer() should return XML_ParsingStatus,
   --  XXX with XML_FINISHED_OK or XML_FINISHED_ERROR replacing XML_FINISHED


   procedure XML_GetParsingStatus (parser : XML_Parser;
                                   status : access XML_ParsingStatus);
   pragma Import (C, XML_GetParsingStatus, "XML_GetParsingStatus");


   --  Creates an XML_Parser object that can parse an external general
   --  entity; context is a '\0'-terminated string specifying the parse
   --  context; encoding is a '\0'-terminated string giving the name of
   --  the externally specified encoding, or NULL if there is no
   --  externally specified encoding.  The context string consists of a
   --  sequence of tokens separated by formfeeds (\f); a token consisting
   --  of a name specifies that the general entity of the name is open; a
   --  token of the form prefix=uri specifies the namespace for a
   --  particular prefix; a token of the form =uri specifies the default
   --  namespace.  This can be called at any point after the first call to
   --  an ExternalEntityRefHandler so longer as the parser has not yet
   --  been freed.  The new parser is completely independent and may
   --  safely be used in a separate thread.  The handlers and userData are
   --  initialized from the parser argument.  Returns NULL if out of memory.
   --  Otherwise returns a new XML_Parser object.


   function XML_ExternalEntityParserCreate (
      parser   : XML_Parser;
      context  : Access_XML_Char;
      encoding : Access_XML_Char) return XML_Parser;  --  expat.h:849
   pragma Import (C, XML_ExternalEntityParserCreate,
                    "XML_ExternalEntityParserCreate");


   type XML_ParamEntityParsing is (
      XML_PARAM_ENTITY_PARSING_NEVER,
      XML_PARAM_ENTITY_PARSING_UNLESS_STANDALONE,
      XML_PARAM_ENTITY_PARSING_ALWAYS);
   pragma Convention (C, XML_ParamEntityParsing);  --  expat.h:853


   --  Controls parsing of parameter entities (including the external DTD
   --  subset). If parsing of parameter entities is enabled, then
   --  references to external parameter entities (including the external
   --  DTD subset) will be passed to the handler set with
   --  XML_SetExternalEntityRefHandler.  The context passed will be 0.
   --  Unlike external general entities, external parameter entities can
   --  only be parsed synchronously.  If the external parameter entity is
   --  to be parsed, it must be parsed during the call to the external
   --  entity ref handler: the complete sequence of
   --  XML_ExternalEntityParserCreate, XML_Parse/XML_ParseBuffer and
   --  XML_ParserFree calls must be made during this call.  After
   --  XML_ExternalEntityParserCreate has been called to create the parser
   --  for the external parameter entity (context must be 0 for this
   --  call), it is illegal to make any calls on the old parser until
   --  XML_ParserFree has been called on the newly created parser.
   --  If the library has been compiled without support for parameter
   --  entity parsing (ie without XML_DTD being defined), then
   --  XML_SetParamEntityParsing will return 0 if parsing of parameter
   --  entities is requested; otherwise it will return non-zero.
   --  Note: If XML_SetParamEntityParsing is called after XML_Parse or
   --     XML_ParseBuffer, then it has no effect and will always return 0.


   function XML_SetParamEntityParsing (
      parser  : XML_Parser;
      parsing : XML_ParamEntityParsing)
   return Interfaces.C.int;  --  expat.h:883
   pragma Import (C, XML_SetParamEntityParsing, "XML_SetParamEntityParsing");


   --  If XML_Parse or XML_ParseBuffer have returned XML_STATUS_ERROR, then
   --  XML_GetErrorCode returns information about the error.


   function XML_GetErrorCode (parser : XML_Parser) return XML_Error;
   pragma Import (C, XML_GetErrorCode, "XML_GetErrorCode");


   --  These functions return information about the current parse
   --  location.  They may be called from any callback called to report
   --  some parse event; in this case the location is the location of the
   --  first of the sequence of characters that generated the event.  When
   --  called from callbacks generated by declarations in the document
   --  prologue, the location identified isn't as neatly defined, but will
   --  be within the relevant markup.  When called outside of the callback
   --  functions, the position indicated will be just past the last parse
   --  event (regardless of whether there was an associated callback).
   --
   --  They may also be called after returning from a call to XML_Parse
   --  or XML_ParseBuffer.  If the return value is XML_STATUS_ERROR then
   --  the location is the location of the character at which the error
   --  was detected; otherwise the location is the location of the last
   --  parse event, as described above.


   function XML_GetCurrentLineNumber (parser : XML_Parser) return XML_Size;
   pragma Import (C, XML_GetCurrentLineNumber, "XML_GetCurrentLineNumber");

   function XML_GetCurrentColumnNumber (parser : XML_Parser) return XML_Size;
   pragma Import (C, XML_GetCurrentColumnNumber, "XML_GetCurrentColumnNumber");

   function XML_GetCurrentByteIndex (parser : XML_Parser) return XML_Index;
   pragma Import (C, XML_GetCurrentByteIndex, "XML_GetCurrentByteIndex");

   --  Return the number of bytes in the current event.
   --  Returns 0 if the event is in an internal entity.


   function XML_GetCurrentByteCount (parser : XML_Parser)
   return Interfaces.C.int;  --  expat.h:916
   pragma Import (C, XML_GetCurrentByteCount, "XML_GetCurrentByteCount");


   --  If XML_CONTEXT_BYTES is defined, returns the input buffer, sets
   --  the integer pointed to by offset to the offset within this buffer
   --  of the current parse position, and sets the integer pointed to by size
   --  to the size of this buffer (the number of input bytes). Otherwise
   --  returns a NULL pointer. Also returns a NULL pointer if a parse isn't
   --  active.
   --  NOTE: The character pointer returned should not be used outside
   --  the handler that makes the call.


   function XML_GetInputContext (
      parser : XML_Parser;
      offset : access Interfaces.C.int;
      size   : access Interfaces.C.int)
   return Interfaces.C.Strings.chars_ptr;
   pragma Import (C, XML_GetInputContext, "XML_GetInputContext");


   --  For backwards compatibility with previous versions.
   --  Frees the content model passed to the element declaration handler
   procedure XML_FreeContentModel (parser : XML_Parser; model : access XML_cp);
   pragma Import (C, XML_FreeContentModel, "XML_FreeContentModel");


   --  Exposing the memory handling functions used in Expat
   function XML_MemMalloc (parser : XML_Parser; size : size_t)
   return Access_Void;  --  expat.h:944
   pragma Import (C, XML_MemMalloc, "XML_MemMalloc");


   function XML_MemRealloc
     (parser : XML_Parser;
      ptr    : Access_Void;
      size   : size_t) return Access_Void;  --  expat.h:947
   pragma Import (C, XML_MemRealloc, "XML_MemRealloc");


   procedure XML_MemFree (parser : XML_Parser; ptr : Access_Void);
   pragma Import (C, XML_MemFree, "XML_MemFree");


   --  Frees memory used by the parser.
   procedure XML_ParserFree (parser : XML_Parser);  --  expat.h:954
   pragma Import (C, XML_ParserFree, "XML_ParserFree");


   type XML_FeatureEnum is
     (XML_FEATURE_END,
      XML_FEATURE_UNICODE,
      XML_FEATURE_UNICODE_WCHAR_T,
      XML_FEATURE_DTD,
      XML_FEATURE_CONTEXT_BYTES,
      XML_FEATURE_MIN_SIZE,
      XML_FEATURE_SIZEOF_XML_CHAR,
      XML_FEATURE_SIZEOF_XML_LCHAR,
      XML_FEATURE_NS,
      XML_FEATURE_LARGE_SIZE);
   pragma Convention (C, XML_FeatureEnum);  --  expat.h:977

   --  Additional features must be added to the end of this enum.
   type XML_Feature is record
      feature : aliased XML_FeatureEnum;   --  expat.h:992
      name    : Access_XML_Char;            --  expat.h:993
      value   : aliased Interfaces.C.long;  --  expat.h:994
   end record;
   pragma Convention (C_Pass_By_Copy, XML_Feature);  --  expat.h:995

   --  skipped anonymous struct anon_48

   function XML_GetFeatureList return access constant XML_Feature;
   pragma Import (C, XML_GetFeatureList, "XML_GetFeatureList");

   --  Expat follows the GNU/Linux convention of odd number minor version for
   --  beta/development releases and even number minor version for stable
   --  releases. Micro is bumped with each release, and set to 0 with each
   --  change to major or minor version.


   function XML_ExpatVersion return String;
   --  Return a string containing the version number of this expat


   function XML_ErrorString (parser : XML_Parser) return String;
     --  Returns a string describing the error.


   type XML_Expat_Version is record
      major : Integer;
      minor : Integer;
      micro : Integer;
   end record;


   function XML_ExpatVersionInfo return XML_Expat_Version;
   --  Return an XML_Expat_Version structure containing numeric version
   --  number information for this version of expat.   [expat.h:974]

private

   type Thin_XML_Expat_Version is record
      major : aliased Interfaces.C.int;  --  expat.h:965
      minor : aliased Interfaces.C.int;  --  expat.h:966
      micro : aliased Interfaces.C.int;  --  expat.h:967
   end record;
   pragma Convention (C_Pass_By_Copy, Thin_XML_Expat_Version);  --  expat.h:968


   function Thin_XML_ExpatVersion return Access_XML_Char;  --  expat.h:962
   pragma Import (C, Thin_XML_ExpatVersion, "XML_ExpatVersion");
   --  Return a string containing the version number of this expat


   function Thin_XML_ErrorString (code : XML_Error) return Access_XML_Char;
   pragma Import (C, Thin_XML_ErrorString, "XML_ErrorString");
   --  Returns a string describing the error.


   function Thin_XML_ExpatVersionInfo return Thin_XML_Expat_Version;
   pragma Import (C, Thin_XML_ExpatVersionInfo, "XML_ExpatVersionInfo");
   --  Return an XML_Expat_Version structure containing numeric version
   --  number information for this version of expat.   [expat.h:974]


end expat;
