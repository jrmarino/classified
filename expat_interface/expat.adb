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


package body expat is


   ------------------------
   --  XML_ExpatVersion  --
   ------------------------

   function XML_ExpatVersion
   return String is
      work_ptr : constant Access_XML_Char := Thin_XML_ExpatVersion;
      result   : constant String := Interfaces.C.Strings.Value (work_ptr);
   begin
      return result;
   end XML_ExpatVersion;



   -----------------------
   --  XML_ErrorString  --
   -----------------------

   function XML_ErrorString (parser : XML_Parser)
   return String is
      use Interfaces.C.Strings;
      errcode  : constant XML_Error := XML_GetErrorCode (parser => parser);
      work_ptr : constant Access_XML_Char :=
                          Thin_XML_ErrorString (code => errcode);
      no_error : constant String := "No XML errors present";
      no_msg   : constant Access_XML_Char := Null_Ptr;
   begin

      if work_ptr = no_msg then
         return no_error;
      end if;
      return Interfaces.C.Strings.Value (work_ptr);

   end XML_ErrorString;



   ----------------------------
   --  XML_ExpatVersionInfo  --
   ----------------------------

   function XML_ExpatVersionInfo
   return XML_Expat_Version is
      work   : constant Thin_XML_Expat_Version := Thin_XML_ExpatVersionInfo;
      result : XML_Expat_Version;
   begin
      result.major := Integer (work.major);
      result.minor := Integer (work.minor);
      result.micro := Integer (work.micro);
      return result;
   end XML_ExpatVersionInfo;


end expat;

