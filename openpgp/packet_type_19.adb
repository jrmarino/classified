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


with OpenPGP_Utilities; use OpenPGP_Utilities;

package body Packet_Type_19 is


   -------------------
   --  Scan_Packet  --
   -------------------

   function Scan_Packet (Header : TPacket_Header;
                         Packet : TOctet_Array)
   return TPacket_19_Fixed is
      index : constant Natural := Header.Body_Starts;
      result : TPacket_19_Fixed;
   begin
      if Header.Body_Length < 20 then
         result.Error := body_too_short;
         return result;
      elsif Header.Body_Length > 20 then
         result.Error := body_too_long;
         return result;
      end if;

      result.Error     := no_error;
      result.SHA1_Hash := TSHA1_Hash (
                              Packet (index .. index + TSHA1_Hash'Last));
      return result;

   end Scan_Packet;



   --------------------------------
   --  Construct_Type_19_Packet  --
   --------------------------------

   function Construct_Type_19_Packet (SHA1_Hash : TSHA1_Hash)
   return TOctet_Array is
      result : TOctet_Array (0 .. 21);
   begin
      result (0) := Convert_Packet_Tag_To_Octet (Modification_Detection_Code);
      result (1) := 20;
      result (2 .. 21) := SHA1_Hash;
      return result;
   end Construct_Type_19_Packet;


end Packet_Type_19;

