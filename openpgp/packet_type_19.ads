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


with OpenPGP_Types; use OpenPGP_Types;

package Packet_Type_19 is

   type TP19_Error is (
      no_error,
      body_too_short,
      body_too_long
   );

   subtype TSHA1_Hash is TOctet_Array (0 .. 19);
   type TPacket_19_Fixed is record
      SHA1_Hash : TSHA1_Hash := (others => 0);
      Error     : TP19_Error := no_error;
   end record;

   function Scan_Packet (Header : TPacket_Header;
                         Packet : TOctet_Array) return TPacket_19_Fixed;
   --  Returns the data inside the Modification Detection Code Packet that is
   --  a fixed size type.  Currently there is no variable size data for
   --  Packet type 19.


   function Construct_Type_19_Packet (SHA1_Hash : TSHA1_Hash)
   return TOctet_Array;
   --  This function returns a complete type 19 packet, which currently only
   --  contains one data item (a 20-character SHA-1 hash).

end Packet_Type_19;
