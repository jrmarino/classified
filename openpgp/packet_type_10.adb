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

package body Packet_Type_10 is

   --------------------------------
   --  Construct_Type_13_Packet  --
   --------------------------------

   function Construct_Type_10_Packet
   return TOctet_Array is
      result      : TOctet_Array (0 .. 4);
   begin
      result (0) := Convert_Packet_Tag_To_Octet (Marker);
      result (1) := 3;
      result (2) := 16#50#;  -- P
      result (3) := 16#47#;  -- G
      result (4) := 16#50#;  -- P
      return result;

   end Construct_Type_10_Packet;

end Packet_Type_10;

