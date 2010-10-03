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

package Packet_Type_18 is

   function Retreive_Version (Header : TPacket_Header;
                              Packet : TOctet_Array)
   return Natural;
   --  Returns the version of the type 18 packet.


   function Retrieve_Encrypted_Data (Header : TPacket_Header;
                                     Packet : TOctet_Array)
   return TOctet_Array;
   --  Returns the encrypted data inside packet type 18.


   function Construct_Type_18_Packet (Encrypted_Data : TOctet_Array)
   return TOctet_Array;
   --  This function returns a complete type 18 packet, which currently only
   --  contains one significant data item, encrypted data.

end Packet_Type_18;
