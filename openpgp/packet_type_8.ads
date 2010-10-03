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

package Packet_Type_8 is

   function Retrieve_Compression_Algoritm (Header : TPacket_Header;
                                           Packet : TOctet_Array)
   return TCompression_Algorithm;
   --  Returns the enumeration of the compressed used.


   function Retrieve_Compressed_Data (Header : TPacket_Header;
                                      Packet : TOctet_Array)
   return TOctet_Array;
   --  Returns the compressed data inside packet type 8.


   function Construct_Type_8_Packet (Data_Compressed : TOctet_Array;
                                     Algorithm       : TCompression_Algorithm)
   return TOctet_Array;
   --  This function returns a complete type 8 compressed data packet.

end Packet_Type_8;
