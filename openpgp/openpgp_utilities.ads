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

package OpenPGP_Utilities is

   function MPI_Byte_Size (Octet_1 : TOctet;
                           Octet_2 : TOctet) return Natural;
   --  Two octets are passes to this function.  They needed to be considered as
   --  Big Endian, meaning Octet_1 is multiplied by 256.
   --  The result is ( Octet_1 * 256 + Octet2 + 7 ) / 8 and represents the
   --  number of bytes (Octets) that the MPI occupies.


   function MPI_Total_Bits (Octet_1 : TOctet;
                            Octet_2 : TOctet) return Natural;
   --  Two octets are passes to this function.  They needed to be considered as
   --  Big Endian, meaning Octet_1 is multiplied by 256.
   --  The result is  Octet_1 * 256 + Octet2  and represents the
   --  number of significant bits that compose the MPI.

end OpenPGP_Utilities;

