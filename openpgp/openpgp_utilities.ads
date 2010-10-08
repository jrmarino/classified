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
with Ada.Strings.Unbounded;

package OpenPGP_Utilities is


   package SU renames Ada.Strings.Unbounded;


   function MPI_Byte_Size (Octet_1 : TOctet;
                           Octet_2 : TOctet) return Natural;
   --  Two octets are passed to this function.  They needed to be considered as
   --  Big Endian, meaning Octet_1 is multiplied by 256.
   --  The result is ( Octet_1 * 256 + Octet2 + 7 ) / 8 and represents the
   --  number of bytes (Octets) that the MPI occupies.


   function MPI_Total_Bits (Octet_1 : TOctet;
                            Octet_2 : TOctet) return Natural;
   --  Two octets are passed to this function.  They needed to be considered as
   --  Big Endian, meaning Octet_1 is multiplied by 256.
   --  The result is  Octet_1 * 256 + Octet2  and represents the
   --  number of significant bits that compose the MPI.


   function Construct_Unix_Time (Octet_1 : TOctet;
                                 Octet_2 : TOctet;
                                 Octet_3 : TOctet;
                                 Octet_4 : TOctet) return TUnixTime;
   --  Four octets are passed to this function.  They are considered to be in
   --  Big Endian format, so the bytes are shifted by 24, 16, 8, and 0
   --  respectively to form a 32-bit integer.


   function Convert_Octet_To_PK_Algorithm (Octet : TOctet)
   return TPubKey_Algorithm;
   --  This function converts an octet into a known public key algorithm ID.
   --  If the provided octet is unknown, the value "Undefined" is returned.


   function Convert_Octet_To_Hash_ID (Octet : TOctet)
   return THash_Algorithm;
   --  This function converts an octet into a known hash algorithm ID.
   --  If the provided octet is unknown, the value "Undefined" is returned.


   function Convert_Hash_ID_To_Octet (hash_Algorithm : THash_Algorithm)
   return TOctet;
   --  This function converts a hash algorithm ID into an octet.  If the
   --  hash is set to Undefined, then the return value is set to zero.


   function Two_Octet_Length (Octet_1 : TOctet;
                              Octet_2 : TOctet) return TBody_Length;
   --  This calculates the length of the following data using two octets.  The
   --  calling code must have already determined it's a 2-octet length.


   function Four_Octet_Length (Octet_1 : TOctet;
                               Octet_2 : TOctet;
                               Octet_3 : TOctet;
                               Octet_4 : TOctet) return TBody_Length;
   --  This calculates the length of the following data using four octets.  The
   --  calling code must have already determined it's a 4-octet length.


   function Encode_Body_Length (Body_Length : TBody_Length) return TOctet_Array;
   --  Depending on the value of Body_Length, this function will return the
   --  equivalent value in 1, 2, or 5 octets.


   function Breakdown_Unix_Time (UnixTime : TUnixTime) return TOctet_Array;
   --  This disintegrates a 32-bit integer representing the unix timestamp into
   --  a 4-element octet array (complement of Construct_Unix_Time).


   function Convert_Packet_Tag_To_Octet (Packet_Tag : TPacket_Tag)
   return TOctet;
   --  This function accepts a packet tag enumeration and returns the
   --  corresponding octet value.  Undefined is given the value of 0, which
   --  is considered reserved, do not use.


   function convert_octet_array_to_string (Block : TOctet_Array) return String;
   --  This function will convert an array of octets to a UTF-8 string.


   function convert_octet_array_to_unbounded_string (Block : TOctet_Array)
   return SU.Unbounded_String;
   --  This function will convert an array of octets to an unbounded string.


   function convert_string_to_octet_array (data : String) return TOctet_Array;
   --  This function converts a string to an array of octets.


   function convert_octet_to_compression_algorithm (Octet : TOctet)
   return TCompression_Algorithm;
   --  This function takes an octet and returns the equivalent compression
   --  algorithm.  Unrecognized values are set to "Undefined" enumeration.


   function convert_compression_algorithm_to_octet
                                       (Algorithm : TCompression_Algorithm)
   return TOctet;
   --  This funciton takes a compression algorithm enumeration and returns the
   --  associated octet value.


   function mpi_byte_size (SU_MPI : TSU_MPI) return Natural;
   --  This function returns the number of bytes it would require to store the
   --  multiple precision integer into an array of octets.  If a zero is
   --  returned, that should be interpreted that the mpi is malformed because
   --  less than 3 octets were used to represent, or the leading bits that
   --  should have been zero weren't.


   function convert_mpi_to_octet_array (SU_MPI        : TSU_MPI;
                                        Number_Octets : Positive)
   return TOctet_Array;
   --  This function takes openPGP multi-precision integers which are stored
   --  insided binary-safe unbounded strings and converts them into an array
   --  of octets.  Both the input and output are big-endian formatted.


   function extract_mpi (start_index  : Natural;
                         octet_array  : TOctet_Array)
   return SU.Unbounded_String;
   --  This function is passed an octet array along with the index in which
   --  to look for an MPI.  It will return that MPI within an unbounded string
   --  sized appropriately.  If there isn't another data to accomplish list,
   --  a null-sized string is returned.

end OpenPGP_Utilities;

