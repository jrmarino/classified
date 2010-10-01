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

package Packet_Type_1 is

   type TP1_Error is (
      no_error,
      invalid_version_number,
      illegal_algorithm,
      body_too_short,
      session_too_long,
      session_too_short,
      session2_too_long,
      session2_too_short,
      MPI_1_missing_data,
      MPI_2_missing_data
   );

   type TPacket_1_Fixed is record
      Version   : TVersion          := 0;
      KeyID     : TKeyID            := (others => 0);
      Algorithm : TPubKey_Algorithm := Undefined;
      Error     : TP1_Error         := no_error;
   end record;

   function Scan_Packet (Header : TPacket_Header;
                         Packet : TOctet_Array) return TPacket_1_Fixed;
   --  Returns the data inside the public key session packet that is a fixed
   --  size type.  Variable size types need to be individually retrieved.


   function Retrieve_RSA_men return TMPI;
   --  For RSA keys, this function returns the MPI value of m^e mod n.


   function Retrieve_Elgamal_gkp return TMPI;
   --  For Elgamal keys, this function returns the MPI value of g^k mod p


   function Retrieve_Elgamal_mykp return TMPI;
   --  For Elgamal keys, this function return the MPI value of my^k mod p


private

   subtype TRaw_Data is TOctet_Array (0 .. 511);
   --  Maximum size for RSA key is 4096 bits (512 Octets)
   --  Maximum size for Elgamal is 2048 bits (256 Octets)

   Raw_data : TRaw_Data := (others => 0);
   MPI_1st  : TSegment;
   MPI_2nd  : TSegment;

   function First_TMPI return TMPI;
   --  This function interprets the first segment as an MPI and returns it.

   function Second_TMPI return TMPI;
   --  This function interprets the second segment as an MPI and returns it.

end Packet_Type_1;
