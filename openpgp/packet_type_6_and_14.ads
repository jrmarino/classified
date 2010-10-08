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

package Packet_Type_6_and_14 is

   type TP614_Error is (
      no_error,
      invalid_version_number,
      illegal_algorithm,
      body_too_short
   );

   type TPubKeyVersion is (two_and_three, four, illegal);
   subtype TDays_Valid is Natural range 0 .. 2 ** 16 - 1;


   type TPacket_614_234_RSA is record
      Version       : TPubKeyVersion    := illegal;
      Creation_Time : TUnixTime         := 0;
      Days_Valid    : TDays_Valid       := 0;
      Algorithm     : TPubKey_Algorithm := Undefined;
      RSA_modulus   : TSU_MPI           := SU.Null_Unbounded_String;
      RSA_Exponent  : TSU_MPI           := SU.Null_Unbounded_String;
      Error         : TP614_Error       := no_error;
   end record;


   type TPacket_614_4_DSA is record
      Creation_Time     : TUnixTime         := 0;
      DSA_prime_p       : TSU_MPI           := SU.Null_Unbounded_String;
      DSA_group_order_q : TSU_MPI           := SU.Null_Unbounded_String;
      DSA_group_gen_g   : TSU_MPI           := SU.Null_Unbounded_String;
      DSA_pubkey_val_y  : TSU_MPI           := SU.Null_Unbounded_String;
      Error             : TP614_Error       := no_error;
   end record;

   type TPacket_614_4_Elgamal is record
      Creation_Time        : TUnixTime         := 0;
      Elgamal_prime_p      : TSU_MPI           := SU.Null_Unbounded_String;
      Elgamal_group_gen_g  : TSU_MPI           := SU.Null_Unbounded_String;
      Elgamal_pubkey_val_y : TSU_MPI           := SU.Null_Unbounded_String;
      Error                : TP614_Error       := no_error;
   end record;


   procedure Metadata (Header    : in  TPacket_Header;
                       Packet    : in  TOctet_Array;
                       Version   : out TPubKeyVersion;
                       Algorithm : out TPubKey_Algorithm);
   --  Determines the packet version number (needs to be 2, 3, or 4) and also
   --  which Algorithm is uses so a different record can be specified.


   function Get_Public_RSA_Key (Header : TPacket_Header;
                                Packet : TOctet_Array)
   return TPacket_614_234_RSA;
   --  Returns a record containing the public RSA key.  The same record is used
   --  for versions 2, 3, and 4.  For version 4, the days valid field is left
   --  at zero because it's not used in this version.


   function Get_Public_DSA_Key (Header : TPacket_Header;
                                Packet : TOctet_Array) return TPacket_614_4_DSA;
   --  Returns a record containing the public DSA key.  Only version 4 is
   --  accepted at this time.


   function Get_Public_Elgamal_key (Header : TPacket_Header;
                                    Packet : TOctet_Array)
   return TPacket_614_4_Elgamal;
   --  Returns a record containing the public Elgamal key.  Only version 4 is
   --  accepted at this time.


   function Construct_Type_614_RSA_Packet (Value_n   : TMPI;
                                           Value_e   : TMPI;
                                           Timestamp : TUnixTime;
                                           Subpacket : Boolean)
   return TOctet_Array;
   --  This function returns a complete type 18 packet, which currently only
   --  contains one significant data item, encrypted data.


   function Construct_Type_614_DSA_Packet (Value_p   : TMPI;
                                           Value_q   : TMPI;
                                           Value_g   : TMPI;
                                           Value_y   : TMPI;
                                           Timestamp : TUnixTime;
                                           Subpacket : Boolean)
   return TOctet_Array;
   --  This function returns a complete type 18 packet, which currently only
   --  contains one significant data item, encrypted data.

   function Construct_Type_614_Elgamal_Packet (Value_p   : TMPI;
                                               Value_g   : TMPI;
                                               Value_y   : TMPI;
                                               Timestamp : TUnixTime;
                                               Subpacket : Boolean)
   return TOctet_Array;
   --  This function returns a complete type 18 packet, which currently only
   --  contains one significant data item, encrypted data.

end Packet_Type_6_and_14;
