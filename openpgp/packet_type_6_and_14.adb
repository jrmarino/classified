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


with OpenPGP_Utilities;  use OpenPGP_Utilities;

package body Packet_Type_6_and_14 is

   ----------------
   --  Metadata  --
   ----------------

   procedure Metadata (Header    : in  TPacket_Header;
                       Packet    : in  TOctet_Array;
                       Version   : out TPubKeyVersion;
                       Algorithm : out TPubKey_Algorithm)
   is
      index : constant Natural := Header.Body_Starts;
   begin
      Version   := illegal;
      Algorithm := Undefined;
      if Header.Body_Length < 2 then
         return;
      end if;

      case Packet (index) is
         when  2 | 3 => Version := two_and_three;
         when      4 => Version := four;
         when others => null;
      end case;
      if Version = two_and_three then
         Algorithm := Convert_Octet_To_PK_Algorithm (Packet (index + 7));
      else
         Algorithm := Convert_Octet_To_PK_Algorithm (Packet (index + 5));
      end if;
   end Metadata;



   --------------------------
   --  Get_Public_RSA_Key  --
   --------------------------

   function Get_Public_RSA_Key (Header : TPacket_Header;
                                Packet : TOctet_Array)
   return TPacket_614_234_RSA is
      index    : constant Natural := Header.Body_Starts;
      part2    : Natural;
      part3    : Natural;
      result   : TPacket_614_234_RSA;

      use SU;
   begin
      if Header.Body_Length < 14 then
         result.Error := body_too_short;
         return result;
      end if;

      case Packet (index) is
         when  2 | 3 => result.Version := two_and_three;
         when      4 => result.Version := four;
         when others => result.Error   := invalid_version_number;
                        return result;
      end case;

      result.Creation_Time := Construct_Unix_Time (
                                 Octet_1 => Packet (index + 1),
                                 Octet_2 => Packet (index + 2),
                                 Octet_3 => Packet (index + 3),
                                 Octet_4 => Packet (index + 4));

      if result.Version = two_and_three then
         result.Days_Valid := Natural (Packet (index + 5)) * 256 +
                              Natural (Packet (index + 6));
         part2 := index + 7;
      else
         part2 := index + 5;
      end if;

      result.Algorithm := Convert_Octet_To_PK_Algorithm (Packet (part2));
      case result.Algorithm is
         when RSA_Encrypt_Only |
              RSA_Encrypt_Or_Sign |
              RSA_Sign_Only => null;
         when others        => result.Error := illegal_algorithm;
                               return result;
      end case;

      result.RSA_modulus := extract_mpi (start_index => part2 + 1,
                                         octet_array => Packet);
      if result.RSA_modulus = SU.Null_Unbounded_String then
         result.Error := body_too_short;
         return result;
      end if;
      part3 := part2 + 1 + SU.Length (result.RSA_modulus);

      result.RSA_Exponent := extract_mpi (start_index => part3,
                                         octet_array => Packet);
      if result.RSA_Exponent = SU.Null_Unbounded_String then
         result.Error := body_too_short;
         return result;
      end if;

      return result;

   end Get_Public_RSA_Key;



   --------------------------
   --  Get_Public_DSA_Key  --
   --------------------------

   function Get_Public_DSA_Key (Header : TPacket_Header;
                                Packet : TOctet_Array)
   return TPacket_614_4_DSA is
      index     : Natural := Header.Body_Starts;
      result    : TPacket_614_4_DSA;
      algorithm : TPubKey_Algorithm;

      use SU;
   begin
      if Header.Body_Length < 18 then
         result.Error := body_too_short;
         return result;
      end if;

      case Packet (index) is
         when      4 => null;
         when others => result.Error   := invalid_version_number;
                        return result;
      end case;

      result.Creation_Time := Construct_Unix_Time (
                                 Octet_1 => Packet (index + 1),
                                 Octet_2 => Packet (index + 2),
                                 Octet_3 => Packet (index + 3),
                                 Octet_4 => Packet (index + 4));

      algorithm := Convert_Octet_To_PK_Algorithm (Packet (index + 5));
      case algorithm is
         when DSA    => null;
         when others => result.Error := illegal_algorithm;
                        return result;
      end case;
      index := index + 6;



      result.DSA_prime_p := extract_mpi (start_index => index + 1,
                                         octet_array => Packet);
      if result.DSA_prime_p = SU.Null_Unbounded_String then
         result.Error := body_too_short;
         return result;
      end if;
      index := index + SU.Length (result.DSA_prime_p);



      result.DSA_group_order_q := extract_mpi (start_index => index + 1,
                                               octet_array => Packet);
      if result.DSA_group_order_q = SU.Null_Unbounded_String then
         result.Error := body_too_short;
         return result;
      end if;
      index := index + SU.Length (result.DSA_group_order_q);



      result.DSA_group_gen_g := extract_mpi (start_index => index + 1,
                                             octet_array => Packet);
      if result.DSA_group_gen_g = SU.Null_Unbounded_String then
         result.Error := body_too_short;
         return result;
      end if;
      index := index + SU.Length (result.DSA_group_gen_g);



      result.DSA_pubkey_val_y := extract_mpi (start_index => index + 1,
                                              octet_array => Packet);
      if result.DSA_pubkey_val_y = SU.Null_Unbounded_String then
         result.Error := body_too_short;
         return result;
      end if;

      return result;

   end Get_Public_DSA_Key;




   ------------------------------
   --  Get_Public_Elgamal_key  --
   ------------------------------

   function Get_Public_Elgamal_key (Header : TPacket_Header;
                                    Packet : TOctet_Array)
   return TPacket_614_4_Elgamal is
      index     : Natural := Header.Body_Starts;
      result    : TPacket_614_4_Elgamal;
      algorithm : TPubKey_Algorithm;

      use SU;
   begin
      if Header.Body_Length < 15 then
         result.Error := body_too_short;
         return result;
      end if;

      case Packet (index) is
         when      4 => null;
         when others => result.Error   := invalid_version_number;
                        return result;
      end case;

      result.Creation_Time := Construct_Unix_Time (
                                 Octet_1 => Packet (index + 1),
                                 Octet_2 => Packet (index + 2),
                                 Octet_3 => Packet (index + 3),
                                 Octet_4 => Packet (index + 4));

      algorithm := Convert_Octet_To_PK_Algorithm (Packet (index + 5));
      case algorithm is
         when Elgamal_Encrypt_Only => null;
         when others               => result.Error := illegal_algorithm;
                                      return result;
      end case;
      index := index + 6;

      result.Elgamal_prime_p := extract_mpi (start_index => index + 1,
                                             octet_array => Packet);
      if result.Elgamal_prime_p = SU.Null_Unbounded_String then
         result.Error := body_too_short;
         return result;
      end if;
      index := index + SU.Length (result.Elgamal_prime_p);



      result.Elgamal_group_gen_g := extract_mpi (start_index => index + 1,
                                                 octet_array => Packet);
      if result.Elgamal_group_gen_g = SU.Null_Unbounded_String then
         result.Error := body_too_short;
         return result;
      end if;
      index := index + SU.Length (result.Elgamal_group_gen_g);


      result.Elgamal_pubkey_val_y := extract_mpi (start_index => index + 1,
                                                  octet_array => Packet);
      if result.Elgamal_pubkey_val_y = SU.Null_Unbounded_String then
         result.Error := body_too_short;
         return result;
      end if;

      return result;

   end Get_Public_Elgamal_key;



   -------------------------------------
   --  Construct_Type_614_RSA_Packet  --
   -------------------------------------

   function Construct_Type_614_RSA_Packet (Value_n   : TMPI;
                                           Value_e   : TMPI;
                                           Timestamp : TUnixTime;
                                           Subpacket : Boolean)
   return TOctet_Array is
      Body_Length : constant TBody_Length := Value_n'Length +
                                             Value_e'Length + 6;
      enclen      : constant TOctet_Array := Encode_Body_Length (Body_Length);

      Last_Index : constant Natural := Natural (Body_Length) + enclen'Length;
      Result     : TOctet_Array (0 .. Last_Index) := (others => 0);
      index      : Natural;
      M1_Index   : Natural;
   begin

      if Subpacket then
         Result (0) := Convert_Packet_Tag_To_Octet (Public_Subkey);
      else
         Result (0) := Convert_Packet_Tag_To_Octet (Public_key);
      end if;
      Result (1 .. enclen'Length)        := enclen;
      index                              := enclen'Length + 1;
      M1_Index                           := index + 6 + Value_n'Length;

      Result (index)                     := 4;     --  Version 4
      Result (index + 1 .. index + 4)    := Breakdown_Unix_Time (Timestamp);
      Result (index + 5)                 := 1;     -- RSA_Encrypt_Or_Sign
      Result (index + 6 .. M1_Index - 1) := Value_n;
      Result (M1_Index .. Last_Index)    := Value_e;
      return Result;

   end Construct_Type_614_RSA_Packet;



   -------------------------------------
   --  Construct_Type_614_DSA_Packet  --
   -------------------------------------

   function Construct_Type_614_DSA_Packet (Value_p   : TMPI;
                                           Value_q   : TMPI;
                                           Value_g   : TMPI;
                                           Value_y   : TMPI;
                                           Timestamp : TUnixTime;
                                           Subpacket : Boolean)
   return TOctet_Array is
      Body_Length : constant TBody_Length := Value_p'Length +
                                             Value_q'Length +
                                             Value_g'Length +
                                             Value_y'Length + 6;
      enclen      : constant TOctet_Array := Encode_Body_Length (Body_Length);

      Last_Index : constant Natural := Natural (Body_Length) + enclen'Length;
      Result     : TOctet_Array (0 .. Last_Index) := (others => 0);
      index      : Natural;
      M1_Index   : Natural;
      M2_Index   : Natural;
      M3_Index   : Natural;
   begin

      if Subpacket then
         Result (0) := Convert_Packet_Tag_To_Octet (Public_Subkey);
      else
         Result (0) := Convert_Packet_Tag_To_Octet (Public_key);
      end if;
      Result (1 .. enclen'Length)        := enclen;
      index                              := enclen'Length + 1;
      M1_Index                           := index + 6 + Value_p'Length;
      M2_Index                           := M1_Index + Value_q'Length;
      M3_Index                           := M2_Index + Value_g'Length;

      Result (index)                     := 4;     --  Version 4
      Result (index + 1 .. index + 4)    := Breakdown_Unix_Time (Timestamp);
      Result (index + 5)                 := 17;     -- DSA
      Result (index + 6 .. M1_Index - 1) := Value_p;
      Result (M1_Index .. M2_Index - 1)  := Value_q;
      Result (M2_Index .. M3_Index - 1)  := Value_g;
      Result (M3_Index .. Last_Index)    := Value_y;
      return Result;

   end Construct_Type_614_DSA_Packet;



   -----------------------------------------
   --  Construct_Type_614_Elgamal_Packet  --
   -----------------------------------------

   function Construct_Type_614_Elgamal_Packet (Value_p   : TMPI;
                                               Value_g   : TMPI;
                                               Value_y   : TMPI;
                                               Timestamp : TUnixTime;
                                               Subpacket : Boolean)
   return TOctet_Array is
      Body_Length : constant TBody_Length := Value_p'Length +
                                             Value_g'Length +
                                             Value_y'Length + 6;
      enclen      : constant TOctet_Array := Encode_Body_Length (Body_Length);

      Last_Index : constant Natural := Natural (Body_Length) + enclen'Length;
      Result     : TOctet_Array (0 .. Last_Index) := (others => 0);
      index      : Natural;
      M1_Index   : Natural;
      M2_Index   : Natural;
   begin

      if Subpacket then
         Result (0) := Convert_Packet_Tag_To_Octet (Public_Subkey);
      else
         Result (0) := Convert_Packet_Tag_To_Octet (Public_key);
      end if;
      Result (1 .. enclen'Length)        := enclen;
      index                              := enclen'Length + 1;
      M1_Index                           := index + 6 + Value_p'Length;
      M2_Index                           := M1_Index + Value_g'Length;

      Result (index)                     := 4;     --  Version 4
      Result (index + 1 .. index + 4)    := Breakdown_Unix_Time (Timestamp);
      Result (index + 5)                 := 16;     -- Elgamal_Encrypt_Only
      Result (index + 6 .. M1_Index - 1) := Value_p;
      Result (M1_Index .. M2_Index - 1)  := Value_g;
      Result (M2_Index .. Last_Index)    := Value_y;
      return Result;
   end Construct_Type_614_Elgamal_Packet;

end Packet_Type_6_and_14;
