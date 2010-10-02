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

package body Packet_Type_2 is


   -------------------------
   --  Determine_Version  --
   -------------------------

   function Determine_Version (Header : TPacket_Header;
                               Packet : TOctet_Array)
   return TSigVersion is
      index : constant Natural := Header.Body_Starts;
   begin
      if Header.Body_Length = 0 then
         return illegal;
      end if;

      case Packet (index) is
         when      3 => return three;
         when      4 => return four;
         when others => return illegal;
      end case;
   end Determine_Version;



   ----------------------
   --  Scan_Packet_V3  --
   ----------------------

   function Scan_Packet_V3 (Header : TPacket_Header;
                            Packet : TOctet_Array)
   return TPacket_2_3_Fixed is
      index   : constant Natural := Header.Body_Starts;
      result  : TPacket_2_3_Fixed;
      Bodylen : Natural := 0;
      FIndex  : Natural;
   begin
      MPI_1st  := (Head => 0, Tail => 0, Length => 0);
      MPI_2nd  := (Head => 0, Tail => 0, Length => 0);
      Raw_data := (others => 0);

      if Header.Body_Length < 20 then
         result.Error := header_too_short;
         return result;
      end if;

      case Packet (index) is
         when      3 => result.Version := 3;
         when others => result.Error := invalid_version_number;
                        return result;
      end case;

      if Packet (index + 1) /= 5 then
         result.Error := invalid_material_length;
         return result;
      end if;

      result.Signature_Type :=
         convert_octet_to_signature_type (Packet (index + 2));

      if result.Signature_Type = Undefined then
         result.Error := unknown_signature;
         return result;
      end if;

      result.Creation_Time := Construct_Unix_Time (
                                 Octet_1 => Packet (index + 3),
                                 Octet_2 => Packet (index + 4),
                                 Octet_3 => Packet (index + 5),
                                 Octet_4 => Packet (index + 6));
      result.KeyID     := Packet (index + 7 .. index + 14);
      result.Algorithm := Convert_Octet_To_PK_Algorithm (Packet (index + 15));

      if (result.Algorithm /= RSA_Encrypt_Or_Sign) and then
         (result.Algorithm /= RSA_Sign_Only) and then
         (result.Algorithm /= DSA) then
            result.Error := illegal_algorithm;
            return result;
      end if;

      result.Hash   := Convert_Octet_To_Hash_ID (Octet => Packet (index + 16));
      result.Left16 := convert_octets_to_16_bits (
                              Octet_1 => Packet (index + 17),
                              Octet_2 => Packet (index + 18));
      FIndex := index + 19;
      declare
         --  all known algorithms have at least one MPI, check that it is valid
         Size : constant Natural := MPI_Byte_Size (
                                       Octet_1 => Packet (FIndex),
                                       Octet_2 => Packet (FIndex + 1));
         Remaining : constant Natural := Packet'Length - FIndex - 2;
      begin
         if Size > Remaining then
            result.Error := MPI_1_missing_data;
            return result;
         end if;

         if         (result.Algorithm = RSA_Encrypt_Or_Sign)
            or else (result.Algorithm = RSA_Sign_Only) then
            if Size < 128 then  --  1024 bits
               result.Error := session_too_short;
               return result;
            end if;
            if Size > 512 then  --  4096 bits
               result.Error := session_too_long;
               return result;
            end if;
         elsif result.Algorithm = DSA then  -- redundant
            if Size < 128 then  --  1024 bits
               result.Error := session_too_short;
               return result;
            end if;
            if Size > 384 then  --  3072 bits
               result.Error := session_too_long;
               return result;
            end if;
         end if;
         Bodylen := Size + 2;
         MPI_1st := (Head   => FIndex,
                     Tail   => FIndex + Size + 1,
                     Length => Bodylen);
      end;

      if result.Algorithm = DSA then
         declare
            index2 : constant Natural := MPI_1st.Tail + 1;
            Size : constant Natural := MPI_Byte_Size (
                                       Octet_1 => Packet (index2),
                                       Octet_2 => Packet (index2 + 1));
            Remaining : constant Natural := Packet'Length - MPI_1st.Tail;
         begin
            if Size > Remaining then
               result.Error := MPI_2_missing_data;
               return result;
            end if;

            if result.Algorithm = DSA then
               if Size < 128 then  --  1024 bits
                  result.Error := session2_too_short;
                  return result;
               end if;
               if Size > 384 then  --  3072 bits
                  result.Error := session2_too_long;
                  return result;
               end if;
            end if;
            Bodylen := Bodylen + Size + 2;
            MPI_2nd := (Head   => index2,
                        Tail   => index2 + Size + 1,
                        Length => Size + 2);
         end;

      end if;

      Raw_data (0 .. Bodylen - 1) := Packet (FIndex .. FIndex + Bodylen - 1);

      return result;

   end Scan_Packet_V3;



   ----------------------
   --  Scan_Packet_V4  --
   ----------------------

   function Scan_Packet_V4 (Header : TPacket_Header;
                            Packet : TOctet_Array)
   return TPacket_2_4_Fixed is
      index   : constant Natural := Header.Body_Starts;
      result  : TPacket_2_4_Fixed;
      Bodylen : Natural := 0;
      FIndex  : Natural;
   begin
      if Header.Body_Length < 7 then
         result.Error := header_too_short;
         return result;
      end if;

      case Packet (index) is
         when      4 => result.Version := 4;
         when others => result.Error := invalid_version_number;
                        return result;
      end case;

      result.Signature_Type :=
         convert_octet_to_signature_type (Packet (index + 1));

      if result.Signature_Type = Undefined then
         result.Error := unknown_signature;
         return result;
      end if;

      result.Algorithm := Convert_Octet_To_PK_Algorithm (Packet (index + 2));

      if (result.Algorithm /= RSA_Encrypt_Or_Sign) and then
         (result.Algorithm /= RSA_Sign_Only) and then
         (result.Algorithm /= DSA) then
            result.Error := illegal_algorithm;
            return result;
      end if;

      result.Hash := Convert_Octet_To_Hash_ID (Octet => Packet (index + 3));
      result.Hashed_Subpackets := convert_octets_to_scalar_count (
                                    Octet_1 => Packet (index + 4),
                                    Octet_2 => Packet (index + 5));
      if Header.Body_Length < TBody_Length (6 + result.Hashed_Subpackets) then
         result.Error := header_too_short;
         return result;
      end if;

      declare
         base : constant Natural := index + Natural (result.Hashed_Subpackets);
      begin
         result.Plain_Subpackets := convert_octets_to_scalar_count (
                                    Octet_1 => Packet (base + 6),
                                    Octet_2 => Packet (base + 7));
      end;

      --  to do, store hashed subpackets
      --  to do, store unhashed subpackets

      FIndex := index + Natural (result.Hashed_Subpackets) + 2 +
                        Natural (result.Plain_Subpackets);
      declare
         --  all known algorithms have at least one MPI, check that it is valid
         Size : constant Natural := MPI_Byte_Size (
                                       Octet_1 => Packet (FIndex),
                                       Octet_2 => Packet (FIndex + 1));
         Remaining : constant Natural := Packet'Length - FIndex - 2;
      begin
         if Size > Remaining then
            result.Error := MPI_1_missing_data;
            return result;
         end if;

         if         (result.Algorithm = RSA_Encrypt_Or_Sign)
            or else (result.Algorithm = RSA_Sign_Only) then
            if Size < 128 then  --  1024 bits
               result.Error := session_too_short;
               return result;
            end if;
            if Size > 512 then  --  4096 bits
               result.Error := session_too_long;
               return result;
            end if;
         elsif result.Algorithm = DSA then  -- redundant
            if Size < 128 then  --  1024 bits
               result.Error := session_too_short;
               return result;
            end if;
            if Size > 384 then  --  3072 bits
               result.Error := session_too_long;
               return result;
            end if;
         end if;
         Bodylen := Size + 2;
         MPI_1st := (Head   => FIndex,
                     Tail   => FIndex + Size + 1,
                     Length => Bodylen);
      end;

      if result.Algorithm = DSA then
         declare
            index2 : constant Natural := MPI_1st.Tail + 1;
            Size : constant Natural := MPI_Byte_Size (
                                       Octet_1 => Packet (index2),
                                       Octet_2 => Packet (index2 + 1));
            Remaining : constant Natural := Packet'Length - MPI_1st.Tail;
         begin
            if Size > Remaining then
               result.Error := MPI_2_missing_data;
               return result;
            end if;

            if result.Algorithm = DSA then
               if Size < 128 then  --  1024 bits
                  result.Error := session2_too_short;
                  return result;
               end if;
               if Size > 384 then  --  3072 bits
                  result.Error := session2_too_long;
                  return result;
               end if;
            end if;
            Bodylen := Bodylen + Size + 2;
            MPI_2nd := (Head   => index2,
                        Tail   => index2 + Size + 1,
                        Length => Size + 2);
         end;
      end if;

      return result;

   end Scan_Packet_V4;



   ---------------------------------------
   --  convert_octet_to_signature_type  --
   ---------------------------------------

   function convert_octet_to_signature_type (Octet : TOctet)
   return TSignatureType is
   begin
      case Octet is
         when 16#00# => return binary_document;
         when 16#01# => return canonical_text_document;
         when 16#02# => return standalone;
         when 16#10# => return generic_certification;
         when 16#11# => return persona_certification;
         when 16#12# => return casual_certification;
         when 16#13# => return positive_certification;
         when 16#18# => return subkey_binding;
         when 16#19# => return primary_key_binding;
         when 16#1F# => return key_directly;
         when 16#20# => return key_revocation;
         when 16#28# => return subkey_revocation;
         when 16#30# => return certificate_revocation;
         when 16#40# => return timestamp;
         when 16#50# => return third_party_confirmation;
         when others => return Undefined;
      end case;
   end convert_octet_to_signature_type;



   ---------------------------------------
   --  convert_signature_type_to_octet  --
   ---------------------------------------

   function convert_signature_type_to_octet (Signature_Type : TSignatureType)
   return TOctet is
   begin
      case Signature_Type is
         when binary_document          => return 16#00#;
         when canonical_text_document  => return 16#01#;
         when standalone               => return 16#02#;
         when generic_certification    => return 16#10#;
         when persona_certification    => return 16#11#;
         when casual_certification     => return 16#12#;
         when positive_certification   => return 16#13#;
         when subkey_binding           => return 16#18#;
         when primary_key_binding      => return 16#19#;
         when key_directly             => return 16#1F#;
         when key_revocation           => return 16#20#;
         when subkey_revocation        => return 16#28#;
         when certificate_revocation   => return 16#30#;
         when timestamp                => return 16#40#;
         when third_party_confirmation => return 16#50#;
         when Undefined                => return 16#FF#;
      end case;
   end convert_signature_type_to_octet;



   ---------------------------------
   --  convert_octets_to_16_bits  --
   ---------------------------------

   function convert_octets_to_16_bits (Octet_1 : TOctet;
                                       Octet_2 : TOctet)
   return TLeft16 is
      NOct1 : constant TLeft16 := TLeft16 (Octet_1) * 256;
      NOct2 : constant TLeft16 := TLeft16 (Octet_2);
   begin
      return NOct1 + NOct2;
   end convert_octets_to_16_bits;



   --------------------------------------
   --  convert_octets_to_scalar_count  --
   --------------------------------------

   function convert_octets_to_scalar_count (Octet_1 : TOctet;
                                            Octet_2 : TOctet)
   return TScalarCount is
      NOct1 : constant TScalarCount := TScalarCount (Octet_1) * 256;
      NOct2 : constant TScalarCount := TScalarCount (Octet_2);
   begin
      return NOct1 + NOct2;
   end convert_octets_to_scalar_count;



  ------------------
   --  First_TMPI  --
   ------------------

   function First_TMPI
   return TMPI is
      result : constant TMPI (0 .. MPI_1st.Length - 1) :=
               Raw_data (MPI_1st.Head .. MPI_1st.Tail);
   begin
      return result;
   end First_TMPI;



   -------------------
   --  Second_TMPI  --
   -------------------

   function Second_TMPI
   return TMPI is
      result : constant TMPI (0 .. MPI_2nd.Length - 1) :=
               Raw_data (MPI_2nd.Head .. MPI_2nd.Tail);
   begin
      return result;
   end Second_TMPI;



   ------------------------
   --  Retrieve_RSA_mdn  --
   ------------------------

   function Retrieve_RSA_mdn
   return TMPI is
   begin
      return First_TMPI;
   end Retrieve_RSA_mdn;



   ----------------------
   --  Retrieve_DSA_r  --
   ----------------------

   function Retrieve_DSA_r
   return TMPI is
   begin
      return First_TMPI;
   end Retrieve_DSA_r;



   ----------------------
   --  Retrieve_DSA_s  --
   ----------------------

   function Retrieve_DSA_s
   return TMPI is
   begin
      return Second_TMPI;
   end Retrieve_DSA_s;



end Packet_Type_2;
