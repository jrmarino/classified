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
      hashed_subpacket_size : TScalarCount;
      plain_subpacket_size  : TScalarCount;
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
      hashed_subpacket_size := convert_octets_to_scalar_count (
                                    Octet_1 => Packet (index + 4),
                                    Octet_2 => Packet (index + 5));
      if Header.Body_Length < TBody_Length (6 + hashed_subpacket_size) then
         result.Error := header_too_short;
         return result;
      end if;

      declare
         base : constant Natural := index + Natural (hashed_subpacket_size);
         maxIndex : Natural;
      begin
         plain_subpacket_size := convert_octets_to_scalar_count (
                                    Octet_1 => Packet (base + 6),
                                    Octet_2 => Packet (base + 7));

         maxIndex := index + Natural (hashed_subpacket_size) + 5;
         result.Hashed_Subpackets := Breakdown_Subpacket (
            Block    => Packet (index + 6 .. maxIndex),
            Position => index + 6);

         maxIndex := base + Natural (plain_subpacket_size) + 7;
         result.Plain_Subpackets := Breakdown_Subpacket (
            Block    => Packet (base + 8 .. maxIndex),
            Position => base + 8);
      end;

      FIndex := index + Natural (hashed_subpacket_size) + 2 +
                        Natural (plain_subpacket_size);
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



   -------------------------------------------
   --  convert_octet_to_sig_subpacket_type  --
   -------------------------------------------

   function convert_octet_to_sig_subpacket_type (Octet : TOctet)
   return TSig_SubPacket_Type is
   begin
      case Octet is
         when      2 => return signature_creation_time;
         when      3 => return signature_expiration_time;
         when      4 => return exportable_certification;
         when      5 => return trust_signature;
         when      6 => return regular_expression;
         when      7 => return revocable;
         when      9 => return key_expiration_time;
         when     11 => return preferred_symmetrics;
         when     12 => return revocation_key;
         when     16 => return issuer;
         when     20 => return notation_data;
         when     21 => return preferred_hashes;
         when     22 => return preferred_compressions;
         when     23 => return server_preferences;
         when     24 => return preferred_key_server;
         when     25 => return primary_user_id;
         when     26 => return policy_uri;
         when     27 => return key_flags;
         when     28 => return signers_user_id;
         when     29 => return reason_for_revocation;
         when     30 => return features;
         when     31 => return signature_target;
         when     32 => return embedded_signature;
         when others => return unimplemented;
      end case;
   end convert_octet_to_sig_subpacket_type;



   ---------------------------
   --  Breakdown_Subpacket  --
   ---------------------------

   function Breakdown_Subpacket (Block    : TOctet_Array;
                                 Position : Natural)
   return TSignature_Subpacket is
      result         : TSignature_Subpacket;
      index          : Natural := 0;
      MaxIndex       : constant Natural := Block'Length - 1;
      Size           : TBody_Length;
      SubPacket_Type : TSig_SubPacket_Type;
      MaxPref        : Natural;
   begin
      result.Subpacket_Length   := Block'Length;
      result.Subpacket_Position := Position;
      result.total_notations    := 0;

      while index <= MaxIndex - 3 loop  -- 3 bytes min for length + content

         --  Calculate field size
         --  If the Size is > 2Gb, it will overflow the Natural index...
         if Block (index) < 192 then
            Size := TBody_Length (Block (index));
            index := index + 1;
         elsif Block (index) < 255 then
            Size := Two_Octet_Length (Block (index), Block (index + 1));
            index := index + 2;
         else
            --  we might not have enough bytes, so check first
            if index + 5 < MaxIndex then
               Size := Four_Octet_Length (Octet_1 => Block (index + 1),
                                          Octet_2 => Block (index + 2),
                                          Octet_3 => Block (index + 3),
                                          Octet_4 => Block (index + 4));
            else
               Size := 0;
            end if;
            index := index + 5;
         end if;
         if index + Natural (Size) <= MaxIndex then
            SubPacket_Type :=
                  convert_octet_to_sig_subpacket_type (Block (index));
            case SubPacket_Type is
               when signature_creation_time =>
                  result.signature_creation_time := Construct_Unix_Time (
                     Octet_1 => Block (index + 1),
                     Octet_2 => Block (index + 2),
                     Octet_3 => Block (index + 3),
                     Octet_4 => Block (index + 4));
               when issuer =>
                  result.issuer := Block (index + 1 .. index + 8);
               when key_expiration_time =>
                  result.key_expiration_time := Construct_Unix_Time (
                     Octet_1 => Block (index + 1),
                     Octet_2 => Block (index + 2),
                     Octet_3 => Block (index + 3),
                     Octet_4 => Block (index + 4));
               when preferred_symmetrics =>
                  --  initializing in case this is 2nd+ of this type
                  result.preferred_symmetrics := (others => 0);
                  if Size < TBody_Length (Range_Preferences'Last) then
                     MaxPref := Natural (Size);
                  else
                     MaxPref := Natural (Range_Preferences'Last);
                  end if;
                  result.preferred_symmetrics (1 .. MaxPref) :=
                        Block (index + 1 .. index + MaxPref);
               when preferred_hashes =>
                  --  initializing in case this is 2nd+ of this type
                  result.preferred_hashes := (others => 0);
                  if Size < TBody_Length (Range_Preferences'Last) then
                     MaxPref := Natural (Size);
                  else
                     MaxPref := Natural (Range_Preferences'Last);
                  end if;
                  result.preferred_hashes (1 .. MaxPref) :=
                        Block (index + 1 .. index + MaxPref);
               when preferred_compressions =>
                  --  initializing in case this is 2nd+ of this type
                  result.preferred_compressions := (others => 0);
                  if Size < TBody_Length (Range_Preferences'Last) then
                     MaxPref := Natural (Size);
                  else
                     MaxPref := Natural (Range_Preferences'Last);
                  end if;
                  result.preferred_compressions (1 .. MaxPref) :=
                        Block (index + 1 .. index + MaxPref);
               when signature_expiration_time =>
                  result.signature_expiration_time := Construct_Unix_Time (
                     Octet_1 => Block (index + 1),
                     Octet_2 => Block (index + 2),
                     Octet_3 => Block (index + 3),
                     Octet_4 => Block (index + 4));
               when exportable_certification =>
                  result.exportable_certification := Block (index + 1);
               when revocable =>
                  result.revocable := Block (index + 1);
               when trust_signature =>
                  result.trust_signature := Block (index + 1 .. index + 2);
               when regular_expression =>
                  result.regular_expression :=
                        convert_octet_array_to_unbounded_string (
                              Block (index + 1 .. index + Natural (Size)));
               when revocation_key =>
                  result.revocation_key := Block (index + 1 .. index + 22);
               when notation_data =>
                  result.total_notations := result.total_notations + 1;
                  if result.total_notations <= result.notational_data'Last then
                     result.notational_data (result.total_notations).flags :=
                        Block (index + 1 .. index + 4);
                     declare
                        mlen : constant TBody_Length := Two_Octet_Length (
                                             Octet_1 => Block (index + 5),
                                             Octet_2 => Block (index + 6));
                        nlen : constant TBody_Length := Two_Octet_Length (
                                             Octet_1 => Block (index + 7),
                                             Octet_2 => Block (index + 8));
                        offsetn : constant Natural := index + 8;
                        offsetv : constant Natural :=
                                             index + 8 + Natural (mlen);
                     begin
                        result.notational_data (result.total_notations).name :=
                           convert_octet_array_to_unbounded_string (
                              Block (offsetn + 1 .. offsetn + Natural (mlen)));
                        result.notational_data (result.total_notations).value :=
                           convert_octet_array_to_unbounded_string (
                              Block (offsetv + 1 .. offsetv + Natural (nlen)));
                     end;
                  end if;
               when server_preferences =>
                  result.server_preferences := Block (index + 1 .. index + 1);
               when preferred_key_server =>
                  result.preferred_key_server :=
                        convert_octet_array_to_unbounded_string (
                              Block (index + 1 .. index + Natural (Size)));
               when primary_user_id =>
                  result.primary_user_id := Block (index + 1);
               when policy_uri =>
                  result.policy_uri :=
                        convert_octet_array_to_unbounded_string (
                              Block (index + 1 .. index + Natural (Size)));
               when key_flags =>
                  result.key_flags := Block (index + 1 .. index + 1);
               when signers_user_id =>
                  result.signers_user_id :=
                        convert_octet_array_to_unbounded_string (
                              Block (index + 1 .. index + Natural (Size)));
               when reason_for_revocation =>
                  result.reason_for_revocation :=
                        convert_octet_to_revoke_reason (Block (index + 1));
                  result.revocation_reason_text :=
                        convert_octet_array_to_unbounded_string (
                              Block (index + 2 .. index + Natural (Size)));
               when features =>
                    result.features := Block (index + 1 .. index + 1);
               when signature_target =>
                  --  initializing in case this is 2nd+ of this type
                  result.signature_target := (others => 0);
                  result.signature_target (0 .. Natural (Size) - 1) :=
                        Block (index + 1 .. index + Natural (Size));
               when embedded_signature =>
                  result.flag_embedded_signature := True;
                  --  The variable data is an entire embedded sig packet!
               when others =>
                  null;
            end case;
         end if;
         index := index + Natural (Size);
      end loop;

      return result;

   end Breakdown_Subpacket;



   -------------------------------------
   --  convert_octet_array_to_string  --
   -------------------------------------

   function convert_octet_array_to_string (Block : TOctet_Array)
   return String is
      result : String (1 .. Block'Length);
      index  : Natural := 0;
   begin
      for x in Natural range 0 .. Block'Length loop
         index := index + 1;
         result (index) := Character'Val (Block (x));
      end loop;
      return result;
   end convert_octet_array_to_string;



   -----------------------------------------------
   --  convert_octet_array_to_unbounded_string  --
   -----------------------------------------------

   function convert_octet_array_to_unbounded_string (Block : TOctet_Array)
   return SU.Unbounded_String is
      scratch : constant String := convert_octet_array_to_string (Block);
   begin
      return SU.To_Unbounded_String (scratch);
   end convert_octet_array_to_unbounded_string;



   -----------------------------------
   --  Retrieve_Embedded_Signature  --
   -----------------------------------

   function Retrieve_Embedded_Signature (Block : TOctet_Array)
   return TEmbedSignature is
      blank      : constant TEmbedSignature (0 .. -1) := (others => 0);
      index      : Natural := 0;
      MaxIndex   : constant Natural := Block'Length - 1;
      Size       : TBody_Length;
      SampleType : TSig_SubPacket_Type;
   begin
      while index <= MaxIndex - 3 loop  -- 3 bytes min for length + content

         --  Calculate field size
         --  If the Size is > 2Gb, it will overflow the Natural index...
         if Block (index) < 192 then
            Size := TBody_Length (Block (index));
            index := index + 1;
         elsif Block (index) < 255 then
            Size := Two_Octet_Length (Block (index), Block (index + 1));
            index := index + 2;
         else
            --  we might not have enough bytes, so check first
            if index + 5 < MaxIndex then
               Size := Four_Octet_Length (Octet_1 => Block (index + 1),
                                          Octet_2 => Block (index + 2),
                                          Octet_3 => Block (index + 3),
                                          Octet_4 => Block (index + 4));
            else
               Size := 0;
            end if;
            index := index + 5;
         end if;

         if index + Natural (Size) <= MaxIndex then
            SampleType := convert_octet_to_sig_subpacket_type (Block (index));
            if SampleType = embedded_signature then
               declare
                  result : constant TOctet_Array :=
                                    Block (index + 1 .. index + Natural (Size));
               begin
                  return result;
               end;
            end if;
         end if;
      end loop;

      return blank;

   end Retrieve_Embedded_Signature;


   -----------------------------------
   --  Construct_Type_2_RSA_Packet  --
   -----------------------------------

   function Construct_Type_2_RSA_Packet (Signature_Type   : TSignatureType;
                                         Hash             : THash_Algorithm;
                                         Left16           : TLeft16;
                                         Hashed_Subpacket : TPackedSubpacket;
                                         Plain_Subpacket  : TPackedSubpacket;
                                         Value_mdn        : TMPI)
   return TOctet_Array is
      HS_Length : constant TOctet_Array :=
                           Encode_Body_Length (Hashed_Subpacket'Length);
      PS_Length : constant TOctet_Array :=
                           Encode_Body_Length (Plain_Subpacket'Length);
      Body_Length : constant TBody_Length := 6 +
                                       HS_Length'Length +
                                       PS_Length'Length +
                                       Hashed_Subpacket'Length +
                                       Plain_Subpacket'Length +
                                       Value_mdn'Length;
      enclen : constant TOctet_Array := Encode_Body_Length (Body_Length);
      Last_Index : constant Natural := Natural (Body_Length) + enclen'Length;
      result : TOctet_Array (0 .. Last_Index) := (others => 0);
      ndx0 : Natural := enclen'Length + 1;
      ndx1 : Natural;
   begin
      result (0) := Convert_Packet_Tag_To_Octet (Signature);
      result (1 .. enclen'Length) := enclen;

      result (ndx0) := 4;   --  version 4
      result (ndx0 + 1) := convert_signature_type_to_octet (Signature_Type);
      result (ndx0 + 2) := 1;   --  RSA Encrypt or Sign
      result (ndx0 + 3) := Convert_Hash_ID_To_Octet (hash_Algorithm => Hash);

      ndx0 := ndx0 + 4;
      ndx1 := ndx0 + HS_Length'Last;
      result (ndx0 .. ndx1) := HS_Length;

      ndx0 := ndx1 + 1;
      ndx1 := Hashed_Subpacket'Length + ndx0 - 1;
      result (ndx0 .. ndx1) := TOctet_Array (Hashed_Subpacket);

      ndx0 := ndx1 + 1;
      ndx1 := PS_Length'Length + ndx0 - 1;
      result (ndx0 .. ndx1) := PS_Length;

      ndx0 := ndx1 + 1;
      ndx1 := Plain_Subpacket'Length + ndx0 - 1;
      result (ndx0 .. ndx1) := TOctet_Array (Plain_Subpacket);

      ndx0 := ndx1 + 1;
      ndx1 := ndx0 + 1;
      result (ndx0 .. ndx1) := convert_16_bits_to_octet_array (Left16);

      ndx0 := ndx1 + 1;
      ndx1 := Value_mdn'Length + ndx0 - 1;
      result (ndx0 .. ndx1) := TOctet_Array (Value_mdn);

      return result;

   end Construct_Type_2_RSA_Packet;



   -----------------------------------
   --  Construct_Type_2_DSA_Packet  --
   -----------------------------------

   function Construct_Type_2_DSA_Packet (Signature_Type   : TSignatureType;
                                         Hash             : THash_Algorithm;
                                         Left16           : TLeft16;
                                         Hashed_Subpacket : TPackedSubpacket;
                                         Plain_Subpacket  : TPackedSubpacket;
                                         Value_r          : TMPI;
                                         Value_s          : TMPI)
   return TOctet_Array is
      HS_Length : constant TOctet_Array :=
                           Encode_Body_Length (Hashed_Subpacket'Length);
      PS_Length : constant TOctet_Array :=
                           Encode_Body_Length (Plain_Subpacket'Length);
      Body_Length : constant TBody_Length := 6 +
                                       HS_Length'Length +
                                       PS_Length'Length +
                                       Hashed_Subpacket'Length +
                                       Plain_Subpacket'Length +
                                       Value_r'Length +
                                       Value_s'Length;
      enclen : constant TOctet_Array := Encode_Body_Length (Body_Length);
      Last_Index : constant Natural := Natural (Body_Length) + enclen'Length;
      result : TOctet_Array (0 .. Last_Index) := (others => 0);
      ndx0 : Natural := enclen'Length + 1;
      ndx1 : Natural;
   begin
      result (0) := Convert_Packet_Tag_To_Octet (Signature);
      result (1 .. enclen'Length) := enclen;

      result (ndx0) := 4;   --  version 4
      result (ndx0 + 1) := convert_signature_type_to_octet (Signature_Type);
      result (ndx0 + 2) := 17;  --  DSA
      result (ndx0 + 3) := Convert_Hash_ID_To_Octet (hash_Algorithm => Hash);

      ndx0 := ndx0 + 4;
      ndx1 := ndx0 + HS_Length'Last;
      result (ndx0 .. ndx1) := HS_Length;

      ndx0 := ndx1 + 1;
      ndx1 := Hashed_Subpacket'Length + ndx0 - 1;
      result (ndx0 .. ndx1) := TOctet_Array (Hashed_Subpacket);

      ndx0 := ndx1 + 1;
      ndx1 := PS_Length'Length + ndx0 - 1;
      result (ndx0 .. ndx1) := PS_Length;

      ndx0 := ndx1 + 1;
      ndx1 := Plain_Subpacket'Length + ndx0 - 1;
      result (ndx0 .. ndx1) := TOctet_Array (Plain_Subpacket);

      ndx0 := ndx1 + 1;
      ndx1 := ndx0 + 1;
      result (ndx0 .. ndx1) := convert_16_bits_to_octet_array (Left16);

      ndx0 := ndx1 + 1;
      ndx1 := Value_r'Length + ndx0 - 1;
      result (ndx0 .. ndx1) := TOctet_Array (Value_r);

      ndx0 := ndx1 + 1;
      ndx1 := Value_s'Length + ndx0 - 1;
      result (ndx0 .. ndx1) := TOctet_Array (Value_s);

      return result;

   end Construct_Type_2_DSA_Packet;



   --------------------------------------
   --  convert_16_bits_to_octet_array  --
   --------------------------------------

   function convert_16_bits_to_octet_array (Left16 : TLeft16)
   return TOctet_Array is
      mask1  : constant TLeft16 := 16#FF00#;
      mask2  : constant TLeft16 := 16#00FF#;
      Oct1   : constant TOctet := TOctet ((Left16 and mask1) / 256);
      Oct2   : constant TOctet := TOctet (Left16 and mask2);
      result : constant TOctet_Array (0 .. 1) := (0 => Oct1, 1 => Oct2);
   begin
      return result;
   end convert_16_bits_to_octet_array;


   -------------------------------------------------
   --  convert_Signature_SubPacket_Type_to_Octet  --
   -------------------------------------------------

   function convert_Signature_SubPacket_Type_to_Octet (
                                       SubPacket_Type : TSig_SubPacket_Type)
   return TOctet is
   begin
      case SubPacket_Type is
         when signature_creation_time   => return 2;
         when signature_expiration_time => return 3;
         when exportable_certification  => return 4;
         when trust_signature           => return 5;
         when regular_expression        => return 6;
         when revocable                 => return 7;
         when key_expiration_time       => return 9;
         when preferred_symmetrics      => return 11;
         when revocation_key            => return 12;
         when issuer                    => return 16;
         when notation_data             => return 20;
         when preferred_hashes          => return 21;
         when preferred_compressions    => return 22;
         when server_preferences        => return 23;
         when preferred_key_server      => return 24;
         when primary_user_id           => return 25;
         when policy_uri                => return 26;
         when key_flags                 => return 27;
         when signers_user_id           => return 28;
         when reason_for_revocation     => return 29;
         when features                  => return 30;
         when signature_target          => return 31;
         when embedded_signature        => return 32;
         when unimplemented             => return 16#FF#;
      end case;
   end convert_Signature_SubPacket_Type_to_Octet;



   -------------------------
   --  count_preferences  --
   -------------------------

   function count_preferences (preferences : TPreferences)
   return Natural is
      result : Natural := 0;
   begin
      for x in preferences'Range loop
         if preferences (x) /= 0 then
            result := result + 1;
         end if;
      end loop;
      if result = 0 then
         return result;
      else
         return result + 2;
      end if;
   end count_preferences;



   ----------------------
   --  count_unixtime  --
   ----------------------

   function count_unixtime (UnixTime : TUnixTime)
   return Natural is
      result : Natural := 6;
   begin
      if UnixTime = 0 then
         result := 0;
      end if;
      return result;
   end count_unixtime;



   ----------------------------------
   --  count_type_converted_array  --
   ----------------------------------

   function count_type_converted_array (octet_array : TOctet_Array)
   return Natural is
      use_it : Boolean := False;
   begin
      for x in octet_array'Range loop
         if x /= 0 then
            use_it := True;
         end if;
      end loop;
      if use_it then
         declare
            result : Natural := octet_array'Length;
            reslen : constant TBody_Length := TBody_Length (result);
            enclen : constant TOctet_Array := Encode_Body_Length (reslen);
         begin
            result := result + 1 + enclen'Length;
            return result;
         end;
      else
         return 0;
      end if;
   end count_type_converted_array;



   --------------------
   --  count_string  --
   --------------------

   function count_string (data : SU.Unbounded_String)
   return Natural is
      strlen : Natural := SU.Length (data);
   begin
      if strlen = 0 then
         return 0;
      end if;
      declare
         reslen : constant TBody_Length := TBody_Length (strlen);
         enclen : constant TOctet_Array := Encode_Body_Length (reslen);
      begin
         strlen := strlen + 1 + enclen'Length;
      end;
      return strlen;

   end count_string;



   -----------------------
   --  count_notations  --
   -----------------------

   function count_notations (NotationSet : TNotationSet)
   return Natural is
      total    : Natural := 0;
      namelen  : Natural;
      valuelen : Natural;
   begin
      for x in NotationSet'Range loop
         namelen  := SU.Length (NotationSet (x).name);
         valuelen := SU.Length (NotationSet (x).value);
         if namelen > 0 and then
            valuelen > 0 then
            total := total + 8 + namelen + valuelen;
         end if;
      end loop;
      if total = 0 then
         return total;
      end if;
      declare
         reslen : constant TBody_Length := TBody_Length (total);
         enclen : constant TOctet_Array := Encode_Body_Length (reslen);
      begin
         total := total + 1 + enclen'Length;
      end;
      return total;

   end count_notations;



   ------------------------
   --  count_revocation  --
   ------------------------

   function count_revocation (human_readable   : SU.Unbounded_String;
                              signature_type   : TSignatureType)
   return Natural is
   begin
      if signature_type = key_revocation or else
         signature_type = subkey_revocation or else
         signature_type = certificate_revocation then
         declare
            result : Natural := 1 + SU.Length (human_readable);
            reslen : constant TBody_Length := TBody_Length (result);
            enclen : constant TOctet_Array := Encode_Body_Length (reslen);
         begin
            result := result + 1 + enclen'Length;
            return result;
         end;
      else
         return 0;
      end if;
   end count_revocation;



   -----------------------------------
   --  determine_space_requirement  --
   -----------------------------------

   function determine_space_requirement (sig_subpacket : TSignature_Subpacket;
                                         signature_type : TSignatureType)
   return TBody_Length is
      total : Natural;
   begin
      total := count_unixtime (sig_subpacket.signature_creation_time)
             + count_type_converted_array (sig_subpacket.issuer)
             + count_unixtime (sig_subpacket.key_expiration_time)
             + count_preferences (sig_subpacket.preferred_symmetrics)
             + count_preferences (sig_subpacket.preferred_hashes)
             + count_preferences (sig_subpacket.preferred_compressions)
             + count_unixtime (sig_subpacket.signature_expiration_time)
             + 3  --  sig_subpacket.exportable_certification
             + 3  --  sig_subpacket.revocable
             + count_type_converted_array (sig_subpacket.trust_signature)
             + count_string (sig_subpacket.regular_expression)
             + count_type_converted_array (sig_subpacket.revocation_key)
             + count_notations (sig_subpacket.notational_data)
             + count_type_converted_array (sig_subpacket.server_preferences)
             + count_string (sig_subpacket.preferred_key_server)
             + 3  --  primary_user_id
             + count_string (sig_subpacket.policy_uri)
             + count_type_converted_array (sig_subpacket.key_flags)
             + count_string (sig_subpacket.signers_user_id)
             + count_revocation (
                  human_readable => sig_subpacket.revocation_reason_text,
                  signature_type => signature_type)
             + count_type_converted_array (sig_subpacket.features)
             + count_type_converted_array (sig_subpacket.signature_target);

      return TBody_Length (total);
   end determine_space_requirement;



   --------------------
   --  insert_field  --
   --------------------

   procedure insert_field (data      : in     TOctet_Array;
                           data_type : in     TSig_SubPacket_Type;
                           index     : in out Natural;
                           subpacket : in out TPackedSubpacket)
   is
      datalen : constant TBody_Length := TBody_Length (data'Length);
      enclen  : constant TOctet_Array := Encode_Body_Length (datalen);
   begin

      subpacket (index .. index + enclen'Last) := enclen;
      index := index + enclen'Length;

      subpacket (index) :=
            convert_Signature_SubPacket_Type_to_Octet (data_type);
      index := index + 1;

      subpacket (index .. index + data'Last) := data;
      index := index + data'Length;

   end insert_field;


   -------------------------
   --  build_preferences  --
   -------------------------

   function build_preferences (preferences : TPreferences)
   return TOctet_Array is
      scratch : TOctet_Array (preferences'Range) := (others => 0);
      index : Natural := 0;
   begin
      for x in preferences'Range loop
         if preferences (x) /= 0 then
            scratch (index) := preferences (x);
            index := index + 1;
         end if;
      end loop;
      return scratch (0 .. index - 1);
   end build_preferences;


   -------------------------------------
   --  convert_unbounded_string_back  --
   -------------------------------------

   function convert_unbounded_string_back (data : SU.Unbounded_String)
   return TOctet_Array is
      work   : constant String := SU.To_String (Source => data);
      result : TOctet_Array (0 .. work'Length - 1);
      index  : Natural := 0;
   begin
      for x in work'Range loop
         result (index) := TOctet (Character'Pos (work (x)));
         index := index + 1;
      end loop;
      return result;
   end convert_unbounded_string_back;



   --------------------------------------
   --  convert_revoke_reason_to_octet  --
   --------------------------------------

   function convert_revoke_reason_to_octet (reason : TRevokeReason)
   return TOctet is
   begin
      case reason is
         when no_reason          => return 0;
         when key_is_superceded  => return 1;
         when key_is_compromised => return 2;
         when key_is_retired     => return 3;
         when user_id_invalid    => return 32;
         when private_100        => return 100;
         when private_101        => return 101;
         when private_102        => return 102;
         when private_103        => return 103;
         when private_104        => return 104;
         when private_105        => return 105;
         when private_106        => return 106;
         when private_107        => return 107;
         when private_108        => return 108;
         when private_109        => return 109;
         when private_110        => return 110;
         when others             => return 16#FF#;
      end case;
   end convert_revoke_reason_to_octet;



   --------------------------------------
   --  convert_revoke_reason_to_octet  --
   --------------------------------------

   function convert_octet_to_revoke_reason (Octet : TOctet)
   return TRevokeReason is
   begin
      case Octet is
         when   0 => return no_reason;
         when   1 => return key_is_superceded;
         when   2 => return key_is_compromised;
         when   3 => return key_is_retired;
         when  32 => return user_id_invalid;
         when 100 => return private_100;
         when 101 => return private_101;
         when 102 => return private_102;
         when 103 => return private_103;
         when 104 => return private_104;
         when 105 => return private_105;
         when 106 => return private_106;
         when 107 => return private_107;
         when 108 => return private_108;
         when 109 => return private_109;
         when 110 => return private_110;
         when others => return Undefined;
      end case;
   end convert_octet_to_revoke_reason;


   ------------------------------
   --  build_revocation_field  --
   ------------------------------

   function build_revocation_field (machine_readable : TRevokeReason;
                                    human_readable   : SU.Unbounded_String)
   return TOctet_Array is
      result : TOctet_Array (0 .. SU.Length (Source => human_readable));
   begin
      result (0) := convert_revoke_reason_to_octet (machine_readable);
      result (1 .. result'Last) :=
                              convert_unbounded_string_back (human_readable);
      return result;
   end build_revocation_field;



   -----------------------
   --  build_notations  --
   -----------------------

   function build_notations (NotationSet : TNotationSet;
                             x           : Natural)
   return TOctet_Array is
      namelen  : Natural;
      valuelen : Natural;
   begin
      namelen  := SU.Length (NotationSet (x).name);
      valuelen := SU.Length (NotationSet (x).value);
      declare
         total  : constant Natural := namelen + valuelen + 8;
         result : TOctet_Array (0 .. total - 1) := (others => 0);
         index  : Natural := 0;
      begin
         result (index + 0) := 16#80#;
         result (index + 4) := TOctet (namelen / 256);
         result (index + 5) := TOctet (namelen rem 256);
         result (index + 6) := TOctet (valuelen / 256);
         result (index + 7) := TOctet (valuelen rem 256);
         index := index + 8;
         result (index .. index + namelen - 1) :=
                convert_unbounded_string_back (data => NotationSet (x).name);
         index := index + namelen;
         result (index .. index + valuelen - 1) :=
                convert_unbounded_string_back (data => NotationSet (x).value);

         return result;
      end;

   end build_notations;


   ---------------------------
   --  Construct_Subpacket  --
   ---------------------------

   function Construct_Subpacket (Subpacket_Data : TSignature_Subpacket;
                                 signature_type : TSignatureType)
   return TPackedSubpacket is
   --  This function will accept the TSignature_Subpacket record and build a
   --  proper subpacket based on its contents.  It will not include an
   --  embedded signature.  If this is needed, use the function
   --  "Construct_Subpacket_with_Embedded_Sig";
      subpacket_size : constant TBody_Length := determine_space_requirement (
                                 sig_subpacket  => Subpacket_Data,
                                 signature_type => signature_type);
      subpacket : TPackedSubpacket (0 .. Natural (subpacket_size) - 1);
      test  : Natural;
      index : Natural := 0;
   begin
      test := count_unixtime (Subpacket_Data.signature_creation_time);
      if test > 0 then
         insert_field (data      => Breakdown_Unix_Time
                                    (Subpacket_Data.signature_creation_time),
                       data_type => signature_creation_time,
                       index     => index,
                       subpacket => subpacket);
      end if;

      test := count_type_converted_array (Subpacket_Data.issuer);
      if test > 0 then
         insert_field (data      => Subpacket_Data.issuer,
                       data_type => issuer,
                       index     => index,
                       subpacket => subpacket);
      end if;

      test := count_unixtime (Subpacket_Data.key_expiration_time);
      if test > 0 then
         insert_field (data      => Breakdown_Unix_Time
                                    (Subpacket_Data.key_expiration_time),
                       data_type => key_expiration_time,
                       index     => index,
                       subpacket => subpacket);
      end if;

      test := count_preferences (Subpacket_Data.preferred_symmetrics);
      if test > 0 then

         insert_field (data      => build_preferences
                                    (Subpacket_Data.preferred_symmetrics),
                       data_type => preferred_symmetrics,
                       index     => index,
                       subpacket => subpacket);
      end if;

      test := count_preferences (Subpacket_Data.preferred_hashes);
      if test > 0 then

         insert_field (data      => build_preferences
                                    (Subpacket_Data.preferred_hashes),
                       data_type => preferred_hashes,
                       index     => index,
                       subpacket => subpacket);
      end if;

      test := count_preferences (Subpacket_Data.preferred_compressions);
      if test > 0 then

         insert_field (data      => build_preferences
                                    (Subpacket_Data.preferred_compressions),
                       data_type => preferred_compressions,
                       index     => index,
                       subpacket => subpacket);
      end if;

      test := count_unixtime (Subpacket_Data.signature_expiration_time);
      if test > 0 then
         insert_field (data      => Breakdown_Unix_Time
                                    (Subpacket_Data.signature_expiration_time),
                       data_type => signature_expiration_time,
                       index     => index,
                       subpacket => subpacket);
      end if;

      insert_field (data      => (0 => Subpacket_Data.exportable_certification),
                    data_type => exportable_certification,
                    index     => index,
                    subpacket => subpacket);

      insert_field (data      => (0 => Subpacket_Data.revocable),
                    data_type => revocable,
                    index     => index,
                    subpacket => subpacket);

      test := count_type_converted_array (Subpacket_Data.trust_signature);
      if test > 0 then
         insert_field (data      => Subpacket_Data.trust_signature,
                       data_type => trust_signature,
                       index     => index,
                       subpacket => subpacket);
      end if;

      test := count_string (Subpacket_Data.regular_expression);
      if test > 0 then
         insert_field (data      => convert_unbounded_string_back
                                    (Subpacket_Data.regular_expression),
                       data_type => regular_expression,
                       index     => index,
                       subpacket => subpacket);
      end if;

      test := count_type_converted_array (Subpacket_Data.revocation_key);
      if test > 0 then
         insert_field (data      => Subpacket_Data.revocation_key,
                       data_type => revocation_key,
                       index     => index,
                       subpacket => subpacket);
      end if;

      test := count_type_converted_array (Subpacket_Data.server_preferences);
      if test > 0 then
         insert_field (data      => Subpacket_Data.server_preferences,
                       data_type => server_preferences,
                       index     => index,
                       subpacket => subpacket);
      end if;

      test := count_string (Subpacket_Data.preferred_key_server);
      if test > 0 then
         insert_field (data      => convert_unbounded_string_back
                                    (Subpacket_Data.preferred_key_server),
                       data_type => preferred_key_server,
                       index     => index,
                       subpacket => subpacket);
      end if;

      insert_field (data      => (0 => Subpacket_Data.primary_user_id),
                    data_type => primary_user_id,
                    index     => index,
                    subpacket => subpacket);

      test := count_string (Subpacket_Data.policy_uri);
      if test > 0 then
         insert_field (data      => convert_unbounded_string_back
                                    (Subpacket_Data.policy_uri),
                       data_type => policy_uri,
                       index     => index,
                       subpacket => subpacket);
      end if;

      test := count_type_converted_array (Subpacket_Data.key_flags);
      if test > 0 then
         insert_field (data      => Subpacket_Data.key_flags,
                       data_type => key_flags,
                       index     => index,
                       subpacket => subpacket);
      end if;

      test := count_string (Subpacket_Data.signers_user_id);
      if test > 0 then
         insert_field (data      => convert_unbounded_string_back
                                    (Subpacket_Data.signers_user_id),
                       data_type => signers_user_id,
                       index     => index,
                       subpacket => subpacket);
      end if;

      test := count_type_converted_array (Subpacket_Data.features);
      if test > 0 then
         insert_field (data      => Subpacket_Data.features,
                       data_type => features,
                       index     => index,
                       subpacket => subpacket);
      end if;

      test := count_type_converted_array (Subpacket_Data.signature_target);
      if test > 0 then
         insert_field (data      => Subpacket_Data.signature_target,
                       data_type => signature_target,
                       index     => index,
                       subpacket => subpacket);
      end if;

      test := count_revocation (
                  human_readable => Subpacket_Data.revocation_reason_text,
                  signature_type => signature_type);
      if test > 0 then
         insert_field (data      => build_revocation_field (
                machine_readable => Subpacket_Data.reason_for_revocation,
                human_readable   => Subpacket_Data.revocation_reason_text),
                       data_type => reason_for_revocation,
                       index     => index,
                       subpacket => subpacket);
      end if;

      test := count_notations (Subpacket_Data.notational_data);
      if test > 0 then
         declare
            namelen  : Natural;
            valuelen : Natural;
         begin
            for x in Subpacket_Data.notational_data'Range loop
               namelen  := SU.Length (Subpacket_Data.notational_data (x).name);
               valuelen := SU.Length (Subpacket_Data.notational_data (x).value);
               if namelen > 0 and then valuelen > 0 then
                  insert_field (data      => build_notations (
                              NotationSet => Subpacket_Data.notational_data,
                              x           => x),
                                data_type => notation_data,
                                index     => index,
                                subpacket => subpacket);
               end if;
            end loop;
         end;
      end if;

      return subpacket;
   end Construct_Subpacket;




   ---------------------------------------------
   --  Construct_Subpacket_with_Embedded_Sig  --
   ---------------------------------------------

   function Construct_Subpacket_with_Embedded_Sig (
                                    Subpacket_Data    : TSignature_Subpacket;
                                    signature_type    : TSignatureType;
                                    EmbeddedSignature : TEmbedSignature)
   return TPackedSubpacket is
      Basis : constant TPackedSubpacket :=
                       Construct_Subpacket (Subpacket_Data => Subpacket_Data,
                                            signature_type => signature_type);
   begin
      if EmbeddedSignature'Length = 0 then
         return Basis;
      end if;

      declare
         ES_Length : constant TOctet_Array :=
                              Encode_Body_Length (EmbeddedSignature'Length);
         SubpacketLen : constant Natural := Basis'Length +
                                            ES_Length'Length +
                                            EmbeddedSignature'Length +
                                            1;  --  field type
         ESType : constant TOctet_Array :=
                           (0 => convert_Signature_SubPacket_Type_to_Octet
                                 (embedded_signature));
         result : TPackedSubpacket (0 .. SubpacketLen - 1) := (others => 0);
         iFirst : Natural := 0;
         iLast  : Natural := Basis'Last;
      begin
         result (iFirst .. iLast) := Basis;

         iFirst := iLast + 1;
         iLast  := iFirst;
         result (iFirst .. iLast) := ESType;

         iFirst := iLast + 1;
         iLast  := iFirst + ES_Length'Last;
         result (iFirst .. iLast) := ES_Length;

         iFirst := iLast + 1;
         iLast  := iFirst + EmbeddedSignature'Last;
         result (iFirst .. iLast) := EmbeddedSignature;
         return result;
      end;

   end Construct_Subpacket_with_Embedded_Sig;

end Packet_Type_2;
