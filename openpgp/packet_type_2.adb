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
                  if result.total_notations <= 10 then
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
                  result.flag_policy_uri := True;
                  --  Variable length, so we can only flag that it exists.
               when key_flags =>
                  result.key_flags := Block (index + 1 .. index + 1);
               when signers_user_id =>
                  result.signers_user_id :=
                        convert_octet_array_to_unbounded_string (
                              Block (index + 1 .. index + Natural (Size)));
               when reason_for_revocation =>
                  result.reason_for_revocation := Block (index + 1);
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
   return TOctet_Array is
      blank      : constant TOctet_Array (1 .. 1) := (0 => 0);
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


end Packet_Type_2;
