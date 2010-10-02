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

package Packet_Type_2 is

   type TP2_Error is (
      no_error,
      header_too_short,
      invalid_version_number,
      invalid_material_length,
      unknown_signature,
      illegal_algorithm,
      session_too_long,
      session_too_short,
      session2_too_long,
      session2_too_short,
      MPI_1_missing_data,
      MPI_2_missing_data
   );
   type TSigVersion is (illegal, three, four);
   type TLeft16 is mod 16#10000#;
   type TScalarCount is mod 16#10000#;
   type TNotationType is (name, value, flags);
   subtype Range_Preferences is Integer range 1 .. 5;
   subtype TPreferences   is TOctet_Array (Range_Preferences);
   subtype TFingerprint   is TOctet_Array (1 .. 22);
   subtype TKeyServPref   is TOctet_Array (1 .. 1);
   subtype TKeyFlags      is TOctet_Array (1 .. 1);
   subtype TFeatures      is TOctet_Array (1 .. 1);
   subtype TSigTarget     is TOctet_Array (1 .. 66);
   subtype TTrust         is TOctet_Array (1 .. 2);

   type TSignatureType is (
      Undefined,
      binary_document,
      canonical_text_document,
      standalone,
      generic_certification,
      persona_certification,
      casual_certification,
      positive_certification,
      subkey_binding,
      primary_key_binding,
      key_directly,
      key_revocation,
      subkey_revocation,
      certificate_revocation,
      timestamp,
      third_party_confirmation
   );

   type TSig_SubPacket_Type is (
      signature_creation_time,
      signature_expiration_time,
      exportable_certification,
      trust_signature,
      regular_expression,
      revocable,
      key_expiration_time,
      preferred_symmetrics,
      revocation_key,
      issuer,
      notation_data,
      preferred_hashes,
      preferred_compressions,
      server_preferences,
      preferred_key_server,
      primary_user_id,
      policy_uri,
      key_flags,
      signers_user_id,
      reason_for_revocation,
      features,
      signature_target,
      embedded_signature,
      unimplemented
   );

   type TSignature_Subpacket is record
      signature_creation_time   : TUnixTime    := 0;              --  5.2.3.4
      issuer                    : TKeyID       := (others => 0);  --  5.2.3.5
      key_expiration_time       : TUnixTime    := 0;              --  5.2.3.6
      preferred_symmetrics      : TPreferences := (others => 0);  --  5.2.3.7
      preferred_hashes          : TPreferences := (others => 0);  --  5.2.3.8
      preferred_compressions    : TPreferences := (others => 0);  --  5.2.3.9
      signature_expiration_time : TUnixTime    := 0;              --  5.2.3.10
      exportable_certification  : TOctet       := 1;              --  5.2.3.11
      revocable                 : TOctet       := 1;              --  5.2.3.12
      trust_signature           : TTrust       := (others => 0);  --  5.2.3.13
      flag_regular_expression   : Boolean      := False;          --  5.2.3.14
      revocation_key            : TFingerprint := (others => 0);  --  5.2.3.15
      total_notations           : Natural      := 0;              --  5.2.3.16
      server_preferences        : TKeyServPref := (others => 0);  --  5.2.3.17
      flag_preferred_key_server : Boolean      := False;          --  5.2.3.18
      primary_user_id           : TOctet       := 0;              --  5.2.3.19
      flag_policy_uri           : Boolean      := False;          --  5.2.3.20
      key_flags                 : TKeyFlags    := (others => 0);  --  5.2.3.21
      flag_signers_user_id      : Boolean      := False;          --  5.2.3.22
      reason_for_revocation     : TOctet       := 0;              --  5.2.3.23
      features                  : TFeatures    := (others => 0);  --  5.2.3.24
      signature_target          : TSigTarget   := (others => 0);  --  5.2.3.25
      flag_embedded_signature   : Boolean      := False;          --  5.2.3.26
      Subpacket_Position        : Natural      := 0;
      Subpacket_Length          : Natural      := 0;
   end record;


   type TPacket_2_3_Fixed is record
      Version        : TVersion          := 0;
      Signature_Type : TSignatureType    := Undefined;
      Creation_Time  : TUnixTime         := 0;
      KeyID          : TKeyID            := (others => 0);
      Algorithm      : TPubKey_Algorithm := Undefined;
      Hash           : THash_Algorithm   := Undefined;
      Left16         : TLeft16           := 0;
      Error          : TP2_Error         := no_error;
   end record;

   type TPacket_2_4_Fixed is record
      Version           : TVersion             := 0;
      Signature_Type    : TSignatureType       := Undefined;
      Algorithm         : TPubKey_Algorithm    := Undefined;
      Hash              : THash_Algorithm      := Undefined;
      Left16            : TLeft16              := 0;
      Error             : TP2_Error            := no_error;
      Hashed_Subpackets : TSignature_Subpacket;
      Plain_Subpackets  : TSignature_Subpacket;
   end record;


   function Determine_Version (Header : TPacket_Header;
                               Packet : TOctet_Array) return TSigVersion;
   --  There are currently two valid versions that can be recognized by OpenPGP.
   --  Rather than mix both versions in an unholy union for the fixed data,
   --  we'd rather scan the packet, determine which version it is, and then
   --  request the fixed data from a function tailored for the version.


   function Scan_Packet_V3 (Header : TPacket_Header;
                            Packet : TOctet_Array) return TPacket_2_3_Fixed;
   --  Returns the data inside the version 3 signature packet that is a fixed
   --  size type.  Variable size types need to be individually retrieved.


   function Scan_Packet_V4 (Header : TPacket_Header;
                            Packet : TOctet_Array) return TPacket_2_4_Fixed;
   --  Returns the data inside the version 4 signature packet that is a fixed
   --  size type.  Variable size types need to be individually retrieved.


   function Retrieve_RSA_mdn return TMPI;
   --  For RSA keys, this function returns the MPI value of m^d mod n.


   function Retrieve_DSA_r return TMPI;
   --  For DSA keys, this function returns the MPI value of r


   function Retrieve_DSA_s return TMPI;
   --  For DSA keys, this function return the MPI value of s


   function Subpacket_value (Block          : TOctet_Array;
                             SubPacket_Type : TSig_SubPacket_Type)
   return String;
   --  For subpacket types of regular_expression, flag_preferred_key_server,
   --  flag_policy_uri, flag_signers_user_id, reason_for_revocation


   function Notation_Keypair (Block        : TOctet_Array;
                              NotationType : TNotationType;
                              number       : Positive) return String;
   --  This function returns a string, either the notation's name or value.
   --  The input index must be at least 1, which stands for notation #1 and it
   --  must be less than the total number of notations in the subpacket.
   --  If the index is too high or there are no notations, a blank string is
   --  returned.  The four flags can be passed as a 4-character string using
   --  the notation type of "flags"


   function Retrieve_Embedded_Signature (Block : TOctet_Array)
   return TOctet_Array;
   --  If the provided subpacket contains an embedded signature, it will return
   --  the entire octet array for separate processing.  If not, an array of
   --  one octet will be returned with a value of zero.


private

   function convert_octet_to_signature_type (Octet : TOctet)
   return TSignatureType;
   --  Converts a byte to a signature type.  If the value of the byte is not
   --  valid, the signature type "Undefined" is returned.


   function convert_signature_type_to_octet (Signature_Type : TSignatureType)
   return TOctet;
   --  Converts the Signature Type enumeration to a number.  If the signature
   --  Type equals "Undefined", then $FF is returned.


   function convert_octets_to_16_bits (Octet_1 : TOctet;
                                       Octet_2 : TOctet) return TLeft16;
   --  This function returns Octet_1 * 256 + Octet2


   function convert_octets_to_scalar_count (Octet_1 : TOctet;
                                            Octet_2 : TOctet)
   return TScalarCount;
   --  This function returns Octet_1 * 256 + Octet2

   subtype TRaw_Data is TOctet_Array (0 .. 511);
   --  Maximum size for RSA key is 4096 bits (512 Octets)
   --  Maximum size for DSA key is 3072 bits (384 Octets)

   Raw_data : TRaw_Data := (others => 0);
   MPI_1st  : TSegment;
   MPI_2nd  : TSegment;

   function First_TMPI return TMPI;
   --  This function interprets the first segment as an MPI and returns it.

   function Second_TMPI return TMPI;
   --  This function interprets the second segment as an MPI and returns it.

   function Breakdown_Subpacket (Block    : TOctet_Array;
                                 Position : Natural)
   return TSignature_Subpacket;
   --  Given an array of bytes that is supposedly formatted as a signature
   --  subpacket, the block will be analyzed and the fixed-size data will be
   --  returned in the TSignature_Subpacket record.  For those few data that
   --  are variable length, a flag will be set to show this is present, but
   --  the fields will have be extracted by another function.

   function convert_octet_to_sig_subpacket_type (Octet : TOctet)
   return TSig_SubPacket_Type;
   --  This function will accept an octet, and return a signature subpacket
   --  type for all implemented types.  If the octet value is not recognized,
   --  or the type is reserved or otherwise non-functional, the value of
   --  "unimplemented" will be returned.

   function convert_octet_array_to_string (Block : TOctet_Array) return String;
   --  This function will convert an array of octets to a UTF-8 string.


end Packet_Type_2;
