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

package Packet_Type_2 is

   package SU renames Ada.Strings.Unbounded;

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
   subtype TPreferences     is TOctet_Array (Range_Preferences);
   subtype TFingerprint     is TOctet_Array (1 .. 22);
   subtype TKeyServPref     is TOctet_Array (1 .. 1);
   subtype TKeyFlags        is TOctet_Array (1 .. 1);
   subtype TFeatures        is TOctet_Array (1 .. 1);
   subtype TSigTarget       is TOctet_Array (1 .. 66);
   subtype TTrust           is TOctet_Array (1 .. 2);
   subtype TPackedSubpacket is TOctet_Array;
   subtype TEmbedSignature  is TOctet_Array;

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

   type TRevokeReason is (
      no_reason,
      key_is_superceded,
      key_is_compromised,
      key_is_retired,
      user_id_invalid,
      private_100,
      private_101,
      private_102,
      private_103,
      private_104,
      private_105,
      private_106,
      private_107,
      private_108,
      private_109,
      private_110,
      Undefined
   );

   type TNotationRecord is record
      flags : TOctet_Array (1 .. 4) := (others => 0);
      name  : SU.Unbounded_String   := SU.Null_Unbounded_String;
      value : SU.Unbounded_String   := SU.Null_Unbounded_String;
   end record;
   type TNotationSet is array (1 .. 20) of TNotationRecord;

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
      regular_expression        : SU.Unbounded_String :=
                                  SU.Null_Unbounded_String;       --  5.2.3.14
      revocation_key            : TFingerprint := (others => 0);  --  5.2.3.15
      total_notations           : Natural      := 0;
      notational_data           : TNotationSet;                   --  5.2.3.16
      server_preferences        : TKeyServPref := (others => 0);  --  5.2.3.17
      preferred_key_server      : SU.Unbounded_String :=
                                  SU.Null_Unbounded_String;       --  5.2.3.18
      primary_user_id           : TOctet       := 0;              --  5.2.3.19
      policy_uri                : SU.Unbounded_String :=
                                  SU.Null_Unbounded_String;       --  5.2.3.20
      key_flags                 : TKeyFlags    := (others => 0);  --  5.2.3.21
      signers_user_id           : SU.Unbounded_String :=
                                  SU.Null_Unbounded_String;       --  5.2.3.22
      reason_for_revocation     : TRevokeReason  := no_reason;
      revocation_reason_text    : SU.Unbounded_String :=
                                  SU.Null_Unbounded_String;       --  5.2.3.23
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


   function Retrieve_Embedded_Signature (Block : TOctet_Array)
   return TEmbedSignature;
   --  If the provided subpacket contains an embedded signature, it will return
   --  the entire octet array for separate processing.  If not, a zero-length
   --  array will be returned;


   function Construct_Type_2_RSA_Packet (Signature_Type   : TSignatureType;
                                         Hash             : THash_Algorithm;
                                         Left16           : TLeft16;
                                         Hashed_Subpacket : TPackedSubpacket;
                                         Plain_Subpacket  : TPackedSubpacket;
                                         Value_mdn        : TMPI)
   return TOctet_Array;
   --  This function constructs an RSA-based packet type 2 given the proper
   --  components.  The packed subpackets (Hashed and Plain) must be previously
   --  constructed and passed as an argument.


   function Construct_Type_2_DSA_Packet (Signature_Type   : TSignatureType;
                                         Hash             : THash_Algorithm;
                                         Left16           : TLeft16;
                                         Hashed_Subpacket : TPackedSubpacket;
                                         Plain_Subpacket  : TPackedSubpacket;
                                         Value_r          : TMPI;
                                         Value_s          : TMPI)
   return TOctet_Array;
   --  This function constructs an DSA-based packet type 2 given the proper
   --  components.  The packed subpackets (Hashed and Plain) must be previously
   --  constructed and passed as an argument.


   function Construct_Subpacket (Subpacket_Data : TSignature_Subpacket;
                                 signature_type : TSignatureType)
   return TPackedSubpacket;
   --  This function will accept the TSignature_Subpacket record and build a
   --  proper subpacket based on its contents.  It will not include an
   --  embedded signature.  If this is needed, use the function
   --  "Construct_Subpacket_with_Embedded_Sig";

   function Construct_Subpacket_with_Embedded_Sig (
                                    Subpacket_Data    : TSignature_Subpacket;
                                    signature_type    : TSignatureType;
                                    EmbeddedSignature : TEmbedSignature)
   return TPackedSubpacket;
   --  This function will accept the TSignature_Subpacket record and build a
   --  proper subpacket based on its contents.  An embedded signature can be
   --  included in the subpacket if the Signature provided is not a zero-length
   --  array.  (If it is, it's equivalent to the "Construct_Subpacket"
   --  function above.


private


   subtype TRaw_Data is TOctet_Array (0 .. 511);
   --  Maximum size for RSA key is 4096 bits (512 Octets)
   --  Maximum size for DSA key is 3072 bits (384 Octets)


   Raw_data : TRaw_Data := (others => 0);
   MPI_1st  : TSegment;
   MPI_2nd  : TSegment;



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


   function convert_16_bits_to_octet_array (Left16 : TLeft16)
   return TOctet_Array;
   --  This function converts a 16-bit mod type into an array of 2 Octets,
   --  Big Endian like everything else.


   function convert_octets_to_scalar_count (Octet_1 : TOctet;
                                            Octet_2 : TOctet)
   return TScalarCount;
   --  This function returns Octet_1 * 256 + Octet2


   function convert_Signature_SubPacket_Type_to_Octet (
                                       SubPacket_Type : TSig_SubPacket_Type)
   return TOctet;
   --  Converts the Signature SubPacket Type enumeration to a number.  If the
   --  signature Type equals "unimplemented", then $FF is returned.


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


   function convert_octet_array_to_unbounded_string (Block : TOctet_Array)
   return SU.Unbounded_String;
   --  This function will convert an array of octets to an unbounded string.


   function convert_unbounded_string_back (data : SU.Unbounded_String)
   return TOctet_Array;
   --  This function accepts an unbounded string and converts it to an array
   --  of octets;


   function count_preferences (preferences : TPreferences) return Natural;
   --  This function looks for all the non-zero selections within a
   --  preferences array and returns the total + 2


   function count_unixtime (UnixTime : TUnixTime) return Natural;
   --  If UnixTime equals zero, then the function returns zero,
   --  otherwise it returns 4 + 2 = 6.


   function count_type_converted_array (octet_array : TOctet_Array)
   return Natural;
   --  If all the elements of the octet_array are equal to zero, the function
   --  returns 0, otherwise it returns length of array + 1 (for type) +
   --  + (1,2,5) for length specifier.


   function count_string (data : SU.Unbounded_String) return Natural;
   --  If the string length = 0 then the function returns 0, otherwise it
   --  returns the length of the unbounded string + 1 + (1,2,5) for the
   --  encoded length (1 for less than 192 bytes, 2 for less than 8384 bytes)


   function determine_space_requirement (sig_subpacket  : TSignature_Subpacket;
                                         signature_type : TSignatureType)
   return TBody_Length;
   --  Returns size of subpacket given the requirements from the signature
   --  subpacket record.  The embedded signature is not included in this
   --  calculation.


   function count_notations (NotationSet : TNotationSet) return Natural;
   --  This function loops through the notation set and adds up the space for
   --  each individual notation and returns the accumulated total.  The
   --  "total_notations" data point is not used as each element of the array
   --  is inspect.  If both name and value strings have length, it's valid.


   function count_revocation (human_readable : SU.Unbounded_String;
                              signature_type : TSignatureType)
   return Natural;
   --  This field is strange.  Zero / null string is actually a valid input
   --  per RFC4880, but it's only used on revocation signatures.  We'll check
   --  the signature type as the criteria for including it or not.


   procedure insert_field (data      : in     TOctet_Array;
                           data_type : in     TSig_SubPacket_Type;
                           index     : in out Natural;
                           subpacket : in out TPackedSubpacket);
   --  This procedure will take an octet array and put it inside a packed
   --  subpacket at the location indicated by "index", then it will increment
   --  index for the next insertion.


   function build_preferences (preferences : TPreferences) return TOctet_Array;
   --  This function returns an array of octets tailored to the exact number
   --  of preferences it was given.  All values of zero are skipped.


   function build_revocation_field (machine_readable : TRevokeReason;
                                    human_readable   : SU.Unbounded_String)
   return TOctet_Array;
   --  This funciton returns an array of octets after accepting two reasons
   --  for a certificate / signature revocations.  It's only used for revoke
   --  signatures.


   function convert_revoke_reason_to_octet (reason : TRevokeReason)
   return TOctet;
   --  Converts an enumerated revocation reason into it's octet value


   function convert_octet_to_revoke_reason (Octet : TOctet)
   return TRevokeReason;
   --  Converts an octet into a revoke reason enumeration.  Illegal values are
   --  returned as "Undefined"


   function build_notations (NotationSet : TNotationSet;
                             x           : Natural)
   return TOctet_Array;
   --  This takes a notation set and converts it into an array of octets


end Packet_Type_2;
