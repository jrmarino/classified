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


package OpenPGP_Types is

   type TOctet is mod 16#100#;
   type TOctet_Array is array (Natural range <>) of TOctet;
   type TBody_Length is range 0 .. 16#FFFFFFFF#;
   type TPacket_Tag is (
         Undefined,
         Public_Key_Encrypted_Session,
         Signature,
         Symmetic_Key_Encrypted_Session,
         One_Pass_Signature_Packet,
         Secret_Key,
         Public_key,
         Secret_Subkey,
         Compressed_Data,
         Symmetrically_Encrypted_Data,
         Marker,
         Literal_Data,
         Trust,
         User_ID,
         Public_Subkey,
         User_Attribute,
         Sym_Encrypted_Integrity_Protected_Data,
         Modification_Detection_Code,
         Private_60,
         Private_61,
         Private_62,
         Private_63
   );

   type TPacket_Header is record
      Packet_Tag  : TPacket_Tag  := Undefined;
      Body_Length : TBody_Length := 0;
      Body_Starts : Natural      := 2;
   end record;

   type TPubKey_Algorithm is (
      Undefined,
      RSA_Encrypt_Or_Sign,
      RSA_Encrypt_Only,
      RSA_Sign_Only,
      Elgamal_Encrypt_Only,
      DSA,
      Reserved_Elliptic_Curve,
      Reserved_ECDSA,
      Reserved_20,
      Reserved_Diffie_Hellman,
      Private_100,
      Private_101,
      Private_102,
      Private_103,
      Private_104,
      Private_105,
      Private_106,
      Private_107,
      Private_108,
      Private_109,
      Private_110
   );

   type TSymKey_Algorithm is (
      Plaintext,
      IDEA,
      TripleDES,
      CAST5,
      Blowfish,
      Reserved_5,
      Reserved_6,
      AES_128,
      AES_192,
      AES_256,
      Twofish,
      Private_100,
      Private_101,
      Private_102,
      Private_103,
      Private_104,
      Private_105,
      Private_106,
      Private_107,
      Private_108,
      Private_109,
      Private_110
   );

   type TCompression_Algorithm is (
      Undefined,
      Uncompressed,
      ZIP_1951,
      ZLIB_1950,
      BZIP2,
      Private_100,
      Private_101,
      Private_102,
      Private_103,
      Private_104,
      Private_105,
      Private_106,
      Private_107,
      Private_108,
      Private_109,
      Private_110
   );

   type THash_Algorithm is (
      Undefined,
      MD5,
      SHA_1,
      RIPE_MD160,
      reserved_4,
      reserved_5,
      reserved_6,
      reserved_7,
      SHA_256,
      SHA_384,
      SHA_512,
      SHA_224,
      Private_100,
      Private_101,
      Private_102,
      Private_103,
      Private_104,
      Private_105,
      Private_106,
      Private_107,
      Private_108,
      Private_109,
      Private_110
   );

   subtype TVersion is TOctet;
   subtype TKeyID is TOctet_Array (0 .. 7);
   subtype TMPI is TOctet_Array;
   type TUnixTime is range 0 .. 16#FFFFFFFF#;
   type TSegment is record
      Head   : Natural := 0;
      Tail   : Natural := 1;
      Length : Natural := 1;
   end record;


end OpenPGP_Types;
