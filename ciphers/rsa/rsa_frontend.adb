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


with Radix64; use Radix64;

package body RSA_Frontend is



   --------------------------------
   --  Decrypt_With_Private_Key  --
   --------------------------------

   procedure Decrypt_With_Private_Key (Private_Key    : in  TPrivateKey;
                                       Scrambled_Text : in  String;
                                       Plain_text     : out String;
                                       Status         : out TCryptoError)
   is
   begin
      null;
   end Decrypt_With_Private_Key;



   -------------------------------
   --  Decrypt_With_Public_Key  --
   -------------------------------

   procedure Decrypt_With_Public_Key (Public_Key     : in  TPublicKey;
                                      Scrambled_Text : in  String;
                                      Plain_text     : out String;
                                      Status         : out TCryptoError)
   is
      Key_Modulus_Len : Positive := (Positive (Public_Key.KeySize) + 7) / 8;
      public_exp : TBinaryString :=
                   Decode_Radix64 (Public_Key.Modulus (1 .. Key_Modulus_Len));
      public_mod : TBinaryString :=
                   Decode_Radix64 (Public_Key.Exponent (1 .. Key_Modulus_Len));
   begin
      null;
   end Decrypt_With_Public_Key;



   --------------------------------
   --  Encrypt_With_Private_Key  --
   --------------------------------

   procedure Encrypt_With_Private_Key (Private_Key    : in  TPrivateKey;
                                       Scrambled_Text : out String;
                                       Plain_text     : in  String;
                                       Status         : out TCryptoError)
   is
   begin
      null;
   end Encrypt_With_Private_Key;



   -------------------------------
   --  Encrypt_With_Public_Key  --
   -------------------------------

   procedure Encrypt_With_Public_Key (Public_Key     : in  TPublicKey;
                                      Scrambled_Text : out String;
                                      Plain_text     : in  String;
                                      Status         : out TCryptoError)
   is
   begin
      null;
   end Encrypt_With_Public_Key;



   -----------------------
   --  Decrypt_to_PKCS  --
   -----------------------

   procedure Decrypt_to_PKCS (Public_Key         : in  TPublicKey;
                              encrypted_bytecode : in  TBinaryString;
                              ErrorCode          : out TCryptoError;
                              pkcs_block         : out TBinaryString)
   is
      --  matrix_M : QuadByteMatrix.TData := QuadByteMatrix.Construct;
      --  matrix_E : QuadByteMatrix.TData := QuadByteMatrix.Construct;
      --  matrix_N : QuadByteMatrix.TData := QuadByteMatrix.Construct;
      --  matrix_C : QuadByteMatrix.TData := QuadByteMatrix.Construct;
   begin
      --  pkcs_block.Zero_Array;
      null;
   end Decrypt_to_PKCS;





   ----------------------------
   --  Print_Status_Message  --
   ----------------------------

   function Get_Status_Message (Status : TCryptoError)
   return String is
   begin
      case Status is
         when  0 => return "No errors.";
         when  1 => return "Radix-64 message length not divisible by 4.";
         when  2 => return "Radix-64 encoding bad, 7th bit used or mismapped.";
         when  3 => return "Radix-64 encoding bad, 4th place pad doesn't " &
                           "follow 3rd place pad.";
         when  4 => return "Encrypted message is larger than key modulus.";
         when  5 => return "PKCS block is not same length as key modulus.";
         when  6 => return "PKCS block does not start with <01>.";
         when  7 => return "PKCS block separator pattern is not <FFFFFF??00>.";
         when  8 => return "Plain text output is more than 11 chars longer " &
                           "than key modulus.";
         when  9 => return "Data Mismatch, message length > key modulus.";
         when 10 => return "Encryption: message length doesn't match modulus.";
         when 11 => return "Encryption: Message too long for modulus.";
         when 12 => return "Radix-64 encoding bad, found pad away from end.";
         when 13 => return "Radix-64 encoding bad, character previous to " &
                           "pad doesn't have clear trailing bits.";
      end case;
   end Get_Status_Message;



end RSA_Frontend;