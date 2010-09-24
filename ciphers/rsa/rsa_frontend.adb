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
with NN;      use NN;

package body RSA_Frontend is

   DPKCS_Error : TCryptoError := 0;
   Last_Error  : TCryptoError := 0;

   --------------------------------
   --  Decrypt_With_Private_Key  --
   --------------------------------

   procedure Decrypt_With_Private_Key (Private_Key   : in  TPrivateKey;
                                       Scrambled_R64 : in  String;
                                       Plain_Text    : out String;
                                       Status        : out TCryptoError)
   is
   begin
      null;
   end Decrypt_With_Private_Key;



   -------------------------------
   --  Decrypt_With_Public_Key  --
   -------------------------------


   function Decrypt_With_Public_Key (Public_Key    : TPublicKey;
                                     Scrambled_R64 : String)
   return String is
      Bytes_Mod : constant Positive := Positive (Public_Key.KeySize) / 8;
      scrambled : constant TBinaryString := Decode_Radix64 (Scrambled_R64);
      OutputLen : Integer;
      z         : Natural := 2;
      error_msg : constant String (1 .. 6) := "ERROR!";
   begin
      Last_Error := Get_Radix_Coding_Status;
      if Last_Error /= 0 then
         return error_msg;
      end if;

      if scrambled'Length > Bytes_Mod then
         Last_Error := 4;  -- Encrypted message is larger than key modulus
         return error_msg;
      end if;

      declare
         Revealed_Bytecode : constant TBinaryString :=
               Decrypt_to_PKCS (Public_Key         => Public_Key,
                                Encrypted_Bytecode => scrambled);
      begin
         if DPKCS_Error /= 0 then
            Last_Error := DPKCS_Error;
            return error_msg;
         end if;
         if Revealed_Bytecode'Length /= Bytes_Mod then
            Last_Error := 5;  --  PKCS block is not same length as key modulus.
            return error_msg;
         end if;
         if (Revealed_Bytecode (0) /= 0) or
            (Revealed_Bytecode (1) /= 1) then
            Last_Error := 6;  --  PKCS block does not start with <01>
            return error_msg;
         end if;

         declare
            maxZ      : constant Integer := Bytes_Mod - 1;
         begin
            Samantha :
               loop
                  exit Samantha when z > maxZ;
                  exit Samantha when Revealed_Bytecode (z) /= 16#FF#;
                  z := z + 1;
               end loop Samantha;
         end;

         if Revealed_Bytecode (z) /= 0 then
            Last_Error := 7;  --  PKCS block separator pattern not <FFFFFF??00>
            return error_msg;
         end if;

         z := z + 1;
         OutputLen := Bytes_Mod - z;
         if OutputLen + 11 > Bytes_Mod then
            Last_Error := 8;  --  Plain text output 11+ chars longer than mod.
            return error_msg;
         end if;

         Last_Error := 0;

         declare
            output : String (1 .. OutputLen);
         begin
            for x in Integer range 1 .. OutputLen loop
               output (x) := Character'Val (Revealed_Bytecode (z));
               z := z + 1;
            end loop;
            return output;
         end;
      end;

   end Decrypt_With_Public_Key;



   --------------------------------
   --  Encrypt_With_Private_Key  --
   --------------------------------

   procedure Encrypt_With_Private_Key (Private_Key   : in  TPrivateKey;
                                       Scrambled_R64 : out String;
                                       Plain_Text    : in  String;
                                       Status        : out TCryptoError)
   is
   begin
      null;
   end Encrypt_With_Private_Key;



   -------------------------------
   --  Encrypt_With_Public_Key  --
   -------------------------------

   procedure Encrypt_With_Public_Key (Public_Key    : in  TPublicKey;
                                      Scrambled_R64 : out String;
                                      Plain_Text    : in  String;
                                      Status        : out TCryptoError)
   is
   begin
      null;
   end Encrypt_With_Public_Key;



   -----------------------
   --  Decrypt_to_PKCS  --
   -----------------------

   function Decrypt_to_PKCS (Public_Key         : TPublicKey;
                             Encrypted_Bytecode : TBinaryString)
   return TBinaryString is
      matrix_M   : QuadByteMatrix.TData;
      matrix_C   : QuadByteMatrix.TData;
      error_result : constant TBinaryString (0 .. 0) := (0 => 0);
   begin
      DPKCS_Error := 0;
      matrix_M := NN_Decode (Encrypted_Bytecode);

      if matrix_M.CurrentLen > Public_Key.MsgSize or else
         matrix_M.Compared_With (
                     Index     => 0,
                     ExtData   => Public_Key.Modulus,
                     ExtDigits => Public_Key.MsgSize) >= 0 then
         DPKCS_Error := 9; --  Data Mismatch, message length > key modulus
         return error_result;
      end if;

      --  Compute c = m^e mod n.  To perform actual RSA calc.
      NN_ModExp (A => matrix_C,
                 B => matrix_M,
                 C => Public_Key.Exponent,
                 D => Public_Key.Modulus);

      --  encode output to standard form
      declare
         result : constant TBinaryString := NN_Encode (
                                 HugeNumber => matrix_C,
                                 numDigits  => Integer (Public_Key.MsgSize));
      begin

      --  clear sensitive data areas
         matrix_C.Zero_Array;
         matrix_M.Zero_Array;

         --  reference previous various so compiler doesn't complain
         if matrix_C.Matrix (0) = matrix_M.Matrix (0) then
            null;
         end if;

         return result;
      end;

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
         when  9 => return "Data mismatch, message length > key modulus.";
         when 10 => return "Encryption: message length doesn't match modulus.";
         when 11 => return "Encryption: Message too long for modulus.";
         when 12 => return "Radix-64 encoding bad, found pad away from end.";
         when 13 => return "Radix-64 encoding bad, character previous to " &
                           "pad doesn't have clear trailing bits.";
      end case;
   end Get_Status_Message;



   ------------------------
   --  Build_Public_Key  --
   ------------------------

   function Build_Public_Key (Modulus  : String;
                              Exponent : String)
   return TPublicKey is
      code              : BuildKeyError := 0;
      KeySize           : TKeySize := 1024;
      MsgSize           : QuadByteMatrixLen := 0;
      Modulus_Array     : QuadByteMatrix.TData := QuadByteMatrix.Construct;
      Exponent_Array    : QuadByteMatrix.TData := QuadByteMatrix.Construct;
      num_bits_modulus  : constant Natural := Modulus'Length * 4;
      num_bits_exponent : constant Natural := Exponent'Length * 4;
   begin
      --  Error codes: 0 means no error
      --               1 means modulus not multiple of 32
      --               2 means exponent not multiple of 32
      --               3 means modulus and exponent not equal
      --               4 means derived keysize < 1024
      --               5 means derived keysize > 4096
      if num_bits_modulus rem 32 /= 0 then
         code := 1;
         goto complete;
      end if;
      if num_bits_exponent rem 32 /= 0 then
         code := 2;
         goto complete;
      end if;
      if num_bits_modulus /= num_bits_exponent then
         code := 3;
         goto complete;
      end if;
      declare
         TestSize : constant Integer := num_bits_modulus;
      begin
         if TestSize < L_MODULUS_BITS then
            code := 4;
            goto complete;
         end if;
         if TestSize > U_MODULUS_BITS then
            code := 5;
            goto complete;
         end if;
         KeySize := TKeySize (TestSize);
         MsgSize := QuadByteMatrixLen (num_bits_modulus / 8);
      end;
      declare
         public_mod : constant TBinaryString := Decode_HexString (Modulus);
         public_exp : constant TBinaryString := Decode_HexString (Exponent);
      begin
         Modulus_Array  := NN_Decode (public_mod);
         Exponent_Array := NN_Decode (public_exp);
      end;


   <<complete>>

         return (KeySize   => KeySize,
                 Modulus   => Modulus_Array,
                 Exponent  => Exponent_Array,
                 MsgSize   => MsgSize,
                 ErrorCode => code);

   end Build_Public_Key;



   -----------------------
   --  Cryption_Status  --
   -----------------------

   function Cryption_Status return TCryptoError is
   begin
      return Last_Error;
   end Cryption_Status;

end RSA_Frontend;
