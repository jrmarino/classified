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
   EPKCS_Error : TCryptoError := 0;
   Last_Error  : TCryptoError := 0;
   error_msg   : constant String (1 .. 6) := "ERROR!";

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
               Decrypt_PKCS (Public_Key         => Public_Key,
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

   function Encrypt_With_Private_Key (Private_Key   : TPrivateKey;
                                      Plain_Text    : String)
   return String is
      Plain_Bytecode : TBinaryString (0 .. Integer (Private_Key.MsgSize) - 1) :=
                       (others => 16#FF#);
      InputLen : Natural := Plain_Text'Length;
      border   : Natural := Natural (Private_Key.MsgSize) - InputLen - 1;
   begin
      Last_Error := 0;

      if InputLen + 11 > Natural (Private_Key.MsgSize) then
         Last_Error := 11;  -- Encryption: Message too long for modulus.
         return error_msg;
      end if;

      --  PKCS format
      Plain_Bytecode (0) := 0;
      Plain_Bytecode (1) := 1;  --  block type 1
      Plain_Bytecode (border) := 0;

      for x in Integer range 1 .. InputLen loop
         Plain_Bytecode (border + x) := Character'Pos (Plain_Text (x));
      end loop;

      declare
         Encrypted_Bytecode : constant TBinaryString := Encrypt_to_PKCS (
                                       Private_Key    => Private_Key,
                                       Plain_Bytecode => Plain_Bytecode);
         z : Natural := 2;
      begin
         --  clear sensitive information from memory
         Plain_Bytecode := (others => 0);

         if EPKCS_Error /= 0 then
            Last_Error := EPKCS_Error;
            return error_msg;
         end if;

         declare
            result : constant String := Encode_to_Radix64 (Encrypted_Bytecode);
         begin
            return result;
         end;
      end;

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



   --------------------
   --  Decrypt_PKCS  --
   --------------------

   function Decrypt_PKCS (Public_Key         : TPublicKey;
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

   end Decrypt_PKCS;



   -----------------------
   --  Encrypt_to_PKCS  --
   -----------------------

   function Encrypt_to_PKCS (Private_Key    : TPrivateKey;
                             Plain_Bytecode : TBinaryString)
   return TBinaryString is
      Matrix_c    : QuadByteMatrix.TData;
      PKCSErr     : constant TBinaryString (0 .. 2) := (5, 5, 5);
   begin

      --  decode required input data from standard form
      Matrix_c    := NN_Decode (HexString => Plain_Bytecode);

      if Matrix_c.Compared_With (
                        Index     => 0,
                        ExtData   => Private_Key.Modulus,
                        ExtDigits => Private_Key.Modulus.CurrentLen) >= 0 then
         EPKCS_Error := 17;
         return PKCSErr;
      end if;

      --  Compute mP = cP^dP mod p  and  mQ = cQ^dQ mod q.
      --  (Assumes q has length at most pDigits, i.e., p > q)

      declare
         scratch   : QuadByteMatrix.TData;
         Matrix_cP : QuadByteMatrix.TData := QuadByteMatrix.Construct;
         Matrix_cQ : QuadByteMatrix.TData := QuadByteMatrix.Construct;
         Matrix_mP : QuadByteMatrix.TData := QuadByteMatrix.Construct;
         Matrix_mQ : QuadByteMatrix.TData := QuadByteMatrix.Construct;
         Matrix_t1 : QuadByteMatrix.TData := QuadByteMatrix.Construct;
         Matrix_t2 : QuadByteMatrix.TData := QuadByteMatrix.Construct;
         Matrix_t3 : QuadByteMatrix.TData := QuadByteMatrix.Construct;
         Matrix_t4 : QuadByteMatrix.TData := QuadByteMatrix.Construct;
         Matrix_t5 : QuadByteMatrix.TData := QuadByteMatrix.Construct;
         borrow    : MQuadByte;
         carry     : MQuadByte;
      begin
         NN_Div (ResDiv => scratch,
                 ResMod => Matrix_cP,
                 C      => Matrix_c,
                 D      => Private_Key.Prime_p);

         NN_Div (ResDiv => scratch,
                 ResMod => Matrix_cQ,
                 C      => Matrix_c,
                 D      => Private_Key.Prime_q);

         NN_ModExp (A   => Matrix_mP,
                    B   => Matrix_cP,
                    C   => Private_Key.Prime_Exp_p,
                    D   => Private_Key.Prime_p);

         NN_ModExp (A   => Matrix_mQ,
                    B   => Matrix_cQ,
                    C   => Private_Key.Prime_Exp_q,
                    D   => Private_Key.Prime_q);

         --  Chinese Remainder Theorem:
         --  m = ((((mP - mQ) mod p) * qInv) mod p) * q + mQ

         if Matrix_mP.Compared_With (
                        Index     => 0,
                        ExtData   => Matrix_mQ,
                        ExtDigits => Private_Key.Prime_p.CurrentLen) >= 0 then
            NN_Sub (A         => Matrix_t2,
                    A_Index   => 0,
                    B         => Matrix_mP,
                    B_Index   => 0,
                    C         => Matrix_mQ,
                    C_Index   => 0,
                    numDigits => Private_Key.Prime_p.CurrentLen,
                    borrow    => borrow);
         else
            NN_Sub (A         => Matrix_t1,
                    A_Index   => 0,
                    B         => Matrix_mQ,
                    B_Index   => 0,
                    C         => Matrix_mP,
                    C_Index   => 0,
                    numDigits => Private_Key.Prime_p.CurrentLen,
                    borrow    => borrow);

            NN_Sub (A         => Matrix_t2,
                    A_Index   => 0,
                    B         => Private_Key.Prime_p,
                    B_Index   => 0,
                    C         => Matrix_t1,
                    C_Index   => 0,
                    numDigits => Private_Key.Prime_p.CurrentLen,
                    borrow    => borrow);
         end if;

         NN_ModMult (A => Matrix_t3,
                     B => Matrix_t2,
                     C => Private_Key.coefficient,
                     D => Private_Key.Prime_p);

         NN_Mult (A       => Matrix_t4,
                  A_Index => 0,
                  B       => Matrix_t3,
                  B_Index => 0,
                  C       => Private_Key.Prime_q,
                  C_Index => 0);

         NN_Add (A         => Matrix_t5,
                 A_Index   => 0,
                 B         => Matrix_t4,
                 B_Index   => 0,
                 C         => Matrix_mQ,
                 C_Index   => 0,
                 numDigits => Private_Key.Modulus.CurrentLen,
                 carry     => carry);

         --  clear sensitive information
         Matrix_cP.Zero_Array;
         Matrix_cQ.Zero_Array;
         Matrix_mP.Zero_Array;
         Matrix_mQ.Zero_Array;
         Matrix_t1.Zero_Array;
         Matrix_t2.Zero_Array;
         Matrix_t3.Zero_Array;
         Matrix_t4.Zero_Array;

         --  encode output to standard form
         return NN_Encode (HugeNumber => Matrix_t5,
                           numDigits  => Natural (Private_Key.MsgSize));
      end;

   end Encrypt_to_PKCS;


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
         when 10 => return "Encryption: Message length doesn't match modulus.";
         when 11 => return "Encryption: Message too long for modulus.";
         when 12 => return "Radix-64 encoding bad, found pad away from end.";
         when 13 => return "Radix-64 encoding bad, character previous to " &
                           "pad doesn't have clear trailing bits.";
         when 14 => return "Encryption: Incorrect Block Type found.";
         when 15 => return "Encryption: Encrypted array of bytes is too " &
                           "long for modulus.";
         when 16 => return "Encryption: Internal separator is too short.";
         when 17 => return "PKCS coding: Byte String longer than modulus.";
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
              MsgSize   => MsgSize,
              ErrorCode => code,
              Modulus   => Modulus_Array,
              Exponent  => Exponent_Array);

   end Build_Public_Key;



   -------------------------
   --  Build_Private_Key  --
   -------------------------

   function Build_Private_Key (Modulus          : String;
                               Public_Exponent  : String;
                               Private_Exponent : String;
                               Prime_p          : String;
                               Prime_q          : String;
                               Prime_Exp_p      : String;
                               Prime_Exp_q      : String;
                               coefficient      : String)
   return TPrivateKey is
      code              : PrivateKeyError := 0;
      KeySize           : TKeySize := 1024;
      MsgSize           : QuadByteMatrixLen := 0;
      Modulus_Array     : QuadByteMatrix.TData := QuadByteMatrix.Construct;
      PubExp_Array      : QuadByteMatrix.TData := QuadByteMatrix.Construct;
      PrivExp_Array     : QuadByteMatrix.TData := QuadByteMatrix.Construct;
      PrimeP_Array      : QuadByteMatrix.TData := QuadByteMatrix.Construct;
      PrimeQ_Array      : QuadByteMatrix.TData := QuadByteMatrix.Construct;
      PrimeExpP_Array   : QuadByteMatrix.TData := QuadByteMatrix.Construct;
      PrimeExpQ_Array   : QuadByteMatrix.TData := QuadByteMatrix.Construct;
      Coefficient_Array : QuadByteMatrix.TData := QuadByteMatrix.Construct;
   begin

      declare
         num_bits_modulus : constant Natural := Modulus'Length * 4;
         num_bits_pubexp  : constant Natural := Public_Exponent'Length * 4;
         num_bits_privexp : constant Natural := Private_Exponent'Length * 4;
         num_bits_p       : constant Natural := Prime_p'Length * 4;
         num_bits_q       : constant Natural := Prime_q'Length * 4;
         num_bits_expp    : constant Natural := Prime_Exp_p'Length * 4;
         num_bits_expq    : constant Natural := Prime_Exp_q'Length * 4;
         num_bits_coeff   : constant Natural := coefficient'Length * 4;

         --  Error codes: 0 means no error
         --               1 means modulus not multiple of 32
         --               2 means one of the 2 exponents is not multiple of 32
         --               3 means one of the 5 prime comp. is not mult of 32
         --               4 means modulus and both exponents not all equal
         --               5 means the 5 prime components are not all equal
         --               6 means the prime components are not 2x mod/exp
         --               7 means derived keysize < 1024
         --               8 means derived keysize > 4096
      begin
         if num_bits_modulus rem 32 /= 0 then
            code := 1;
            goto complete;
         end if;
         if (num_bits_pubexp  rem 32 /= 0) or
            (num_bits_privexp rem 32 /= 0) then
            code := 2;
            goto complete;
         end if;
         if (num_bits_p     rem 32 /= 0) or
            (num_bits_q     rem 32 /= 0) or
            (num_bits_expp  rem 32 /= 0) or
            (num_bits_expq  rem 32 /= 0) or
            (num_bits_coeff rem 32 /= 0) then
            code := 3;
            goto complete;
         end if;
         if not ((num_bits_modulus = num_bits_pubexp) and
                 (num_bits_modulus = num_bits_privexp)) then
            code := 4;
            goto complete;
         end if;
         if not ((num_bits_p = num_bits_q) and
                 (num_bits_p = num_bits_expp) and
                 (num_bits_p = num_bits_expq) and
                 (num_bits_p = num_bits_coeff)) then
            code := 5;
            goto complete;
         end if;
         if num_bits_p * 2 /= num_bits_modulus then
            code := 6;
            goto complete;
         end if;
         if num_bits_modulus < L_MODULUS_BITS then
            code := 7;
            goto complete;
         end if;
         if num_bits_modulus > U_MODULUS_BITS then
            code := 8;
            goto complete;
         end if;
         KeySize := TKeySize (num_bits_modulus);
         MsgSize := QuadByteMatrixLen (num_bits_modulus / 8);
      end;

      declare
         pumod : constant TBinaryString := Decode_HexString (Modulus);
         puexp : constant TBinaryString := Decode_HexString (Public_Exponent);
         prexp : constant TBinaryString := Decode_HexString (Private_Exponent);
         prp   : constant TBinaryString := Decode_HexString (Prime_p);
         prq   : constant TBinaryString := Decode_HexString (Prime_q);
         prxp  : constant TBinaryString := Decode_HexString (Prime_Exp_p);
         prxq  : constant TBinaryString := Decode_HexString (Prime_Exp_q);
         prcof : constant TBinaryString := Decode_HexString (coefficient);
      begin
         Modulus_Array     := NN_Decode (pumod);
         PubExp_Array      := NN_Decode (puexp);
         PrivExp_Array     := NN_Decode (prexp);
         PrimeP_Array      := NN_Decode (prp);
         PrimeQ_Array      := NN_Decode (prq);
         PrimeExpP_Array   := NN_Decode (prxp);
         PrimeExpQ_Array   := NN_Decode (prxq);
         Coefficient_Array := NN_Decode (prcof);
      end;

   <<complete>>

      return (KeySize          => KeySize,
              MsgSize          => MsgSize,
              ErrorCode        => code,
              Modulus          => Modulus_Array,
              Public_Exponent  => PubExp_Array,
              Private_Exponent => PrivExp_Array,
              Prime_p          => PrimeP_Array,
              Prime_q          => PrimeQ_Array,
              Prime_Exp_p      => PrimeExpP_Array,
              Prime_Exp_q      => PrimeExpQ_Array,
              coefficient      => Coefficient_Array);

   end Build_Private_Key;



   -----------------------
   --  Cryption_Status  --
   -----------------------

   function Cryption_Status return TCryptoError is
   begin
      return Last_Error;
   end Cryption_Status;

end RSA_Frontend;
