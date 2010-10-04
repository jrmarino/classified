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


with Ada.Numerics.Discrete_Random;
with NN;      use NN;

package body RSA_Frontend is

   --------------------------------
   --  Decrypt_With_Private_Key  --
   --------------------------------

   function Decrypt_With_Private_Key (Private_Key   : in  TPrivateKey;
                                      Scrambled_R64 : in  String)
   return String is
      Bytes_Mod : constant Positive := Positive (Private_Key.KeySize) / 8;
      scrambled : constant TBinaryString := Decode_Radix64 (Scrambled_R64);
      OutputLen : Integer;
      z         : Natural := 2;
   begin
      Last_Error := Get_Radix_Coding_Status;
      if Last_Error /= 0 then
         return error_msg;
      end if;

      if scrambled'Length > Bytes_Mod then
         Last_Error := 8;  -- Encrypted message is larger than key modulus
         return error_msg;
      end if;

      declare
         Revealed_Bytecode : constant TBinaryString := Private_Transformation (
                                          Private_Key => Private_Key,
                                          Bytecode   => scrambled);
      begin
         if DPKCS_Error /= 0 then
            Last_Error := DPKCS_Error;
            return error_msg;
         end if;

         if (Revealed_Bytecode (0) /= 0) or else
            (Revealed_Bytecode (1) /= 2) then
            Last_Error := 6;  --  Decrypt error (doesn't start with 02)
            return error_msg;
         end if;

         declare
            maxZ      : constant Integer := Bytes_Mod - 1;
         begin
            Samantha :
               loop
                  exit Samantha when z > maxZ;
                  exit Samantha when Revealed_Bytecode (z) = 0;
                  z := z + 1;
               end loop Samantha;
            if (Revealed_Bytecode (z) /= 0) or else (z < 10) then
               Last_Error := 6;  --  Decrypt error (no zero bookend or pad < 8)
                                 --  means msg at least 11 chars less than mod
               return error_msg;
            end if;
         end;

         z := z + 1;
         OutputLen := Bytes_Mod - z;
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
         Last_Error := 8;  -- Encrypted message is larger than key modulus
         return error_msg;
      end if;

      declare
         Revealed_Bytecode : constant TBinaryString := Public_Transformation (
                                          Public_Key => Public_Key,
                                          Bytecode   => scrambled);
      begin
         if DPKCS_Error /= 0 then
            Last_Error := DPKCS_Error;
            return error_msg;
         end if;

         if (Revealed_Bytecode (0) /= 0) or else
            (Revealed_Bytecode (1) /= 1) then
            Last_Error := 6;  --  Decrypt error (doesn't start with 01)
            return error_msg;
         end if;

         declare
            maxZ      : constant Integer := Bytes_Mod - 1;
         begin
            Samantha :
               loop
                  exit Samantha when z > maxZ;
                  exit Samantha when Revealed_Bytecode (z) = 0;
                  z := z + 1;
               end loop Samantha;
            if (Revealed_Bytecode (z) /= 0) or else (z < 10) then
               Last_Error := 6;  --  Decrypt error (no zero bookend or pad < 8)
                                 --  means msg at least 11 chars less than mod
               return error_msg;
            end if;
         end;

         z := z + 1;
         OutputLen := Bytes_Mod - z;
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
      InputLen : constant Natural := Plain_Text'Length;
      Plain_Bytecode : TBinaryString := Message_Template (
                        ModulusSize => Private_Key.MsgSize,
                        MessageSize => InputLen,
                        BlockType   => 1);
      border   : constant Natural :=
                          Natural (Private_Key.MsgSize) - InputLen - 1;
   begin
      Last_Error := 0;

      if InputLen + 11 > Natural (Private_Key.MsgSize) then
         Last_Error := 7;  -- Encryption: Message too long for modulus.
         return error_msg;
      end if;

      for x in Integer range 1 .. InputLen loop
         Plain_Bytecode (border + x) := Character'Pos (Plain_Text (x));
      end loop;

      declare
         Encrypted_Bytecode : constant TBinaryString :=
                                 Private_Transformation (
                                       Private_Key => Private_Key,
                                       Bytecode    => Plain_Bytecode);
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
            Last_CRCR64 := CRC_Radix64 (Encrypted_Bytecode);
            return result;
         end;
      end;

   end Encrypt_With_Private_Key;



   -------------------------------
   --  Encrypt_With_Public_Key  --
   -------------------------------

   function Encrypt_With_Public_Key (Public_Key    : in  TPublicKey;
                                     Plain_Text    : in  String)
   return String is
      InputLen : constant Natural := Plain_Text'Length;
      Plain_Bytecode : TBinaryString := Message_Template (
                        ModulusSize => Public_Key.MsgSize,
                        MessageSize => InputLen,
                        BlockType   => 2);
      border   : constant Natural :=
                          Natural (Public_Key.MsgSize) - InputLen - 1;
   begin
      Last_Error := 0;

      if InputLen + 11 > Natural (Public_Key.MsgSize) then
         Last_Error := 7;  -- Encryption: Message too long for modulus.
         return error_msg;
      end if;

      for x in Integer range 1 .. InputLen loop
         Plain_Bytecode (border + x) := Character'Pos (Plain_Text (x));
      end loop;

      declare
         Encrypted_Bytecode : constant TBinaryString :=
                                 Public_Transformation (
                                       Public_Key  => Public_Key,
                                       Bytecode    => Plain_Bytecode);
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
            Last_CRCR64 := CRC_Radix64 (Encrypted_Bytecode);
            return result;
         end;
      end;

   end Encrypt_With_Public_Key;



   -----------------------------
   --  Public_Transformation  --
   -----------------------------

   function Public_Transformation (Public_Key : TPublicKey;
                                   Bytecode   : TBinaryString)
   return TBinaryString is
      matrix_M   : QuadByteMatrix.TData;
      matrix_C   : QuadByteMatrix.TData;
      error_result : constant TBinaryString (0 .. 0) := (0 => 0);
   begin
      DPKCS_Error := 0;
      matrix_M := NN_Decode (Bytecode);

      if matrix_M.CurrentLen > Public_Key.MsgSize or else
         matrix_M.Compared_With (
                     Index     => 0,
                     ExtData   => Public_Key.Modulus,
                     ExtDigits => Public_Key.MsgSize) >= 0 then
         DPKCS_Error := 8; --  Data Mismatch, message length > key modulus
                           --  Should never get here, this is normally checked
                           --  before this function is called.
         return error_result;
      end if;

      --  Compute c = m^e mod n.  To perform actual RSA calc.
      matrix_C := NN_ModExp (LHS    => matrix_M,
                             RHS    => Public_Key.Exponent,
                             Modulo => Public_Key.Modulus);

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

   end Public_Transformation;



   ------------------------------
   --  Private_Transformation  --
   ------------------------------

   function Private_Transformation (Private_Key  : TPrivateKey;
                                    Bytecode     : TBinaryString)
   return TBinaryString is
      Matrix_c    : QuadByteMatrix.TData;
      PKCSErr     : constant TBinaryString (0 .. 2) := (5, 5, 5);
   begin

      --  decode required input data from standard form
      Matrix_c    := NN_Decode (HexString => Bytecode);

      if Matrix_c.Compared_With (
                        Index     => 0,
                        ExtData   => Private_Key.Modulus,
                        ExtDigits => Private_Key.Modulus.CurrentLen) >= 0 then
         EPKCS_Error := 7;  -- should never get here
         return PKCSErr;
      end if;

      --  Compute mP = cP^dP mod p  and  mQ = cQ^dQ mod q.
      --  (Assumes q has length at most pDigits, i.e., p > q)

      declare
         Matrix_cP     : QuadByteMatrix.TData := QuadByteMatrix.Construct;
         Matrix_cQ     : QuadByteMatrix.TData := QuadByteMatrix.Construct;
         Matrix_mP     : QuadByteMatrix.TData := QuadByteMatrix.Construct;
         Matrix_mQ     : QuadByteMatrix.TData := QuadByteMatrix.Construct;
         Matrix_t1     : QuadByteMatrix.TData := QuadByteMatrix.Construct;
         Matrix_t2     : QuadByteMatrix.TData := QuadByteMatrix.Construct;
         Matrix_t3     : QuadByteMatrix.TData := QuadByteMatrix.Construct;
         Matrix_t4     : QuadByteMatrix.TData := QuadByteMatrix.Construct;
         Matrix_t5     : QuadByteMatrix.TData := QuadByteMatrix.Construct;
         Matrix_t6     : QuadByteMatrix.TData := QuadByteMatrix.Construct;
         Matrix_Random : QuadByteMatrix.TData := NN_Random_Number;
         Matrix_Blind  : constant QuadByteMatrix.TData := NN_Blind (
                               Real_Matrix   => Matrix_c,
                               Random_Number => Matrix_Random,
                               pub_exponent  => Private_Key.Public_Exponent,
                               pub_modulus   => Private_Key.Modulus);
         borrow        : MQuadByte;
         carry         : MQuadByte;
      begin
         Matrix_cP := NN_Mod (Dividend => Matrix_Blind,
                              Divisor  => Private_Key.Prime_p);

         Matrix_cQ := NN_Mod (Dividend => Matrix_Blind,
                              Divisor  => Private_Key.Prime_q);

         Matrix_mP := NN_ModExp (LHS    => Matrix_cP,
                                 RHS    => Private_Key.Prime_Exp_p,
                                 Modulo => Private_Key.Prime_p);

         Matrix_mQ := NN_ModExp (LHS    => Matrix_cQ,
                                 RHS    => Private_Key.Prime_Exp_q,
                                 Modulo => Private_Key.Prime_q);

         --  Chinese Remainder Theorem:
         --  m = ((((mP - mQ) mod p) * qInv) mod p) * q + mQ

         if Matrix_mP.Compared_With (
                        Index     => 0,
                        ExtData   => Matrix_mQ,
                        ExtDigits => Private_Key.Prime_p.CurrentLen) >= 0 then
            NN_Sub (Result    => Matrix_t2, Res_Index => 0,
                    LHS       => Matrix_mP, LHS_Index => 0,
                    RHS       => Matrix_mQ, RHS_Index => 0,
                    numDigits => Private_Key.Prime_p.CurrentLen,
                    borrow    => borrow);
         else
            NN_Sub (Result    => Matrix_t1, Res_Index => 0,
                    LHS       => Matrix_mQ, LHS_Index => 0,
                    RHS       => Matrix_mP, RHS_Index => 0,
                    numDigits => Private_Key.Prime_p.CurrentLen,
                    borrow    => borrow);

            NN_Sub (Result    => Matrix_t2,           Res_Index => 0,
                    LHS       => Private_Key.Prime_p, LHS_Index => 0,
                    RHS       => Matrix_t1,           RHS_Index => 0,
                    numDigits => Private_Key.Prime_p.CurrentLen,
                    borrow    => borrow);
         end if;

         Matrix_t3 := NN_ModMult (LHS    => Matrix_t2,
                                  RHS    => Private_Key.coefficient,
                                  Modulo => Private_Key.Prime_p);

         Matrix_t4 := NN_Mult (LHS => Matrix_t3,
                               RHS => Private_Key.Prime_q);

         NN_Add (Result    => Matrix_t5,
                 LHS       => Matrix_t4,
                 RHS       => Matrix_mQ,
                 numDigits => Private_Key.Modulus.CurrentLen,
                 carry     => carry);

         Matrix_t6 := NN_Unblind (Blinded_Matrix => Matrix_t5,
                                  Random_Number  => Matrix_Random,
                                  pub_modulus    => Private_Key.Modulus);

         --  clear sensitive information
         Matrix_cP.Zero_Array;
         Matrix_cQ.Zero_Array;
         Matrix_mP.Zero_Array;
         Matrix_mQ.Zero_Array;
         Matrix_t1.Zero_Array;
         Matrix_t2.Zero_Array;
         Matrix_t3.Zero_Array;
         Matrix_t4.Zero_Array;
         Matrix_t5.Zero_Array;
         Matrix_Random.Zero_Array;

         --  encode output to standard form
         return NN_Encode (HugeNumber => Matrix_t6,
                           numDigits  => Natural (Private_Key.MsgSize));
      end;

   end Private_Transformation;


   ----------------------------
   --  Print_Status_Message  --
   ----------------------------

   function Get_Status_Message (Status : TCryptoError)
   return String is
   begin
      --  RFC 3447 standard messages:
      --      "message too long"  (encryption)
      --      "decryption error"  (decryption)
      case Status is
         when  0 => return "No errors.";
         when  1 => return "Radix-64 message length not divisible by 4.";
         when  2 => return "Radix-64 encoding bad, 7th bit used or mismapped.";
         when  3 => return "Radix-64 encoding bad, 4th place pad doesn't " &
                           "follow 3rd place pad.";
         when  4 => return "Radix-64 encoding bad, found pad away from end.";
         when  5 => return "Radix-64 encoding bad, character previous to " &
                           "pad doesn't have clear trailing bits.";
         when  6 => return "decryption error";
         when  7 => return "message too long";

         when  8 => return "Encrypted message is larger than key modulus.";
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
         if (num_bits_pubexp  rem 32 /= 0) or else
            (num_bits_privexp rem 32 /= 0) then
            code := 2;
            goto complete;
         end if;
         if (num_bits_p     rem 32 /= 0) or else
            (num_bits_q     rem 32 /= 0) or else
            (num_bits_expp  rem 32 /= 0) or else
            (num_bits_expq  rem 32 /= 0) or else
            (num_bits_coeff rem 32 /= 0) then
            code := 3;
            goto complete;
         end if;
         if not ((num_bits_modulus = num_bits_pubexp) and then
                 (num_bits_modulus = num_bits_privexp)) then
            code := 4;
            goto complete;
         end if;
         if not ((num_bits_p = num_bits_q) and then
                 (num_bits_p = num_bits_expp) and then
                 (num_bits_p = num_bits_expq) and then
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



   ------------------------
   --  Message_Template  --
   ------------------------

   function Message_Template (ModulusSize : QuadByteMatrixLen;
                              MessageSize : Natural;
                              BlockType   : TBlockType)
   return TBinaryString is
      type TRandomByte is range 1 .. MByte'Last;
      package RandByte is new Ada.Numerics.Discrete_Random (TRandomByte);
      Byte_Gen  : RandByte.Generator;
      BMax      : constant Natural := Natural (ModulusSize) - 1;
      Bytecode  : TBinaryString (0 .. BMax) := (others => 0);
      PadLength : constant Natural := Natural (ModulusSize) - 3 - MessageSize;
   begin
      --  Index 0: must be zero
      --  Index 1: most be 1 or 2 (BlockType)
      --  Index 2 - X: Random padding, but can't be zero
      --  Index X + 1: must be zero
      --  remainder:   message goes here, leave as zero

      Bytecode (1) := MByte (BlockType);

      RandByte.Reset (Byte_Gen);
      for x in Natural range 2 .. PadLength - 1 loop
         Bytecode (x) := MByte (RandByte.Random (Byte_Gen));
      end loop;

      return Bytecode;

   end Message_Template;



   ------------------------
   --  Get_CRC_Checksum  --
   ------------------------

   function Get_CRC_Checksum
   return CRCR64String is
   begin
      return Last_CRCR64;
   end Get_CRC_Checksum;

end RSA_Frontend;
