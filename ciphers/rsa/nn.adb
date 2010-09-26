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


with RSA_Utilities; use RSA_Utilities;
with Ada.Numerics.Discrete_Random;

package body NN is

   --------------
   --  NN_Sub  --
   --------------

   procedure NN_Sub (Result    : out QuadByteMatrix.TData;
                     A_Index   : in  QuadByteDigitIndex;
                     B         : in  QuadByteMatrix.TData;
                     B_Index   : in  QuadByteDigitIndex;
                     C         : in  QuadByteMatrix.TData;
                     C_Index   : in  QuadByteDigitIndex;
                     numDigits : in  QuadByteMatrixLen;
                     borrow    : out MQuadByte)
   is
      temp      : MQuadByte;
      Andx      : QuadByteDigitIndex := A_Index;
      Bndx      : QuadByteDigitIndex := B_Index;
      Cndx      : QuadByteDigitIndex := C_Index;
   begin
      borrow := 0;

      for x in QuadByteMatrixLen range 1 .. numDigits loop
         temp := B.Matrix (Bndx) - borrow;
         if temp = MAX_NN_DIGIT then
            temp := MAX_NN_DIGIT - C.Matrix (Cndx);
         else
            temp := Flowguard_Sub (temp, C.Matrix (Cndx));
            if temp > Flowguard_Sub (MAX_NN_DIGIT, C.Matrix (Cndx)) then
               borrow := 1;
            else
               borrow := 0;
            end if;
         end if;
         Result.Matrix (Andx) := temp;
         Andx := Andx + 1;
         Bndx := Bndx + 1;
         Cndx := Cndx + 1;
      end loop;
      Result.CurrentLen := Result.Significant_Length;

   end NN_Sub;



   -------------
   --  NN_Add --
   -------------

   procedure NN_Add (Result    : out QuadByteMatrix.TData;
                     LHS       : in  QuadByteMatrix.TData;
                     RHS       : in  QuadByteMatrix.TData;
                     numDigits : in  QuadByteMatrixLen;
                     carry     : out MQuadByte)
   is
      temp  : MQuadByte          := 0;
      index : QuadByteDigitIndex := 0;
   begin
      carry := 0;

      for x in QuadByteMatrixLen range 1 .. numDigits loop
         temp := LHS.Matrix (index) + carry;
         if temp < carry then
            temp := RHS.Matrix (index);
         else
            temp := Flowguard_Add (temp, RHS.Matrix (index));
            if temp < RHS.Matrix (index) then
               carry := 1;
            else
               carry := 0;
            end if;
         end if;
         Result.Matrix (index) := temp;
         index := index + 1;
      end loop;
      Result.CurrentLen := Result.Significant_Length;

   end NN_Add;



   ---------------
   --  NN_Mult  --
   ---------------

   function NN_Mult (LHS : QuadByteMatrix.TData;
                     RHS : QuadByteMatrix.TData)
   return QuadByteMatrix.TData is
      MaxLHS : constant QuadByteDigitIndex :=
                        QuadByteDigitIndex (LHS.CurrentLen - 1);
      MaxRHS : constant QuadByteDigitIndex :=
                        QuadByteDigitIndex (RHS.CurrentLen - 1);
      carry : MQuadByte;
      High  : MQuadByte;
      Low   : MQuadByte;
      zndx  : QuadByteDigitIndex;

      ZZ_Internal : QuadByteMatrix.TData := QuadByteMatrix.Construct;
   begin
      --  This computes result = LHS * RHS

      B_Loop :
         for k in QuadByteDigitIndex range 0 .. MaxLHS loop
            carry := 0;
            if LHS.Matrix (k) /= 0 then
               C_Loop :
                  for j in QuadByteDigitIndex range 0 .. MaxRHS loop
                     DMult (LHS        => LHS.Matrix (k),
                            RHS        => RHS.Matrix (j),
                            ResultHigh => High,
                            ResultLow  => Low);
                     zndx := k + j;
                     ZZ_Internal.Matrix (zndx) :=
                           Flowguard_Add (ZZ_Internal.Matrix (zndx), carry);
                     if ZZ_Internal.Matrix (zndx) < carry then
                        carry := 1;
                     else
                        carry := 0;
                     end if;
                     ZZ_Internal.Matrix (zndx) :=
                           Flowguard_Add (ZZ_Internal.Matrix (zndx), Low);
                     if ZZ_Internal.Matrix (zndx) < Low then
                        carry := carry + 1;
                     end if;
                     carry := carry + High;
                  end loop C_Loop;
            end if;

            zndx := k + MaxRHS + 1;
            ZZ_Internal.Matrix (zndx) :=
                  Flowguard_Add (ZZ_Internal.Matrix (zndx), carry);
         end loop B_Loop;

      ZZ_Internal.CurrentLen := ZZ_Internal.Significant_Length;
      return ZZ_Internal;

   end NN_Mult;



   -----------------
   --  NN_LShift  --
   -----------------

   procedure NN_LShift (Result    : out QuadByteMatrix.TData;
                        LHS       : in  QuadByteMatrix.TData;
                        numBits   : in  TDigit;
                        carry     : out MQuadByte)
   is
   begin
      --  This computes Result := LHS * 2^numBits, returns carry
      carry := 0;
      if numBits = 0 then
         LHS.CopyTo (destination => Result);
         return;
      end if;

      Result.Zero_Array;
      declare
         factor     : constant MQuadByte := MQuadByte (2 ** Natural (numBits));
         numDigits  : constant QuadByteMatrixLen := LHS.CurrentLen;
         antibits   : constant Natural := NN_DIGIT_BITS - Natural (numBits);
         antifactor : constant MQuadByte := MQuadByte (2 ** antibits);
         antimask   : constant MQuadByte := antifactor - 1;
         mask       : constant MQuadByte := MQuadByte'Last xor antimask;
         last_carry : MQuadByte;
         index      : QuadByteDigitIndex := 0;
      begin
         for x in QuadByteMatrixLen range 1 .. numDigits loop
            last_carry       := carry;
            carry            := (LHS.Matrix (index) and mask) / antifactor;
            Result.Matrix (index) := (LHS.Matrix (index) and antimask) *
                                      factor;
            Result.Matrix (index) := Result.Matrix (index) or last_carry;
            index := index + 1;
         end loop;
      end;
      Result.CurrentLen := Result.Significant_Length;

   end NN_LShift;



   -----------------
   --  NN_RShift  --
   -----------------

   procedure NN_RShift (Result    : out QuadByteMatrix.TData;
                        LHS       : in  QuadByteMatrix.TData;
                        numBits   : in  TDigit;
                        carry     : out MQuadByte)
   is
   begin
   --  This computes Result := LHS / 2^bits, returns carry
      carry := 0;
      if numBits = 0 then
         LHS.CopyTo (destination => Result);
         return;
      end if;

      Result.Zero_Array;
      declare
         factor     : constant MQuadByte := MQuadByte (2 ** Natural (numBits));
         mask       : constant MQuadByte := factor - 1;
         numDigits  : constant QuadByteMatrixLen := LHS.CurrentLen;
         antibits   : constant Natural := NN_DIGIT_BITS - Natural (numBits);
         antifactor : constant MQuadByte := MQuadByte (2 ** antibits);
         last_carry : MQuadByte;
         index      : QuadByteDigitIndex;
      begin
         for x in QuadByteMatrixLen range 1 .. numDigits loop
            last_carry := carry * antifactor;
            index      := QuadByteDigitIndex (numDigits - x);
            carry      := LHS.Matrix (index) and mask;

            Result.Matrix (index) := (LHS.Matrix (index) / factor) or
                                      last_carry;
         end loop;
      end;
      Result.CurrentLen := Result.Significant_Length;

   end NN_RShift;



   ----------------------
   --  Sub_Digit_Mult  --
   ----------------------

   procedure Sub_Digit_Mult (A         : out QuadByteMatrix.TData;
                             A_Index   : in  QuadByteDigitIndex;
                             B         : in  QuadByteMatrix.TData;
                             B_Index   : in  QuadByteDigitIndex;
                             C         : in  MQuadByte;
                             D         : in  QuadByteMatrix.TData;
                             D_Index   : in  QuadByteDigitIndex;
                             numDigits : in  QuadByteMatrixLen;
                             borrow    : out MQuadByte)
   is
      High      : MQuadByte;
      Low       : MQuadByte;
      Andx      : QuadByteDigitIndex;
      Bndx      : QuadByteDigitIndex;
      Dndx      : QuadByteDigitIndex;
   begin
      borrow := 0;
      if C = 0 then
         return;
      end if;

      for K in QuadByteDigitIndex
               range 0 .. QuadByteDigitIndex (numDigits - 1) loop
         Andx := K + A_Index;
         Bndx := K + B_Index;
         Dndx := K + D_Index;

         DMult (LHS        => C,
                RHS        => D.Matrix (Dndx),
                ResultHigh => High,
                ResultLow  => Low);

         A.Matrix (Andx) := Flowguard_Sub (B.Matrix (Bndx), borrow);
         if A.Matrix (Andx) > Flowguard_Sub (MAX_NN_DIGIT, borrow) then
            borrow := 1;
         else
            borrow := 0;
         end if;
         A.Matrix (Andx) := Flowguard_Sub (A.Matrix (Andx), Low);
         if A.Matrix (Andx) > Flowguard_Sub (MAX_NN_DIGIT, Low) then
            borrow := borrow + 1;
         end if;
         borrow := borrow + High;
      end loop;

   end Sub_Digit_Mult;



   --------------
   --  NN_Div  --
   --------------

   procedure NN_Div (ResDiv   : out QuadByteMatrix.TData;
                     ResMod   : out QuadByteMatrix.TData;
                     LHS      : in  QuadByteMatrix.TData;
                     RHS      : in  QuadByteMatrix.TData)
   is
      shift      : TDigit;
      carry      : MQuadByte;
      borrow     : MQuadByte;
      DD_Digits  : QuadByteMatrixLen;
      CC_Pointer : QuadByteDigitIndex;
      K          : QuadByteDigitIndex;
      ndx        : QuadByteDigitIndex;
      S          : MQuadByte;
      U          : MQuadByte;
      V          : MQuadByte;
      AI         : MQuadByte;
      T0         : MQuadByte;
      T1         : MQuadByte;
      postShiftL : MQuadByte;
      CHigh      : MDualByte;
      CLow       : MDualByte;
      AHigh      : MDualByte;
      ALow       : MDualByte;

      CC_Internal : QuadByteMatrix.TData := QuadByteMatrix.Construct;
      DD_Internal : QuadByteMatrix.TData := QuadByteMatrix.Construct;
      C_Digits    : constant QuadByteMatrixLen := LHS.CurrentLen;
   begin
      --  This computes the modulus and dividend of LHS divided by RHS
      --  e.g. dividend = int (LHS/RHS) and modulus = LHS mod RHS

      ResDiv.Zero_Array;
      ResMod.Zero_Array;
      DD_Digits := RHS.CurrentLen;
      if DD_Digits = 0 then
         return;
      end if;


      shift := TDigit (NN_DIGIT_BITS - RHS.Significant_Bits
               (index => QuadByteDigitIndex (DD_Digits - 1)));

      NN_LShift (Result    => CC_Internal,
                 LHS       => LHS,
                 numBits   => shift,
                 carry     => carry);
      CC_Internal.Matrix (QuadByteDigitIndex (C_Digits)) := carry;

      NN_LShift (Result    => DD_Internal,
                 LHS       => RHS,
                 numBits   => shift,
                 carry     => carry);
      S := DD_Internal.Matrix (QuadByteDigitIndex (DD_Digits - 1));

      if DD_Digits > C_Digits then
         goto Determine_Modulus;
      end if;

      K := QuadByteDigitIndex (C_Digits - DD_Digits);
      While_One :
         loop
            if S = MAX_NN_DIGIT then
               AI := CC_Internal.Matrix (K + QuadByteDigitIndex (DD_Digits));
            else
               CC_Pointer := K + QuadByteDigitIndex (DD_Digits) - 1;

               S := S + 1;
               CHigh := High_Half (S);
               CLow  := Low_Half (S);

               T0 := CC_Internal.Matrix (CC_Pointer);
               T1 := CC_Internal.Matrix (CC_Pointer + 1);

               if CHigh = MDualByte (MAX_NN_HALF_DIGIT) then
                  AHigh := High_Half (T1);
               else
                  AHigh := MDualByte (T1 / MQuadByte (CHigh + 1));
               end if;
               U := Flowguard_Mult (AHigh, CLow);
               V := Flowguard_Mult (AHigh, CHigh);
               postShiftL := Shift_To_High_Half (
                                   MDualByte (U and MAX_NN_HALF_DIGIT));

               T0 := Flowguard_Sub (T0, postShiftL);
               if T0 > Flowguard_Sub (MAX_NN_DIGIT, postShiftL) then
                  T1 := T1 - 1;
               end if;
               T1 := T1 - MQuadByte (High_Half (U)) - V;

               while (T1 > MQuadByte (CHigh)) or
                     ((T1 = MQuadByte (CHigh)) and (T0 >= Shift_To_High_Half
                            (CLow))) loop
                  postShiftL := Shift_To_High_Half (CLow);
                  T0 := Flowguard_Sub (T0, postShiftL);
                  if T0 > Flowguard_Sub (MAX_NN_DIGIT, postShiftL) then
                     T1 := T1 - 1;
                  end if;
                  T1 := T1 - MQuadByte (CHigh);
                  AHigh := AHigh + 1;
               end loop;

               if MQuadByte (CHigh) = MAX_NN_HALF_DIGIT then
                  ALow := High_Half (T1);
               else
                  ALow := MDualByte (Flowguard_Add (
                          MQuadByte (Shift_To_High_Half (MDualByte (T1))),
                          MQuadByte (High_Half (T0))) /
                          MQuadByte (CHigh + 1));
               end if;
               U  := Flowguard_Mult (ALow, CLow);
               V  := Flowguard_Mult (ALow, CHigh);
               T0 := Flowguard_Sub  (T0, U);
               if T0 > Flowguard_Sub (MAX_NN_DIGIT, U) then
                  T1 := T1 - 1;
               end if;
               postShiftL := Shift_To_High_Half
                             (MDualByte (V and MAX_NN_HALF_DIGIT));
               T0 := Flowguard_Sub (T0, postShiftL);
               if T0 > Flowguard_Sub (MAX_NN_DIGIT, postShiftL) then
                  T1 := T1 - 1;
               end if;
               T1 := Flowguard_Sub (T1, MQuadByte (High_Half (V)));

               while (T1 > 0) or ((T1 = 0) and (T0 >= S)) loop
                  T0 := Flowguard_Sub (T0, S);
                  if T0 > Flowguard_Sub (MAX_NN_DIGIT, S) then
                     T1 := T1 - 1;
                  end if;
                  ALow := ALow + 1;
               end loop;

               AI := Shift_To_High_Half (AHigh) + MQuadByte (ALow);
               S  := S - 1;
            end if;
            Sub_Digit_Mult (A         => CC_Internal,
                            A_Index   => K,
                            B         => CC_Internal,
                            B_Index   => K,
                            C         => AI,
                            D         => DD_Internal,
                            D_Index   => 0,
                            numDigits => DD_Digits,
                            borrow    => borrow);
            ndx := K + QuadByteDigitIndex (DD_Digits);
            CC_Internal.Matrix (ndx) := CC_Internal.Matrix (ndx) - borrow;

            while (CC_Internal.Matrix (ndx) > 0) or
                  (CC_Internal.Compared_With (
                    Index     => K,
                    ExtData   => DD_Internal,
                    ExtDigits => DD_Digits) >= 0) loop
               AI := AI + 1;
               NN_Sub (Result    => CC_Internal,
                       A_Index   => K,
                       B         => CC_Internal,
                       B_Index   => K,
                       C         => DD_Internal,
                       C_Index   => 0,
                       numDigits => DD_Digits,
                       borrow    => borrow);
               CC_Internal.Matrix (ndx) := Flowguard_Sub (
                                           CC_Internal.Matrix (ndx), borrow);
            end loop;
            ResDiv.Matrix (K) := AI;

            exit While_One when K = 0;
            K := K - 1;
         end loop While_One;

<<Determine_Modulus>>

      NN_RShift (Result    => ResMod,
                 LHS       => CC_Internal,
                 numBits   => shift,
                 carry     => carry);

      ResMod.CurrentLen := ResMod.Significant_Length;
      ResDiv.CurrentLen := ResDiv.Significant_Length;

   end NN_Div;



   --------------
   --  NN_Mod  --
   --------------

   function NN_Mod (Dividend : QuadByteMatrix.TData;
                    Divisor  : QuadByteMatrix.TData)
   return QuadByteMatrix.TData is
      scratch : QuadByteMatrix.TData := QuadByteMatrix.Construct;
      result  : QuadByteMatrix.TData := QuadByteMatrix.Construct;
   begin
      NN_Div (ResDiv => scratch,
              ResMod => result,
              LHS    => Dividend,
              RHS    => Divisor);
      return result;
   end NN_Mod;



   ------------------
   --  NN_ModMult  --
   ------------------

   function NN_ModMult (LHS    : QuadByteMatrix.TData;
                        RHS    : QuadByteMatrix.TData;
                        Modulo : QuadByteMatrix.TData)
   return QuadByteMatrix.TData is
      intermediate    : QuadByteMatrix.TData := QuadByteMatrix.Construct;
      result_dividend : QuadByteMatrix.TData := QuadByteMatrix.Construct;
      result_modulus  : QuadByteMatrix.TData := QuadByteMatrix.Construct;
   begin
   --  This computes result = (LHS * RHS) mod Modulo
      intermediate := NN_Mult (LHS => LHS, RHS => RHS);

      NN_Div (ResDiv   => result_dividend,
              ResMod   => result_modulus,
              LHS      => intermediate,
              RHS      => Modulo);

      return result_modulus;

   end NN_ModMult;



   -----------------
   --  NN_ModExp  --
   -----------------

   function NN_ModExp (LHS    : QuadByteMatrix.TData;
                       RHS    : QuadByteMatrix.TData;
                       Modulo : QuadByteMatrix.TData)
   return QuadByteMatrix.TData is
      C_Digits  : constant QuadByteMatrixLen := RHS.CurrentLen;
      K         : QuadByteDigitIndex := QuadByteDigitIndex (C_Digits - 1);
      ci        : MQuadByte;
      S         : MQuadByte;
      ciBits    : Integer;
      J         : Integer := 0;

      T      : QuadByteMatrix.TData := QuadByteMatrix.Construct;
      Tp1    : QuadByteMatrix.TData := QuadByteMatrix.Construct;
      Tp2    : QuadByteMatrix.TData := QuadByteMatrix.Construct;
      BPower : array (0 .. 2) of QuadByteMatrix.TData := (
                                 QuadByteMatrix.Construct,
                                 QuadByteMatrix.Construct,
                                 QuadByteMatrix.Construct);
      invmask : constant MQuadByte := MQuadByte'Last xor 3;
   begin
      --  This computes result = (LHS ^ RHS) mod Modulo
      --  Store LHS, LHS^2 mod Modulo, and LHS^3 mod Modulo
      LHS.CopyTo (destination => BPower (0));

      BPower (1) := NN_ModMult (LHS    => LHS,
                                RHS    => LHS,
                                Modulo => Modulo);

      BPower (2) := NN_ModMult (LHS    => BPower (1),
                                RHS    => LHS,
                                Modulo => Modulo);

      T.Assign_Zero_Digit (1);

      Outer_Loop :
         loop
            ci     := RHS.Matrix (K);
            ciBits := NN_DIGIT_BITS;
            --  Scan past leading zero bits of most significant digit.
            if K = QuadByteDigitIndex (C_Digits - 1) then
               while Digit_2MSB (ci) = 0 loop
                  ci := (ci * MQuadByte (4)) and invmask;  -- shift left 2 bits
                  ciBits := ciBits - 2;
               end loop;
            end if;
            J := 0;
            while J < ciBits loop
               --  Compute t = t^4 * b^s mod d, where s = two MSB's of ci.

               Tp1 := NN_ModMult (LHS    => T,
                                  RHS    => T,
                                  Modulo => Modulo);

               Tp2 := NN_ModMult (LHS    => Tp1,
                                  RHS    => Tp1,
                                  Modulo => Modulo);

               S := Digit_2MSB (ci);
               if S /= 0 then
                  T := NN_ModMult (LHS    => Tp2,
                                   RHS    => BPower (Integer (S) - 1),
                                   Modulo => Modulo);
               else
                  Tp2.CopyTo (destination => T);
               end if;

               J := J + 2;
               ci := ci * MQuadByte (4);  -- shift left 2 bits
            end loop;

            exit Outer_Loop when K = 0;
            K := K - 1;
         end loop Outer_Loop;

      return T;

   end NN_ModExp;


   -----------------
   --  NN_Decode  --
   -----------------

   function NN_Decode (HexString : TBinaryString)
   return QuadByteMatrix.TData is
      result    : QuadByteMatrix.TData := QuadByteMatrix.Construct;
      multiple  : constant Natural := NN_DIGIT_BITS / 8;
      OutputLen : constant Natural := (HexString'Length + 3) / multiple;
      kmax      : constant QuadByteDigitIndex :=
                           QuadByteDigitIndex (OutputLen - 1);
      index     : Natural := HexString'Length;
      u         : Natural;
      t         : MQuadByte;
      shifted   : MQuadByte;
      factor    : MQuadByte;
   begin
      for k in QuadByteDigitIndex range 0 .. kmax loop
         t := 0;
         u := 0;
         for x in Natural range 1 .. multiple loop
            if index > 0 then
               index   := index - 1;
               factor  := MQuadByte (2 ** u);
               shifted := MQuadByte (HexString (index)) * factor;
               t := t or shifted;
               u := u + 8;
            end if;
         end loop;
         result.Matrix (k) := t;
      end loop;

      result.CurrentLen := result.Significant_Length;
      return result;

   end NN_Decode;



   -----------------
   --  NN_Encode  --
   -----------------

   function NN_Encode (HugeNumber : QuadByteMatrix.TData;
                       numDigits  : Natural)
   return TBinaryString is
      multiple  : constant Natural := NN_DIGIT_BITS / 8;
      kmax      : constant QuadByteDigitIndex :=
                           QuadByteDigitIndex (numDigits / multiple) - 1;
      resultLen : constant Natural := numDigits;
      result    : TBinaryString (0 .. resultLen - 1) := (others => 0);
      index     : Natural := resultLen;
      u         : Natural;
      shifted   : MQuadByte;
      factor    : MQuadByte;
   begin

      for k in QuadByteDigitIndex range 0 .. kmax loop
         u := 0;
         for x in Natural range 1 .. multiple loop
            index := index - 1;
            factor  := MQuadByte (2 ** u);
            shifted := (HugeNumber.Matrix (k) / factor) and 16#FF#;
            result (index) := MByte (shifted);

            u     := u + 8;
         end loop;
      end loop;

      return result;

   end NN_Encode;



   ------------------------
   --  NN_Random_Number  --
   ------------------------

   function NN_Random_Number return QuadByteMatrix.TData is
      type TRandomQB is range 1 .. MQuadByte'Last;
      package RandQuadByte is new Ada.Numerics.Discrete_Random (TRandomQB);
      result : QuadByteMatrix.TData := QuadByteMatrix.Construct;
      QuadByte_Gen : RandQuadByte.Generator;
   begin
      result.CurrentLen := 1;
      RandQuadByte.Reset (QuadByte_Gen);

      --  The following code was disabled because it's not really needed.
      --  The modulus by definition is the product of prime numbers p and q
      --  So the only random numbers candidates that wouldn't have a GCD
      --  of 1 are p and q.  Since we limit our candidates to a mere 4.3B
      --  which is many magnitudes less than the smallest prime, we are
      --  guaranteed that GCD always equals 1.

      --  Sara :
      --    loop
      --       result.Matrix (0) :=
      --              MQuadByte (RandQuadByte.Random (QuadByte_Gen));
      --       exit Sara when NN_GCD_Is_1 (Modulus   => Modulus,
      --                                  Candidate => result);
      --    end loop Sara;

      result.Matrix (0) := MQuadByte (RandQuadByte.Random (QuadByte_Gen));
      return result;

   end NN_Random_Number;


   ----------------
   --  NN_Blind  --
   ----------------

   function NN_Blind (Real_Matrix   : QuadByteMatrix.TData;
                      Random_Number : QuadByteMatrix.TData;
                      pub_exponent  : QuadByteMatrix.TData;
                      pub_modulus   : QuadByteMatrix.TData)
   return QuadByteMatrix.TData is
      temp   : QuadByteMatrix.TData := QuadByteMatrix.Construct;
   begin
      --  temp = random^pubexp mod pubmod
      temp := NN_ModExp (LHS    => Random_Number,
                         RHS    => pub_exponent,
                         Modulo => pub_modulus);

      return NN_ModMult (LHS    => temp,
                         RHS    => Real_Matrix,
                         Modulo => pub_modulus);

   end NN_Blind;



   ------------------
   --  NN_Unblind  --
   ------------------

   function NN_Unblind (Blinded_Matrix : QuadByteMatrix.TData;
                        Random_Number  : QuadByteMatrix.TData;
                        pub_modulus    : QuadByteMatrix.TData)
   return QuadByteMatrix.TData is
      RandInv : QuadByteMatrix.TData := QuadByteMatrix.Construct;
   begin

      RandInv := NN_ModInv (Value   => Random_Number,
                            Modulus => pub_modulus);

      return NN_ModMult (LHS    => RandInv,
                         RHS    => Blinded_Matrix,
                         Modulo => pub_modulus);

   end NN_Unblind;



   -----------------
   --  NN_ModInv  --
   -----------------

   function NN_ModInv (Value   : QuadByteMatrix.TData;
                       Modulus : QuadByteMatrix.TData)
   return QuadByteMatrix.TData is
      type TU1Sign is range -1 .. 1;
      u1Sign  : TU1Sign := 1;
      q       : QuadByteMatrix.TData := QuadByteMatrix.Construct;
      t1      : QuadByteMatrix.TData := QuadByteMatrix.Construct;
      t3      : QuadByteMatrix.TData := QuadByteMatrix.Construct;
      u1      : QuadByteMatrix.TData := QuadByteMatrix.Construct;
      u3      : QuadByteMatrix.TData := QuadByteMatrix.Construct;
      v1      : QuadByteMatrix.TData := QuadByteMatrix.Construct;
      v3      : QuadByteMatrix.TData := QuadByteMatrix.Construct;
      w       : QuadByteMatrix.TData := QuadByteMatrix.Construct;
      discard : MQuadByte;
   begin
      --  Apply extended Euclidean algorithm, modified to avoid
      --  negative numbers
      u1.Assign_Zero_Digit (1);
      Value.CopyTo (u3);
      Modulus.CopyTo (v3);

      Macarena :
         loop
            exit Macarena when v3.IsZero;

            NN_Div (ResDiv => q,
                    ResMod => t3,
                    LHS    => u3,
                    RHS    => v3);

            w := NN_Mult (LHS => q, RHS => v1);

            NN_Add (Result    => t1,
                    LHS       => u1,
                    RHS       => w,
                    numDigits => Modulus.CurrentLen,
                    carry     => discard);

            v1.CopyTo (destination => u1);
            t1.CopyTo (destination => v1);
            v3.CopyTo (destination => u3);
            t3.CopyTo (destination => v3);
            u1Sign := -1 * u1Sign;

         end loop Macarena;

      --  Negate result if sign is negative.
      if u1Sign < 0 then
         declare
            result : QuadByteMatrix.TData := QuadByteMatrix.Construct;
         begin
            NN_Sub (Result    => result,
                    A_Index   => 0,
                    B         => Modulus,
                    B_Index   => 0,
                    C         => u1,
                    C_Index   => 0,
                    numDigits => Modulus.CurrentLen,
                    borrow    => discard);
            return result;
         end;
      else
         return u1;
      end if;

   end NN_ModInv;



   -------------------
   --  NN_GCD_Is_1  --
   -------------------

   function NN_GCD_Is_1 (Modulus   : QuadByteMatrix.TData;
                         Candidate : QuadByteMatrix.TData)
   return Boolean is
      type TJ is range 0 .. 2;
      type TG is array (TJ) of QuadByteMatrix.TData;
      g          : TG;
      j          : TJ := 1;
      jplus1     : TJ := j + 1;
      jminus1    : TJ := j - 1;
   begin
      Modulus.CopyTo (destination => g (jminus1));
      Candidate.CopyTo (destination => g (j));

      while not g (j).IsZero loop
         g (jplus1) := NN_Mod (Dividend => g (jminus1),
                               Divisor  => g (j));
         case j is
            when 0 =>   j       := 1;
                        jplus1  := 2;
                        jminus1 := 0;
            when 1 =>   j       := 2;
                        jplus1  := 0;
                        jminus1 := 1;
            when 2 =>   j       := 0;
                        jplus1  := 1;
                        jminus1 := 2;
         end case;
      end loop;

      return g (jminus1).IsOne;
   end NN_GCD_Is_1;



end NN;
