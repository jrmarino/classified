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

package body NN is

   ---------------------
   --  NN_Sub (Quad)  --
   ---------------------

   procedure NN_Sub (A         : out QuadByteMatrix.TData;
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
         A.Matrix (Andx) := temp;
         Andx := Andx + 1;
         Bndx := Bndx + 1;
         Cndx := Cndx + 1;
      end loop;
      A.CurrentLen := A.Significant_Length;

   end NN_Sub;



   -------------
   --  NN_Add --
   -------------

   procedure NN_Add (A         : out QuadByteMatrix.TData;
                     A_Index   : in  QuadByteDigitIndex;
                     B         : in  QuadByteMatrix.TData;
                     B_Index   : in  QuadByteDigitIndex;
                     C         : in  QuadByteMatrix.TData;
                     C_Index   : in  QuadByteDigitIndex;
                     numDigits : in  QuadByteMatrixLen;
                     carry     : out MQuadByte)
   is
      temp      : MQuadByte          := 0;
      Andx      : QuadByteDigitIndex := A_Index;
      Bndx      : QuadByteDigitIndex := B_Index;
      Cndx      : QuadByteDigitIndex := C_Index;
   begin
      carry := 0;

      for x in QuadByteMatrixLen range 1 .. numDigits loop
         temp := B.Matrix (Bndx) + carry;
         if temp < carry then
            temp := C.Matrix (Cndx);
         else
            temp := Flowguard_Add (temp, C.Matrix (Cndx));
            if temp < C.Matrix (Cndx) then
               carry := 1;
            else
               carry := 0;
            end if;
         end if;
         A.Matrix (Andx) := temp;
         Andx := Andx + 1;
         Bndx := Bndx + 1;
         Cndx := Cndx + 1;
      end loop;
      A.CurrentLen := A.Significant_Length;

   end NN_Add;



   ---------------
   --  NN_Mult  --
   ---------------

   procedure NN_Mult (A       : out QuadByteMatrix.TData;
                      A_Index : in  QuadByteDigitIndex;
                      B       : in  QuadByteMatrix.TData;
                      B_Index : in  QuadByteDigitIndex;
                      C       : in  QuadByteMatrix.TData;
                      C_Index : in  QuadByteDigitIndex)
   is
      MaxB  : constant QuadByteDigitIndex :=
                       QuadByteDigitIndex (B.CurrentLen - 1);
      MaxC  : constant QuadByteDigitIndex :=
                       QuadByteDigitIndex (C.CurrentLen - 1);
      carry : MQuadByte;
      High  : MQuadByte;
      Low   : MQuadByte;
      bndx  : QuadByteDigitIndex;
      cndx  : QuadByteDigitIndex;
      zndx  : QuadByteDigitIndex;

      ZZ_Internal : QuadByteMatrix.TData := QuadByteMatrix.Construct;
   begin
      --  This computes A = B * C

      B_Loop :
         for k in QuadByteDigitIndex range 0 .. MaxB loop
            carry := 0;
            bndx  := B_Index + k;
            if B.Matrix (bndx) /= 0 then
               C_Loop :
                  for j in QuadByteDigitIndex range 0 .. MaxC loop
                     cndx := C_Index + j;
                     DMult (LHS        => B.Matrix (bndx),
                            RHS        => C.Matrix (cndx),
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
            zndx := k + MaxC + 1;
            ZZ_Internal.Matrix (zndx) := ZZ_Internal.Matrix (zndx) + carry;
         end loop B_Loop;

      ZZ_Internal.CurrentLen := ZZ_Internal.Significant_Length;
      ZZ_Internal.CopyTo (destination => A);

   end NN_Mult;



   -----------------
   --  NN_LShift  --
   -----------------

   procedure NN_LShift (A         : out QuadByteMatrix.TData;
                        A_Index   : in  QuadByteDigitIndex;
                        B         : in  QuadByteMatrix.TData;
                        B_Index   : in  QuadByteDigitIndex;
                        numBits   : in  TDigit;
                        numDigits : in  QuadByteMatrixLen;
                        carry     : out MQuadByte)
   is
      factor     : constant MQuadByte := MQuadByte (2 ** Natural (numBits));
      mask       : constant MQuadByte := factor - 1;
      invmask    : constant MQuadByte := MQuadByte'Last xor mask;
      Andx       : QuadByteDigitIndex := A_Index;
      Bndx       : QuadByteDigitIndex := B_Index;
      last_carry : MQuadByte;
   begin
      --  This computes A := B * 2^numBits, returns carry and modifies A
      carry := 0;
      if numBits = 0 then
         if numDigits > 0 then
            B.CopyTo (destination => A);
         end if;
         return;
      end if;

      --  In Ada, the mod type doesn't shift bits off, they just rotate.
      --  Therefore we need to clear the right-most bits before or'ing
      --  the previous carry.  Before we clear, we copy the bits, because
      --  this is the carry for the next element.

      for x in QuadByteMatrixLen range 1 .. numDigits loop
         last_carry      := carry;
         A.Matrix (Andx) := B.Matrix (Bndx) * factor;
         carry           := A.Matrix (Andx) and mask;
         A.Matrix (Andx) := A.Matrix (Andx) and invmask;
         A.Matrix (Andx) := A.Matrix (Andx) or  last_carry;
         Andx            := Andx + 1;
         Bndx            := Bndx + 1;
      end loop;

   end NN_LShift;



   -----------------
   --  NN_RShift  --
   -----------------

   procedure NN_RShift (A         : out QuadByteMatrix.TData;
                        A_Index   : in  QuadByteDigitIndex;
                        B         : in  QuadByteMatrix.TData;
                        B_Index   : in  QuadByteDigitIndex;
                        numBits   : in  TDigit;
                        numDigits : in  QuadByteMatrixLen;
                        carry     : out MQuadByte)
   is
      factor     : constant MQuadByte := MQuadByte (2 ** Natural (numBits));
      mask       : constant MQuadByte := factor - 1;
      invmask    : constant MQuadByte := MQuadByte'Last xor mask;
      BBndx      : QuadByteDigitIndex;
      AAndx      : QuadByteDigitIndex;
      last_carry : MQuadByte;
   begin
   --  This computes A := B / 2^bits, returns carry and modifies A
      carry := 0;
      if numBits = 0 then
         if numDigits > 0 then
            B.CopyTo (destination => A);
         end if;
         return;
      end if;

      --  In Ada, the mod type doesn't shift bits off, they just rotate.
      --  The "carry" bits start off in the right-most position, so we will
      --  copy them into the carry variable, and then clear them, and finally
      --  or the previous carry in their place.  After the shift/rotation,
      --  the new carry will be in the correct location.

      for x in QuadByteMatrixLen range 1 .. numDigits loop
         last_carry       := carry;
         BBndx            := B_Index + QuadByteDigitIndex (numDigits - x);
         AAndx            := A_Index + QuadByteDigitIndex (numDigits - x);
         carry            := B.Matrix (BBndx) and mask;
         A.Matrix (AAndx) := B.Matrix (BBndx) and invmask;
         A.Matrix (AAndx) := A.Matrix (AAndx) or  last_carry;
         A.Matrix (AAndx) := A.Matrix (AAndx) * factor;
      end loop;

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
                     C        : in  QuadByteMatrix.TData;
                     C_Index  : in  QuadByteDigitIndex;
                     C_Digits : in  QuadByteMatrixLen;
                     D        : in  QuadByteMatrix.TData;
                     D_Index  : in  QuadByteDigitIndex;
                     D_Digits : in  QuadByteMatrixLen)
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
   begin
      --  This computes the modulus and dividend of C divided by D
      --  e.g. dividend = int (C/D) and modulus = C mod D

      ResDiv.Zero_Array;
      ResMod.Zero_Array;
      DD_Digits := D.CurrentLen;
      if DD_Digits = 0 then
         return;
      end if;


      shift := TDigit (NN_DIGIT_BITS - Integer (D.Significant_Bits
               (index => QuadByteDigitIndex (DD_Digits - 1))));

      NN_RShift (A         => CC_Internal, A_Index   => 0,
                 B         => C,           B_Index   => 0,
                 numBits   => shift,
                 numDigits => C_Digits,
                 carry     => carry);
      CC_Internal.Matrix (QuadByteDigitIndex (C_Digits)) := carry;

      NN_RShift (A         => DD_Internal, A_Index   => 0,
                 B         => D,           B_Index   => 0,
                 numBits   => shift,
                 numDigits => DD_Digits,
                 carry     => carry);
      S := DD_Internal.Matrix (QuadByteDigitIndex (DD_Digits - 1));


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
                  AHigh := MDualByte (T1) / (CHigh + 1);
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
               T1 := T1 - MQuadByte (High_Half (V));

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
               NN_Sub (A         => CC_Internal,
                       A_Index   => K,
                       B         => CC_Internal,
                       B_Index   => K,
                       C         => DD_Internal,
                       C_Index   => 0,
                       numDigits => DD_Digits,
                       borrow    => borrow);
               CC_Internal.Matrix (ndx) := CC_Internal.Matrix (ndx) -
                                           borrow;
            end loop;
            ResDiv.Matrix (K) := AI;

            exit While_One when K = 0;
            K := K - 1;
         end loop While_One;

      NN_RShift (A         => ResMod,
                 A_Index   => 0,
                 B         => CC_Internal,
                 B_Index   => 0,
                 numBits   => shift,
                 numDigits => DD_Digits,
                 carry     => carry);

      ResMod.CurrentLen := ResMod.Significant_Length;
      ResDiv.CurrentLen := ResDiv.Significant_Length;

   end NN_Div;



   ------------------
   --  NN_ModMult  --
   ------------------

   procedure NN_ModMult (A         : out QuadByteMatrix.TData;
                         B         : in  QuadByteMatrix.TData;
                         B_Index   : in  QuadByteDigitIndex;
                         C         : in  QuadByteMatrix.TData;
                         C_Index   : in  QuadByteDigitIndex;
                         D         : in  QuadByteMatrix.TData;
                         D_Index   : in  QuadByteDigitIndex;
                         numDigits : in  QuadByteMatrixLen)
   is
      intermediate  : QuadByteMatrix.TData := QuadByteMatrix.Construct;
      Work_Dividend : QuadByteMatrix.TData := QuadByteMatrix.Construct;
   begin
      --  This computes A = (b * C) mod D
      NN_Mult (A       => intermediate,
               A_Index => 0,
               B       => B,
               B_Index => B_Index,
               C       => C,
               C_Index => C_Index);

      NN_Div (ResDiv   => Work_Dividend,
              ResMod   => A,
              C        => intermediate,
              C_Index  => 0,
              C_Digits => 2 * numDigits,
              D        => D,
              D_Index  => D_Index,
              D_Digits => numDigits);

      A.CurrentLen := A.Significant_Length;

   end NN_ModMult;



   -----------------
   --  NN_ModExp  --
   -----------------

   procedure NN_ModExp (A         : out QuadByteMatrix.TData;
                        B         : in  QuadByteMatrix.TData;
                        B_Index   : in  QuadByteDigitIndex;
                        C         : in  QuadByteMatrix.TData;
                        C_Index   : in  QuadByteDigitIndex;
                        C_Digits  : in  QuadByteMatrixLen;
                        D         : in  QuadByteMatrix.TData;
                        D_Index   : in  QuadByteDigitIndex;
                        D_Digits  : in  QuadByteMatrixLen)
   is
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
   begin
      --  Computes a = b^c mod d.  assumes d > 0.
      --  Store b, b^2 mod d, and b^3 mod d
      B.CopyTo (BPower (0));
      NN_ModMult (A         => BPower (1),
                  B         => BPower (0),
                  B_Index   => B_Index,
                  C         => B,
                  C_Index   => B_Index,
                  D         => D,
                  D_Index   => D_Index,
                  numDigits => D_Digits);
      NN_ModMult (A         => BPower (2),
                  B         => BPower (1),
                  B_Index   => B_Index,
                  C         => B,
                  C_Index   => B_Index,
                  D         => D,
                  D_Index   => D_Index,
                  numDigits => D_Digits);
      T.Assign_Zero_Digit (1);

      Outer_Loop :
         loop
            ci     := C.Matrix (K + C_Index);
            ciBits := NN_DIGIT_BITS;

            --  Scan past leading zero bits of most significant digit.
            if K = QuadByteDigitIndex (C_Digits - 1) then
               while Digit_2MSB (ci) = 0 loop
                  ci := ci * MQuadByte (4);  -- shift left 2 bits
                  ciBits := ciBits - 2;
               end loop;
            end if;

            J := 0;
            while J < ciBits loop
               --  Compute t = t^4 * b^s mod d, where s = two MSB's of ci.

               NN_ModMult (A         => Tp1,
                           B         => T,
                           B_Index   => 0,
                           C         => T,
                           C_Index   => 0,
                           D         => D,
                           D_Index   => D_Index,
                           numDigits => D_Digits);
               NN_ModMult (A         => Tp2,
                           B         => Tp1,
                           B_Index   => 0,
                           C         => Tp1,
                           C_Index   => 0,
                           D         => D,
                           D_Index   => D_Index,
                           numDigits => D_Digits);

               S := Digit_2MSB (ci);
               if S /= 0 then
                  NN_ModMult (A         => T,
                              B         => Tp2,
                              B_Index   => 0,
                              C         => BPower (Integer (S) - 1),
                              C_Index   => 0,
                              D         => D,
                              D_Index   => D_Index,
                              numDigits => D_Digits);
               end if;

               J := J + 2;
               ci := ci * MQuadByte (4);  -- shift left 2 bits
            end loop;

            exit Outer_Loop when K = 0;
            K := K - 1;
         end loop Outer_Loop;

      T.CopyTo (A);
      A.CurrentLen := A.Significant_Length;

   end NN_ModExp;


   -----------------
   --  NN_Decode  --
   -----------------

   function NN_Decode (HexString : TBinaryString)
   return QuadByteMatrix.TData is
      result    : QuadByteMatrix.TData := QuadByteMatrix.Construct;
      OutputLen : constant Natural := (HexString'Length + 3) / 4;
      j         : Natural := OutputLen - 1;
      k         : QuadByteDigitIndex := 0;
      u         : Natural;
      t         : MQuadByte;
      shifted   : MQuadByte;
      factor    : MQuadByte;
   begin
      Outer_Loop :
         loop
            t := 0;
            u := 0;
            Inner_Loop :
               loop
                  factor  := MQuadByte (2 ** u);
                  shifted := MQuadByte (HexString (j)) * factor;
                  t := t or shifted;
                  exit Inner_Loop when j = 0;
                  j := j - 1;
                  u := u + 8;
                  exit Inner_Loop when u >= NN_DIGIT_BITS;
               end loop Inner_Loop;

            result.Matrix (k) := t;
            exit Outer_Loop when j = 0;
            exit Outer_Loop when k = QuadByteDigitIndex (NN_Digits);
            k := k + 1;

         end loop Outer_Loop;

      result.CurrentLen := result.Significant_Length;
      return result;

   end NN_Decode;



   -----------------
   --  NN_Encode  --
   -----------------

   function NN_Encode (HugeNumber : QuadByteMatrix.TData;
                       numDigits  : QuadByteMatrixLen)
   return TBinaryString is
      multiple  : constant Natural := NN_DIGIT_BITS / 8;
      kmax      : constant QuadByteDigitIndex :=
                           QuadByteDigitIndex (numDigits) - 1;
      resultLen : constant Natural := Natural (numDigits) * multiple;
      result    : TBinaryString (0 .. resultLen - 1) := (others => 0);
      index     : Natural := resultLen - 1;
      u         : Natural;
      shifted   : MQuadByte;
      factor    : MQuadByte;
   begin

      for k in QuadByteDigitIndex range 0 .. kmax loop
         u := 0;
         for x in Natural range 1 .. multiple loop
            factor  := MQuadByte (2 ** u);
            shifted := (HugeNumber.Matrix (k) / factor) and 16#FF#;
            result (index) := MByte (shifted);

            u     := u + 8;
            index := index - 1;
         end loop;
      end loop;

      return result;

   end NN_Encode;


end NN;
