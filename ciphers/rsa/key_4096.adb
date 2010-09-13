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

package body Key_4096 is


   --  Internal "scratch paper" variables, saves constant memory allocation
   Work_Product  : QuadByteMatrix.TData := QuadByteMatrix.Construct;
   Work_Dividend : QuadByteMatrix.TData := QuadByteMatrix.Construct;
   CC_Internal   : QuadByteMatrix.TData := QuadByteMatrix.Construct;
   DD_Internal   : QuadByteMatrix.TData := QuadByteMatrix.Construct;
   Isolation     : QuadByteMatrix.TData := QuadByteMatrix.Construct;

   BPower        : array (0 .. 2) of QuadByteMatrix.TData := (
                                    QuadByteMatrix.Construct,
                                    QuadByteMatrix.Construct,
                                    QuadByteMatrix.Construct);

   ---------------------
   --  NN_Sub (Quad)  --
   ---------------------

   procedure NN_Sub (A      : out QuadByteMatrix.TData;
                     B      : in  QuadByteMatrix.TData;
                     C      : in  QuadByteMatrix.TData;
                     index  : in  QuadByteDigitIndex;
                     matlen : in  QuadByteMatrixLen;
                     borrow : out MQuadByte)
   is
      C_Index   : QuadByteDigitIndex := 0;
      AB_Index  : QuadByteDigitIndex := index;
      numDigits : QuadByteMatrixLen  := matlen;
      temp      : MQuadByte;
   begin
      borrow := 0;
      if numDigits = 0 then
         return;
      end if;

      Repeat_Until :
         loop
            temp := B.Matrix (AB_Index) - borrow;
            if temp = MAX_NN_DIGIT then
               temp := MAX_NN_DIGIT - C.Matrix (C_Index);
            else
               temp := Flowguard_Sub (temp, C.Matrix (C_Index));
               if temp > Flowguard_Sub (MAX_NN_DIGIT, C.Matrix (C_Index)) then
                  borrow := 1;
               else
                  borrow := 0;
               end if;
            end if;
            A.Matrix (AB_Index) := temp;
            C_Index   := C_Index + 1;
            AB_Index  := AB_Index + 1;
            numDigits := numDigits - 1;
            exit Repeat_Until when numDigits = 0;
         end loop Repeat_Until;
         A.CurrentLen := A.Significant_Length;
   end NN_Sub;



   -------------
   --  NN_Add --
   -------------

   procedure NN_Add (A      : out QuadByteMatrix.TData;
                     B      : in  QuadByteMatrix.TData;
                     C      : in  QuadByteMatrix.TData;
                     index  : in  QuadByteDigitIndex;
                     matlen : in  QuadByteMatrixLen;
                     carry  : out MQuadByte)
   is
      C_Index   : QuadByteDigitIndex    := 0;
      AB_Index  : QuadByteDigitIndex := index;
      numDigits : QuadByteMatrixLen     := matlen;
      temp      : MQuadByte;
   begin
      carry := 0;
      if numDigits = 0 then
         return;
      end if;

      Repeat_Until :
         loop
            temp := B.Matrix (AB_Index) + carry;
            if temp < carry then
               temp := C.Matrix (C_Index);
            else
               temp := Flowguard_Add (temp, C.Matrix (C_Index));
               if temp < C.Matrix (C_Index) then
                  carry := 1;
               else
                  carry := 0;
               end if;
            end if;
            A.Matrix (AB_Index) := temp;
            C_Index   := C_Index + 1;
            AB_Index  := AB_Index + 1;
            numDigits := numDigits - 1;
            exit Repeat_Until when numDigits = 0;
         end loop Repeat_Until;
         A.CurrentLen := A.Significant_Length;

   end NN_Add;



   ------------------
   --  NN_ModMult  --
   ------------------

   procedure NN_ModMult (A : out QuadByteMatrix.TData;
                         B : in  QuadByteMatrix.TData;
                         C : in  QuadByteMatrix.TData;
                         D : in  QuadByteMatrix.TData)
   is
   begin
      --  This computes A = (b * C) mod D
      NN_Mult   (A => Work_Product, B => B, C => C);
      NN_Divide (ResDiv => Work_Dividend,
                 ResMod => A,
                 C      => Work_Product,
                 D      => D);
      A.CurrentLen := A.Significant_Length;

   end NN_ModMult;



   ---------------
   --  NN_Mult  --
   ---------------

   procedure NN_Mult (A : out QuadByteMatrix.TData;
                      B : in  QuadByteMatrix.TData;
                      C : in  QuadByteMatrix.TData)
   is
      MaxB  : constant QuadByteDigitIndex :=
                       QuadByteDigitIndex (B.CurrentLen - 1);
      MaxC  : constant QuadByteDigitIndex :=
                       QuadByteDigitIndex (C.CurrentLen - 1);
      carry : MQuadByte;
      High  : MQuadByte;
      Low   : MQuadByte;
      ndx   : QuadByteDigitIndex;
   begin
      --  This computes A = B * C
      A.Zero_Array;

      B_Loop :
         for k in QuadByteDigitIndex range 0 .. MaxB loop
            carry := 0;
            if B.Matrix (k) /= 0 then
               C_Loop :
                  for j in QuadByteDigitIndex range 0 .. MaxC loop
                     ndx := k + j;
                     DMult (LHS        => B.Matrix (k),
                            RHS        => C.Matrix (j),
                            ResultHigh => High,
                            ResultLow  => Low);
                     A.Matrix (ndx) := Flowguard_Add (A.Matrix (ndx), carry);
                     if A.Matrix (ndx) < carry then
                        carry := 1;
                     else
                        carry := 0;
                     end if;
                     A.Matrix (ndx) := Flowguard_Add (A.Matrix (ndx), Low);
                     if A.Matrix (ndx) < Low then
                        carry := carry + 1;
                     end if;
                     carry := carry + High;
                  end loop C_Loop;
               ndx := k + MaxC + 1;
               A.Matrix (ndx) := A.Matrix (ndx) + carry;
            end if;
         end loop B_Loop;
      A.CurrentLen := A.Significant_Length;
   end NN_Mult;



   -----------------
   --  NN_Divide  --
   -----------------

   procedure NN_Divide (ResDiv : out QuadByteMatrix.TData;
                        ResMod : out QuadByteMatrix.TData;
                        C      : in  QuadByteMatrix.TData;
                        D      : in  QuadByteMatrix.TData)
   is
      shift      : TDigit;
      carry      : MQuadByte;
      borrow     : MQuadByte;
      index      : QuadByteDigitIndex;
      CC_Pointer : QuadByteDigitIndex;
      DD_Digits  : QuadByteDigitIndex;
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
   begin
      --  This computes the modulus and dividend of C divided by D
      --  e.g. dividend = int (C/D) and modulus = C mod D

      ResDiv.Zero_Array;
      ResMod.Zero_Array;
      if D.CurrentLen = 0 then
         return;
      end if;
      CC_Internal.Zero_Array;
      DD_Internal.Zero_Array;

      index := QuadByteDigitIndex (D.CurrentLen) - 1;
      shift := D.Significant_Bits (index => index);
      shift := TDigit (NN_DIGIT_BITS - Integer (shift));
      CC_Internal.NN_RShift (B     => C,
                             bits  => shift,
                             carry => carry);
      CC_Internal.Matrix (QuadByteDigitIndex (C.CurrentLen)) := carry;
      DD_Internal.NN_RShift (B     => D,
                             bits  => shift,
                             carry => carry);
      CC_Internal.CurrentLen := CC_Internal.Significant_Length;
      DD_Internal.CurrentLen := DD_Internal.Significant_Length;

      DD_Digits := QuadByteDigitIndex (DD_Internal.CurrentLen);
      S         := DD_Internal.Matrix (DD_Digits - 1);

      if QuadByteDigitIndex (C.CurrentLen) >= DD_Digits then
         K := QuadByteDigitIndex (C.CurrentLen) - DD_Digits;
         While_One :
            loop
               if S = MAX_NN_DIGIT then
                  AI := CC_Internal.Matrix (K + DD_Digits);
               else
                  CC_Pointer := K + DD_Digits - 1;

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
                               (CLow and MDualByte (MAX_NN_HALF_DIGIT)))) loop
                     postShiftL := Shift_To_High_Half
                                   (CLow and MDualByte (MAX_NN_HALF_DIGIT));
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
                     if T0 > (MAX_NN_DIGIT - S) then
                        T1 := T1 - 1;
                     end if;
                     ALow := ALow + 1;
                  end loop;

                  AI := Shift_To_High_Half (AHigh) + MQuadByte (ALow);
                  S  := S - 1;
               end if;

               Sub_Digit_Mult (A      => CC_Internal,
                               AIndex => K,
                               C      => AI,
                               D      => DD_Internal,
                               borrow => borrow);
               ndx := K + DD_Digits;
               CC_Internal.Matrix (ndx) := CC_Internal.Matrix (ndx) - borrow;

               while (CC_Internal.Matrix (ndx) > 0) or
                     (CC_Internal.Compared_With (
                       Index     => K,
                       ExtData   => DD_Internal,
                       ExtDigits => QuadByteMatrixLen (DD_Digits)) >= 0) loop
                  AI := AI + 1;
                  NN_Sub (A      => CC_Internal,
                          B      => CC_Internal,
                          C      => DD_Internal,
                          index  => K,
                          matlen => QuadByteMatrixLen (DD_Digits),
                          borrow => borrow);
                  CC_Internal.Matrix (ndx) := CC_Internal.Matrix (ndx) -
                                              borrow;
               end loop;
               ResDiv.Matrix (K) := AI;

               exit While_One when K = 0;
               K := K - 1;
            end loop While_One;
      end if;
      ResMod.NN_RShift (B     => CC_Internal,
                        bits  => shift,
                        carry => carry);
      ResMod.CurrentLen := ResMod.Significant_Length;
      ResDiv.CurrentLen := ResDiv.Significant_Length;

   end NN_Divide;


   ----------------------
   --  Sub_Digit_Mult  --
   ----------------------

   procedure Sub_Digit_Mult (A      : out QuadByteMatrix.TData;
                             AIndex : in  QuadByteDigitIndex;
                             C      : in  MQuadByte;
                             D      : in  QuadByteMatrix.TData;
                             borrow : out MQuadByte)
   is
      numDigits : constant QuadByteDigitIndex  :=
                           QuadByteDigitIndex (D.CurrentLen);
      K         : QuadByteDigitIndex := 0;
      High      : MQuadByte;
      Low       : MQuadByte;
      ndx       : QuadByteDigitIndex;
   begin
      A.Zero_Array;
      borrow := 0;
      if C = 0 then
         return;
      end if;

      while K < numDigits loop
         DMult (LHS        => C,
                RHS        => D.Matrix (K),
                ResultHigh => High,
                ResultLow  => Low);
         ndx := AIndex + K;
         A.Matrix (ndx) := Flowguard_Sub (A.Matrix (ndx), borrow);
         if A.Matrix (ndx) > Flowguard_Sub (MAX_NN_DIGIT, borrow) then
            borrow := 1;
         else
            borrow := 0;
         end if;
         A.Matrix (ndx) := Flowguard_Sub (A.Matrix (ndx), Low);
         if A.Matrix (ndx) > Flowguard_Sub (MAX_NN_DIGIT, Low) then
            borrow := borrow + 1;
         end if;
         borrow := borrow + High;
         K := K + 1;
      end loop;

   end Sub_Digit_Mult;


   -----------------
   --  NN_ModExp  --
   -----------------

   procedure NN_ModExp (A : out QuadByteMatrix.TData;
                        B : in  QuadByteMatrix.TData;
                        C : in  QuadByteMatrix.TData;
                        D : in  QuadByteMatrix.TData)
   is
      numDigits : constant QuadByteDigitIndex  :=
                           QuadByteDigitIndex (C.CurrentLen);
      K         : QuadByteDigitIndex := numDigits - 1;
      ci        : MQuadByte;
      S         : MQuadByte;
      ciBits    : Integer := NN_DIGIT_BITS;
      J         : Integer := 0;
   begin
      --  Computes a = b^c mod d.  assumes d > 0.
      Isolation.Zero_Array;
      BPower (0).Zero_Array;
      BPower (1).Zero_Array;
      BPower (2).Zero_Array;
      A.Zero_Array;

      B.CopyTo (BPower (0));
      NN_ModMult (BPower (1), BPower (0), B, D);   --  a = (b * c) mod d
      NN_ModMult (BPower (2), BPower (1), B, D);
      A.Assign_Zero_Digit (1);

      Outer_Loop :
         loop
            ci := C.Matrix (K);

            --  Scan past leading zero bits of most significant digit.
            if K = numDigits - 1 then
               while Digit_2MSB (ci) = 0 loop
                  ci := ci * MQuadByte (4);  -- shift left 2 bits
                  ciBits := ciBits - 2;
               end loop;
            end if;


            while J < ciBits loop
               --  Compute t = t^4 * b^s mod d, where s = two MSB's of ci.

               NN_ModMult (Isolation, A, A, D);
               Isolation.CopyTo (A);

               NN_ModMult (Isolation, A, A, D);
               Isolation.CopyTo (A);

               S := Digit_2MSB (ci);
               if S /= 0 then
                  NN_ModMult (Isolation, A, BPower (Integer (S) - 1), D);
                  Isolation.CopyTo (A);
               end if;

               J := J + 2;
               ci := ci * MQuadByte (4);  -- shift left 2 bits
            end loop;

            exit Outer_Loop when K = 0;
            K := K - 1;
         end loop Outer_Loop;


   end NN_ModExp;



end Key_4096;