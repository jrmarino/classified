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


package body RSA_Utilities is


   ----------------
   --  Low_Half  --
   ----------------

   function Low_Half (original : MQuadByte)
   return MDualByte is
      work   : MQuadByte;
   begin
      work   := original and MAX_NN_HALF_DIGIT;
      return MDualByte (work);
   end Low_Half;



   -----------------
   --  High_Half  --
   -----------------

   function High_Half (original : MQuadByte)
   return MDualByte is
      work   : MQuadByte;
   begin
      --  This is equivalent to shift_right 16 bits
      work := original / 2 ** NN_HALF_DIGIT_BITS;
      return MDualByte (work);
   end High_Half;



   --------------------------
   --  Shift_To_High_Half  --
   --------------------------

   function Shift_To_High_Half (original : MDualByte)
   return MQuadByte is
      work   : MQuadByte := MQuadByte (original);
   begin
      --  This is equivalent to shift left 16 bits
      work := work * 2 ** NN_HALF_DIGIT_BITS;
      return work;
   end Shift_To_High_Half;



   ------------------
   --  Digit_2MSB  --
   ------------------

   function Digit_2MSB (original : MQuadByte)
   return MQuadByte is
      work : MQuadByte := original;
   begin
      work := work / 2 ** (NN_DIGIT_BITS - 2);
      return work;
   end Digit_2MSB;



   ---------------------
   --  Flowguard_Add  --
   ---------------------

   function Flowguard_Add  (LHS : MQuadByte;
                            RHS : MQuadByte)
   return MQuadByte is
   begin
      return LHS + RHS;
   end Flowguard_Add;



   ---------------------
   --  Flowguard_Sub  --
   ---------------------

   function Flowguard_Sub  (LHS : MQuadByte;
                            RHS : MQuadByte)
   return MQuadByte is
   begin
      return LHS - RHS;
   end Flowguard_Sub;



   ----------------------
   --  Flowguard_Mult  --
   ----------------------

   function Flowguard_Mult (LHS : MDualByte;
                            RHS : MDualByte)
   return MQuadByte is
      work : MQuadByte := MQuadByte (LHS);
   begin
      work := work * MQuadByte (RHS);
      return work;
   end Flowguard_Mult;



   -------------
   --  DMult  --
   -------------

   procedure DMult (LHS        : in  MQuadByte;
                    RHS        : in  MQuadByte;
                    ResultHigh : out MQuadByte;
                    ResultLow  : out MQuadByte)
   is
      carry : MQuadByte := 0;
      al    : constant MDualByte := Low_Half  (LHS);
      ah    : constant MDualByte := High_Half (LHS);
      bl    : constant MDualByte := Low_Half  (RHS);
      bh    : constant MDualByte := High_Half (RHS);
      m     : MQuadByte;
      m1    : MQuadByte;
      m2    : MQuadByte;
      ml    : MQuadByte;
      mh    : MQuadByte;
   begin
      ResultLow  := Flowguard_Mult (al, bl);
      ResultHigh := Flowguard_Mult (ah, bh);

      m1         := Flowguard_Mult (al, bh);
      m2         := Flowguard_Mult (ah, bl);
      m          := Flowguard_Add  (m1, m2);

      if m < m1 then
         --  This is equivalent to 1 shift left 16
         carry := 1 * 2 ** NN_HALF_DIGIT_BITS;
      end if;
      ml := Shift_To_High_Half (MDualByte (m and MAX_NN_HALF_DIGIT));
      mh := MQuadByte (High_Half (m));

      ResultLow := Flowguard_Add (ResultLow, ml);
      if ResultLow < ml then
         carry := carry + 1;
      end if;

      ResultHigh := Flowguard_Add (ResultHigh, mh);
      ResultHigh := Flowguard_Add (ResultHigh, carry);

   end DMult;



end RSA_Utilities;
