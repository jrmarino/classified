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


With Interfaces; Use Interfaces;

package body Generic_Matrix is

   Matrix     : TMatrix;
   CurrentLen : TMatrixLen;

   ------------------
   --  Zero_Array  --
   ------------------

   procedure Zero_Array
   is
      zero : constant MatrixType := MatrixType (0);
   begin
      for index in DigitIndex loop
         Matrix (index) := zero;
      end loop;
   end Zero_Array;



   -------------------------
   --  Assign_Zero_Digit  --
   -------------------------

   procedure Assign_Zero_Digit (Value : in MatrixType)
   is
   begin
      Zero_Array;
      Matrix (0) := Value;
      CurrentLen := 1;
   end Assign_Zero_Digit;



   --------------
   --  CopyTo  --
   --------------

   procedure CopyTo (destination : in out TMatrix)
   is
   begin
      destination := Matrix;
   end CopyTo;



   --------------------------
   --  Significant_Length  --
   --------------------------

   function Significant_Length
   return Positive is
      numDigits : TMatrixLen := CurrentLen;
      i         : DigitIndex := DigitIndex (numDigits - 1);
   begin
      while (numDigits > 0) and then Matrix (i) = 0 loop
         numDigits := numDigits - 1;
         i := i - 1;
      end loop;
      if numDigits = 0 then
         return 1;
      else
         return Positive (numDigits);
      end if;
   end Significant_Length;



   ------------------------
   --  Significant_Bits  --
   ------------------------

   function Significant_Bits (index : DigitIndex)
   return TDigit is
      i     : TDigit     := 0;
      a     : Unsigned_64 := Unsigned_64 (Matrix (index));
      found : Boolean    := False;
   begin
      while not found and (i <= TDigit'Last) loop
         if a = 0 then
            found := True;
         else
            i := i + 1;
            a := Shift_Right (a, 1);
         end if;
      end loop;
      return i;
   end Significant_Bits;



   ---------------------
   --  Compared_With  --
   ---------------------

   function Compared_With (index     : DigitIndex;
                           ExtMatrix : TMatrix;
                           ExtDigits : TMatrixLen)
   return TCompare is
      result       : TCompare := 0;
      digitCounter : TMatrixLen := ExtDigits;
      newIndex     : DigitIndex;
      dcIndex      : DigitIndex;
   begin
      if digitCounter > 0 then
         Repeat_Until:
            loop
               digitCounter := digitCounter - 1;
               dcIndex      := DigitIndex (digitCounter);
               newIndex     := dcIndex + index;
               if Matrix (newIndex) > ExtMatrix (dcIndex) then
                  result := 1;
               elsif Matrix (newIndex) < ExtMatrix (dcIndex) then
                  result := -1;
               end if;
               exit Repeat_Until when (result /= 0) or (digitCounter = 0);
            end loop Repeat_Until;
      end if;
      return result;
   end Compared_With;



end Generic_Matrix;