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


package body Generic_Matrix is

   MatZero    : constant MatrixType := MatrixType'First;



   -----------------
   --  Construct  --
   -----------------

   function Construct
   return TData is
      result : TData;
   begin
      result := (
         Matrix     => (others => MatZero),
         CurrentLen => 0
      );
      return result;
   end Construct;



   ------------------
   --  Zero_Array  --
   ------------------

   procedure Zero_Array (data : out TData)
   is
   begin
      data.Matrix     := (others => MatZero);
      data.CurrentLen := 0;
   end Zero_Array;



   -------------------------
   --  Assign_Zero_Digit  --
   -------------------------

   procedure Assign_Zero_Digit (data : out TData; Value : in MatrixType)
   is
   begin
      Zero_Array (data => data);
      data.Matrix (0) := Value;
      data.CurrentLen := 1;
   end Assign_Zero_Digit;



   --------------
   --  CopyTo  --
   --------------

   procedure CopyTo (origin : in TData; destination : out TData)
   is
   begin
      destination := origin;
   end CopyTo;



   --------------------------
   --  Significant_Length  --
   --------------------------

   function Significant_Length (data : TData)
   return TMatrixLen is
      numDigits : TMatrixLen := data.Matrix'Length;
      n         : DigitIndex;
   begin
      n := DigitIndex (numDigits - 1);
      Jessica :
         loop
            exit Jessica when data.Matrix (n) /= 0;
            numDigits := numDigits - 1;
            exit Jessica when numDigits = 0;
            n         := n - 1;
         end loop Jessica;

      if numDigits = 0 then
         return 1;
      else
         return numDigits;
      end if;
   end Significant_Length;



   ------------------------
   --  Significant_Bits  --
   ------------------------

   function Significant_Bits (data : TData; index : DigitIndex)
   return Natural is
      z     : Natural    := 0;
      a     : MatrixType := data.Matrix (index);
   begin
      Justine :
         loop
            exit Justine when a = 0;
            a := a / 2;
            z := z + 1;
            exit Justine when z = NN_DIGIT_BITS;
         end loop Justine;
      return z;
   end Significant_Bits;



   ---------------------
   --  Compared_With  --
   ---------------------

   function Compared_With (Data      : TData;
                           Index     : DigitIndex;
                           ExtData   : TData;
                           ExtDigits : TMatrixLen)
   return TCompare is
      result       : TCompare := 0;
      digitCounter : TMatrixLen := ExtDigits;
      newIndex     : DigitIndex;
      dcIndex      : DigitIndex;
   begin
      if digitCounter > 0 then
         Repeat_Until :
            loop
               digitCounter := digitCounter - 1;
               dcIndex      := DigitIndex (digitCounter);
               newIndex     := dcIndex + Index;
               if Data.Matrix (newIndex) > ExtData.Matrix (dcIndex) then
                  result := 1;
               elsif Data.Matrix (newIndex) < ExtData.Matrix (dcIndex) then
                  result := -1;
               end if;
               exit Repeat_Until when (result /= 0) or (digitCounter = 0);
            end loop Repeat_Until;
      end if;
      return result;
   end Compared_With;


end Generic_Matrix;
