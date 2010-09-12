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


   ------------------------------
   --  NN_Sub (Extended Quad)  --
   ------------------------------

   procedure NN_Sub (A      : out ExtendedQuadByteMatrix.TData;
                     B      : in  ExtendedQuadByteMatrix.TData;
                     C      : in  ExtendedQuadByteMatrix.TData;
                     index  : in  ExtQuadByteDigitIndex;
                     matlen : in  ExtQuadByteMatrixLen;
                     borrow : out MQuadByte)
   is
      C_Index   : ExtQuadByteDigitIndex := 0;
      AB_Index  : ExtQuadByteDigitIndex := index;
      numDigits : ExtQuadByteMatrixLen  := matlen;
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
            C_Index := C_Index + 1;
            A.Matrix (AB_Index) := temp;
            AB_Index := AB_Index + 1;
            numDigits := numDigits - 1;
            exit Repeat_Until when numDigits = 0;
         end loop Repeat_Until;
         A.CurrentLen := A.Significant_Length;
   end NN_Sub;



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
            C_Index := C_Index + 1;
            A.Matrix (AB_Index) := temp;
            AB_Index := AB_Index + 1;
            numDigits := numDigits - 1;
            exit Repeat_Until when numDigits = 0;
         end loop Repeat_Until;
         A.CurrentLen := A.Significant_Length;
   end NN_Sub;


end Key_4096;