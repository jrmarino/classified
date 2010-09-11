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


with RSATypes; use RSATypes;

generic
   type MatrixType is range <>;
   type TMatrixLen is range <>;
   type DigitIndex is range <>;
package Generic_Matrix is
   type TMatrix is array (DigitIndex) of MatrixType;
   procedure Zero_Array;
   procedure Assign_Zero_Digit (Value : in MatrixType);
   procedure CopyTo (destination : in out TMatrix);
   function Significant_Length return Positive;
   function Significant_Bits (index : DigitIndex) return TDigit;
   function Compared_With (index     : DigitIndex;
                           ExtMatrix : TMatrix;
                           ExtDigits : TMatrixLen) return TCompare;
end Generic_Matrix;

--   KeySize_Bits : TKeySize;
--   Matrix_Size  : Positive;
--   type TMatrixLen is range 1 .. Matrix_Size;
--   type DigitIndex is range 0 .. Matrix_Size - 1;