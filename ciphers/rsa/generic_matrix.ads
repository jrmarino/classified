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
   type MatrixType is mod <>;
   type TMatrixLen is range <>;
   type DigitIndex is range <>;
package Generic_Matrix is

   type TMatrix is array (DigitIndex) of MatrixType;

   type TData is tagged record
      Matrix     : TMatrix;
      CurrentLen : TMatrixLen;
   end record;

   procedure Zero_Array (data : out TData);
   --  This procedure initializes all elements of the TData.Matrix array to 0
   --  It does not change the existing value of TData.CurrentLen


   procedure Assign_Zero_Digit (data : out TData; Value : in MatrixType);
   --  This procedure initializes all the elements, sets the length of the data
   --  array to 1, and sets the first element's value to "value"


   procedure CopyTo (origin : in TData; destination : out TData);
   --  This procedure copies the entire TData structure from "origin" to
   --  "destination".


   function Significant_Length (data : TData) return TMatrixLen;
   --  This function starts at the last element and works its way backwards to
   --  discover a non-zero value.  Once it does, it returns the array index
   --  plus 1.  If all the elements have a value of zero, the result is 1.


   function Significant_Bits (data : TData; index : DigitIndex) return TDigit;
   --  Given the value of a TData array element with the index of "index", the
   --  value is shifted right (divides by 2) until the value is zero.  The
   --  number of shifts it took to do this is returned.  If the value is starts
   --  at zero, then zero is returned because no shifts were required.


   function Compared_With (Data      : TData;
                           Index     : DigitIndex;
                           ExtData   : TData;
                           ExtDigits : TMatrixLen) return TCompare;
   --  Returns the sign of Data - ExtData elements, offset by Index.
   --  If Data > Extdata, return 1
   --  If Data < Extdata, return -1
   --  If Data = Extdata, return 0


   function Construct return TData;
   --  Returns an initialized TData Type


end Generic_Matrix;

--   Matrix_Size  : Positive;
--   type TMatrixLen is range 1 .. Matrix_Size;
--   type DigitIndex is range 0 .. Matrix_Size - 1;
