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


with RSATypes;       use RSATypes;
with Generic_Matrix;

package Key_2048 is

   KeySize_Bits : constant TKeySize := 2048;

   PrimeBytes : constant TPrimeBytes := TPrimeBytes ((KeySize_Bits + 15) / 16);
   ModLength  : constant Integer := (Integer (KeySize_Bits) + 7) / 8;
   MsgBytes   : constant TModulus := TModulus (ModLength);
   NN_Digits  : constant TMAXNNDigits := TMAXNNDigits (
                ((ModLength + NN_DIGIT_BYTES - 1) / NN_DIGIT_BYTES) + 1);
   NN_Dig2X   : constant T2XMAXNNDigits := T2XMAXNNDigits (NN_Digits * 2);

   --  The *MatrixLen ranges are supposed to start with 1
   --  However, for some reason the generics aren't liking it.

   type PrimeMatrixLen  is range 0 .. PrimeBytes;
   type PrimeDigitIndex is range 0 .. PrimeBytes - 1;

   package Prime_Matrix is
      new Generic_Matrix (
         MatrixType => MByte,
         TMatrixLen => PrimeMatrixLen,
         DigitIndex => PrimeDigitIndex
      );

   type ModExpMsgMatrixLen  is range 0 .. MsgBytes;
   type ModExpMsgDigitIndex is range 0 .. MsgBytes - 1;

   package ModExp_Matrix is
      new Generic_Matrix (
         MatrixType => MByte,
         TMatrixLen => ModExpMsgMatrixLen,
         DigitIndex => ModExpMsgDigitIndex
      );

   type QuadByteMatrixLen  is range 0 .. NN_Digits;
   type QuadByteDigitIndex is range 0 .. NN_Digits - 1;

   package QuadByteMatrix is
      new Generic_Matrix (
         MatrixType => MQuadByte,
         TMatrixLen => QuadByteMatrixLen,
         DigitIndex => QuadByteDigitIndex
      );

   type ExtQuadByteMatrixLen  is range 0 .. NN_Dig2X + 1;
   type ExtQuadByteDigitIndex is range 0 .. NN_Dig2X;

   package ExtendedQuadByteMatrix is
      new Generic_Matrix (
         MatrixType => MQuadByte,
         TMatrixLen => ExtQuadByteMatrixLen,
         DigitIndex => ExtQuadByteDigitIndex
      );

end Key_2048;
