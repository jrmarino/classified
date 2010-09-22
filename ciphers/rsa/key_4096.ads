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

package Key_4096 is

   KeySize_Bits : constant TKeySize := 4096;

   PrimeBytes : constant TPrimeBytes    := TPrimeBytes (KeySize_Bits / 16);
   ModLength  : constant Integer        := Integer (KeySize_Bits) / 8;
   MsgBytes   : constant TModulus       := TModulus (ModLength);
   NN_Digits  : constant TMAXNNDigits   := TMAXNNDigits
                                          (ModLength / NN_DIGIT_BYTES) + 1;
   NN_Dig2X   : constant T2XMAXNNDigits := T2XMAXNNDigits (NN_Digits * 2);

   subtype LongKeyString is String (1 .. ModLength);
   subtype HalfKeyString is String (1 .. ModLength / 2);


   --  The *MatrixLen ranges are supposed to start with 1
   --  However, for some reason the generics aren't liking it.

   type PrimeMatrixLen  is range 0 .. PrimeBytes;
   type PrimeDigitIndex is range 0 .. PrimeBytes - 1;
   --  Used for all the private key components
   --  Sized for 4096-bit key, but will accommodate smaller down to 1024-bit


   type ModExpMsgMatrixLen  is range 0 .. MsgBytes;
   type ModExpMsgDigitIndex is range 0 .. MsgBytes - 1;
   --  Used for encoding and decoding the messages, and public key components
   --  Sized for 4096-bit key, but will accommodate smaller down to 1024-bit


   type TBinaryString is array (Natural range <>) of MByte;
   --  subtype Prime_Matrix     is TBinaryString range 0 .. PrimeBytes - 1;
   --  subtype ModExpMsg_Matrix is TBinaryString range 0 .. MsgBytes - 1;


   type QuadByteMatrixLen  is range 0 .. NN_Dig2X + 1;
   type QuadByteDigitIndex is range 0 .. NN_Dig2X;

   package QuadByteMatrix is
      new Generic_Matrix (
         MatrixType => MQuadByte,
         TMatrixLen => QuadByteMatrixLen,
         DigitIndex => QuadByteDigitIndex
      );
   --  Used for intermediate calculations
   --  Sized for 4096-bit key, but will accommodate smaller down to 1024-bit


   type BuildKeyError is range 0 .. 5;

end Key_4096;
