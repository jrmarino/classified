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
with Key_4096; use Key_4096;

package NN is

   procedure NN_LShift (A         : out QuadByteMatrix.TData;
                        A_Index   : in  QuadByteDigitIndex;
                        B         : in  QuadByteMatrix.TData;
                        B_Index   : in  QuadByteDigitIndex;
                        numBits   : in  TDigit;
                        numDigits : in  QuadByteMatrixLen;
                        carry     : out MQuadByte);
   --  This computes A := B * 2^numBits, and returns the carry.
   --  In other words, it shifts B left by "numBits" bits and returns result
   --  as A.



   procedure NN_RShift (A         : out QuadByteMatrix.TData;
                        A_Index   : in  QuadByteDigitIndex;
                        B         : in  QuadByteMatrix.TData;
                        B_Index   : in  QuadByteDigitIndex;
                        numBits   : in  TDigit;
                        numDigits : in  QuadByteMatrixLen;
                        carry     : out MQuadByte);
   --  This computes A := B / 2^bits, returns carry and modifies A
   --  In other words, if shifts B right by "bits" bits and returns result
   --  as A.  A and B are the same lenght.



   procedure NN_Sub (A         : out QuadByteMatrix.TData;
                     A_Index   : in  QuadByteDigitIndex;
                     B         : in  QuadByteMatrix.TData;
                     B_Index   : in  QuadByteDigitIndex;
                     C         : in  QuadByteMatrix.TData;
                     C_Index   : in  QuadByteDigitIndex;
                     numDigits : in  QuadByteMatrixLen;
                     borrow    : out MQuadByte);
   --  This computes A := B - C, returns borrow and modifies A (Quad)


   procedure NN_Add (A         : out QuadByteMatrix.TData;
                     A_Index   : in  QuadByteDigitIndex;
                     B         : in  QuadByteMatrix.TData;
                     B_Index   : in  QuadByteDigitIndex;
                     C         : in  QuadByteMatrix.TData;
                     C_Index   : in  QuadByteDigitIndex;
                     numDigits : in  QuadByteMatrixLen;
                     carry     : out MQuadByte);
   --  This computes A := B + C, returns carry and modifies A



   procedure NN_ModMult (A         : out QuadByteMatrix.TData;
                         B         : in  QuadByteMatrix.TData;
                         B_Index   : in  QuadByteDigitIndex;
                         C         : in  QuadByteMatrix.TData;
                         C_Index   : in  QuadByteDigitIndex;
                         D         : in  QuadByteMatrix.TData;
                         D_Index   : in  QuadByteDigitIndex;
                         numDigits : in  QuadByteMatrixLen);
   --  This computes A = (b * C) mod D



   procedure NN_Mult (A       : out QuadByteMatrix.TData;
                      A_Index : in  QuadByteDigitIndex;
                      B       : in  QuadByteMatrix.TData;
                      B_Index : in  QuadByteDigitIndex;
                      C       : in  QuadByteMatrix.TData;
                      C_Index : in  QuadByteDigitIndex);
   --  This computes A = B * C



   procedure NN_Div (ResDiv   : out QuadByteMatrix.TData;
                     ResMod   : out QuadByteMatrix.TData;
                     C        : in  QuadByteMatrix.TData;
                     C_Index  : in  QuadByteDigitIndex;
                     C_Digits : in  QuadByteMatrixLen;
                     D        : in  QuadByteMatrix.TData;
                     D_Index  : in  QuadByteDigitIndex;
                     D_Digits : in  QuadByteMatrixLen);
   --  This computes the modulus and dividend of C divided by D
   --  e.g. dividend = int (C/D) and modulus = C mod D


   procedure NN_ModExp (A         : out QuadByteMatrix.TData;
                        B         : in  QuadByteMatrix.TData;
                        B_Index   : in  QuadByteDigitIndex;
                        C         : in  QuadByteMatrix.TData;
                        C_Index   : in  QuadByteDigitIndex;
                        C_Digits  : in  QuadByteMatrixLen;
                        D         : in  QuadByteMatrix.TData;
                        D_Index   : in  QuadByteDigitIndex;
                        D_Digits  : in  QuadByteMatrixLen);
   --  Computes a = b^c mod d.  assumes d > 0.


   function NN_Decode (HexString : TBinaryString)
   return QuadByteMatrix.TData;
   --  Encodes a hexidecimal string (represented by array of bytes) into an
   --  array of 32-bit integers, but reverses the order such that the bits are
   --  arranges from Most Significant to Least Significant.
   --
   --  It pads the array with zeros for the elements of (hexstring length -
   --  NN_DIGIT_LEN * MAX_NN_DIGITS).  If the hexstring is too loog, it will
   --  truncate the most significant bytes.


   function NN_Encode (HugeNumber : QuadByteMatrix.TData;
                       numDigits  : QuadByteMatrixLen) return TBinaryString;
   --  Encodes an array of 32-integers back into a hexidecimal string
   --  represented by an array of bytes.  The result is 4 times longer than
   --  the input.


private


   procedure Sub_Digit_Mult (A         : out QuadByteMatrix.TData;
                             A_Index   : in  QuadByteDigitIndex;
                             B         : in  QuadByteMatrix.TData;
                             B_Index   : in  QuadByteDigitIndex;
                             C         : in  MQuadByte;
                             D         : in  QuadByteMatrix.TData;
                             D_Index   : in  QuadByteDigitIndex;
                             numDigits : in  QuadByteMatrixLen;
                             borrow    : out MQuadByte);
   --  Still not sure what this does


end NN;