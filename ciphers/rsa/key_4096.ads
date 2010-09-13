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
   --  Used for all the private key components
   --  Sized for 4096-bit key, but will accommodate smaller down to 1024-bit



   type ModExpMsgMatrixLen  is range 0 .. MsgBytes;
   type ModExpMsgDigitIndex is range 0 .. MsgBytes - 1;

   package ModExp_Matrix is
      new Generic_Matrix (
         MatrixType => MByte,
         TMatrixLen => ModExpMsgMatrixLen,
         DigitIndex => ModExpMsgDigitIndex
      );
   --  Used for encoding and decoding the messages, and public key components
   --  Sized for 4096-bit key, but will accommodate smaller down to 1024-bit



   type QuadByteMatrixLen  is range 0 .. NN_Digits;
   type QuadByteDigitIndex is range 0 .. NN_Digits - 1;

   package QuadByteMatrix is
      new Generic_Matrix (
         MatrixType => MQuadByte,
         TMatrixLen => QuadByteMatrixLen,
         DigitIndex => QuadByteDigitIndex
      );
   --  Used for intermediate calculations
   --  Sized for 4096-bit key, but will accommodate smaller down to 1024-bit



   type ExtQuadByteMatrixLen  is range 0 .. NN_Dig2X + 1;
   type ExtQuadByteDigitIndex is range 0 .. NN_Dig2X;

   package ExtendedQuadByteMatrix is
      new Generic_Matrix (
         MatrixType => MQuadByte,
         TMatrixLen => ExtQuadByteMatrixLen,
         DigitIndex => ExtQuadByteDigitIndex
      );
   --  Used for multiplying the intermediately calculations together
   --  Sized for 4096-bit key, but will accommodate smaller down to 1024-bit


   procedure NN_Sub (A      : out ExtendedQuadByteMatrix.TData;
                     B      : in  ExtendedQuadByteMatrix.TData;
                     C      : in  ExtendedQuadByteMatrix.TData;
                     index  : in  ExtQuadByteDigitIndex;
                     matlen : in  ExtQuadByteMatrixLen;
                     borrow : out MQuadByte);
   --  This computes A := B - C, returns borrow and modifies A (Extended Quad)


   procedure NN_Sub (A      : out QuadByteMatrix.TData;
                     B      : in  QuadByteMatrix.TData;
                     C      : in  QuadByteMatrix.TData;
                     index  : in  QuadByteDigitIndex;
                     matlen : in  QuadByteMatrixLen;
                     borrow : out MQuadByte);
   --  This computes A := B - C, returns borrow and modifies A (Quad)


   procedure NN_Add (A      : out ExtendedQuadByteMatrix.TData;
                     B      : in  ExtendedQuadByteMatrix.TData;
                     C      : in  QuadByteMatrix.TData;
                     index  : in  ExtQuadByteDigitIndex;
                     matlen : in  QuadByteMatrixLen;
                     carry  : out MQuadByte);
   --  This computes A := B + C, returns carry and modifies A
   --  It only has one application where B = A, so A := A + C




   --  procedure NN_ModExp (

   procedure NN_ModMult (A : out QuadByteMatrix.TData;
                         B : in  QuadByteMatrix.TData;
                         C : in  QuadByteMatrix.TData;
                         D : in  QuadByteMatrix.TData);
   --  This computes A = (b * C) mod D


   procedure NN_ModMult (A : out QuadByteMatrix.TData;
                         B : in  ExtendedQuadByteMatrix.TData;
                         C : in  QuadByteMatrix.TData;
                         D : in  QuadByteMatrix.TData);
   --  This computes A = (b * C) mod D


   procedure NN_Mult (A : out ExtendedQuadByteMatrix.TData;
                      B : in  QuadByteMatrix.TData;
                      C : in  QuadByteMatrix.TData);
   --  This computes A = B * C


   procedure NN_Mult (A : out ExtendedQuadByteMatrix.TData;
                      B : in  ExtendedQuadByteMatrix.TData;
                      C : in  QuadByteMatrix.TData);
   --  This computes A = B * C


   procedure NN_Divide (ResDiv : out QuadByteMatrix.TData;
                        ResMod : out QuadByteMatrix.TData;
                        C      : in  ExtendedQuadByteMatrix.TData;
                        D      : in  QuadByteMatrix.TData);
   --  This computes the modulus and dividend of C divided by D
   --  e.g. dividend = int (C/D) and modulus = C mod D


   procedure Sub_Digit_Mult (A      : out ExtendedQuadByteMatrix.TData;
                             AIndex : in  ExtQuadByteDigitIndex;
                             C      : in  MQuadByte;
                             D      : in  ExtendedQuadByteMatrix.TData;
                             borrow : out MQuadByte);
   --  Still not sure what this does


   procedure NN_RShift_Mixed (A     : out ExtendedQuadByteMatrix.TData;
                              B     : in  QuadByteMatrix.TData;
                              bits  : in  TDigit;
                              carry : out MQuadByte);
   --  This computes A := B / 2^bits, returns carry and modifies A
   --  Normally this is an object method, but we need to mix types.


end Key_4096;
