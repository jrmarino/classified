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

   procedure NN_LShift (Result    : out QuadByteMatrix.TData;
                        LHS       : in  QuadByteMatrix.TData;
                        numBits   : in  TDigit;
                        carry     : out MQuadByte);
   --  This computes Result := LHS * 2^numBits, and returns the carry.
   --  In other words, it shifts LHS left by "numBits" bits



   procedure NN_RShift (Result    : out QuadByteMatrix.TData;
                        LHS       : in  QuadByteMatrix.TData;
                        numBits   : in  TDigit;
                        carry     : out MQuadByte);
   --  This computes Result := LHS / 2^bits, and returns carry
   --  In other words, if shifts LHS right by "numBits" bits



   procedure NN_Sub (Result    : out QuadByteMatrix.TData;
                     A_Index   : in  QuadByteDigitIndex;
                     B         : in  QuadByteMatrix.TData;
                     B_Index   : in  QuadByteDigitIndex;
                     C         : in  QuadByteMatrix.TData;
                     C_Index   : in  QuadByteDigitIndex;
                     numDigits : in  QuadByteMatrixLen;
                     borrow    : out MQuadByte);
   --  This computes A := B - C, returns borrow and modifies A (Quad)


   procedure NN_Add (Result    : out QuadByteMatrix.TData;
                     LHS       : in  QuadByteMatrix.TData;
                     RHS       : in  QuadByteMatrix.TData;
                     numDigits : in  QuadByteMatrixLen;
                     carry     : out MQuadByte);
   --  This computes A := B + C, returns carry and modifies A



   function NN_ModMult (LHS    : QuadByteMatrix.TData;
                        RHS    : QuadByteMatrix.TData;
                        Modulo : QuadByteMatrix.TData)
   return QuadByteMatrix.TData;
   --  This computes result = (LHS * RHS) mod Modulo



   function NN_Mult (LHS : QuadByteMatrix.TData;
                     RHS : QuadByteMatrix.TData)
   return QuadByteMatrix.TData;
   --  This computes result = LHS * RHS



   procedure NN_Div (ResDiv : out QuadByteMatrix.TData;
                     ResMod : out QuadByteMatrix.TData;
                     C      : in  QuadByteMatrix.TData;
                     D      : in  QuadByteMatrix.TData);
   --  This computes the modulus and dividend of C divided by D
   --  e.g. dividend = int (C/D) and modulus = C mod D


   function NN_ModExp (LHS    : QuadByteMatrix.TData;
                       RHS    : QuadByteMatrix.TData;
                       Modulo : QuadByteMatrix.TData)
   return QuadByteMatrix.TData;
   --  This computes result = (LHS ^ RHS) mod Modulo
   --  Assumes Modulo > 0


   function NN_Mod (Dividend : QuadByteMatrix.TData;
                    Divisor  : QuadByteMatrix.TData)
   return QuadByteMatrix.TData;
   --  Computes result = Dividend mod Divisor


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
                       numDigits  : Natural) return TBinaryString;
   --  Encodes an array of 32-integers back into a hexidecimal string
   --  represented by an array of bytes.  The result is 4 times longer than
   --  the input.


   function NN_Random_Number return QuadByteMatrix.TData;
   --  Returns a random number ranging from 1 to 2^32 -1 (4.3 E9)


   function NN_Blind (Real_Matrix   : QuadByteMatrix.TData;
                      Random_Number : QuadByteMatrix.TData;
                      pub_exponent  : QuadByteMatrix.TData;
                      pub_modulus   : QuadByteMatrix.TData)
   return QuadByteMatrix.TData;
   --  This function multiplies a large number by a random number raised to
   --  the power of the public key exponent, and returns the modulus of that
   --  product after dividing by the public key modulus.
   --  result = random^pub_exponent mod pub_modulus
   --  It is the first have of the RSA blinding technique to thwart timing
   --  attackings during the decryption stage that use CRT.


   function NN_Unblind (Blinded_Matrix : QuadByteMatrix.TData;
                        Random_Number  : QuadByteMatrix.TData;
                        pub_modulus    : QuadByteMatrix.TData)
   return QuadByteMatrix.TData;
   --  This function takes the result of the decryption, which is blinded,
   --  and multiplies it by the modular multiplicative inverse of the
   --  random number used to the blind it at the start of the decryption.
   --  The result is the true plain text.


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


   function NN_ModInv (Value   : QuadByteMatrix.TData;
                       Modulus : QuadByteMatrix.TData)
   return QuadByteMatrix.TData;
   --  This calculates the modular multiplicative inverse of value
   --  with respect to Modulus


   function NN_GCD_Is_1 (Modulus   : QuadByteMatrix.TData;
                         Candidate : QuadByteMatrix.TData) return Boolean;
   --  If the Euclidian algorithm calculuates the Greated Common Denominator
   --  to be 1, this function returns True.


end NN;