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

package RSA_Utilities is

   function Low_Half (original : MQuadByte) return MDualByte;
   --  Returns the lowest 16 bits (Least Significant Word) for a given
   --  32-bit word.


   function High_Half (original : MQuadByte) return MDualByte;
   --  Returns the highest 16 bits (Most Significant Word) for a given
   --  32-bit word.


   function Shift_To_High_Half (original : MDualByte) return MQuadByte;
   --  Takes a 16-bit word and shifts it left 16 bits into a 32-bit word.
   --  The original word becomes the MSW of a 32-bit result.


   function Digit_2MSB (original : MQuadByte) return MQuadByte;
   --  Takes a 32-bit word, shifts it 30 bits to the right, and returns
   --  the most significant 2 bits.


   function Flowguard_Add  (LHS : MQuadByte;
                            RHS : MQuadByte) return MQuadByte;
   --  Adds two 32-bit words, and returns the result in another 32-bit word.
   --  Any overflow is stripped silently.  In fact, this is done inherently
   --  by the modularly type, so no special handling is necessary.


   function Flowguard_Sub  (LHS : MQuadByte;
                            RHS : MQuadByte) return MQuadByte;
   --  Subtracts one 32-bit word from another.  If the Right-Hand-Side word is
   --  greater than the Left-Hand-Side, the negative answer is subtracted from
   --  MQuadByte'Last.  This is inherent property of modular type.


   function Flowguard_Mult (LHS : MDualByte;
                            RHS : MDualByte) return MQuadByte;
   --  Multiplies two 16-bit words and returns a 32-bit word.  This will never
   --  overflow.


   procedure DMult (LHS        : in  MQuadByte;
                    RHS        : in  MQuadByte;
                    ResultHigh : out MQuadByte;
                    ResultLow  : out MQuadByte);
   --  Multiples two 32-bit words and returns a 64-bit word, but separately, in
   --  a high and low 32-bit word.


end RSA_Utilities;
