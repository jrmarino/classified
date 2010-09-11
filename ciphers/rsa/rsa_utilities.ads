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
   --  Returns the lowest 32 bits (Least Significant Word) for a given
   --  64-bit word.


   function High_Half (original : MQuadByte) return MDualByte;
   --  Returns the highest 32 bits (Most Significant Word) for a given
   --  64-bit word.


   function Shift_To_High_Half (original : MDualByte) return MQuadByte;
   --  Takes a 32-bit word and shifts it left to create a 64-bit word.
   --  The original word becomes the MSW of the result.


end RSA_Utilities;
