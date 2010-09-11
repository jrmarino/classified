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


package RSATypes is

   type MQuadByte is range 0 .. 16#FFFFFFFF#;

   NN_DIGIT_BITS      : constant Integer := 32;
   NN_DIGIT_BYTES     : constant Integer := NN_DIGIT_BITS / 8;
   NN_HALF_DIGIT_BITS : constant Integer := NN_DIGIT_BITS / 2;
   MAX_NN_DIGIT       : constant MQuadByte := MQuadByte'Last;
   MAX_NN_HALF_DIGIT  : constant MQuadByte := MAX_NN_DIGIT / 2;

   L_MODULUS_BITS     : constant Integer := 1024;
   U_MODULUS_BITS     : constant Integer := 4096;

   L_MODULUS_BYTES    : constant Integer := (L_MODULUS_BITS + 7) / 8;
   U_MODULUS_BYTES    : constant Integer := (U_MODULUS_BITS + 7) / 8;

   L_PRIME_BITS       : constant Integer := (L_MODULUS_BITS + 1) / 2;
   U_PRIME_BITS       : constant Integer := (U_MODULUS_BITS + 1) / 2;

   L_PRIME_BYTES      : constant Integer := (L_PRIME_BITS + 7) / 8;
   U_PRIME_BYTES      : constant Integer := (U_PRIME_BITS + 7) / 8;

   L_MAX_NN_DIGITS    : constant Integer := 1 + ((L_MODULUS_BYTES +
                        NN_DIGIT_BYTES - 1) / NN_DIGIT_BYTES);
   U_MAX_NN_DIGITS    : constant Integer := 1 + ((U_MODULUS_BYTES +
                        NN_DIGIT_BYTES - 1) / NN_DIGIT_BYTES);

   L_2XMAX_NN_DIGITS  : constant Integer := L_MAX_NN_DIGITS * 2;
   U_2XMAX_NN_DIGITS  : constant Integer := U_MAX_NN_DIGITS * 2;



   type TKeySize       is range L_MODULUS_BITS .. U_MODULUS_BITS;
   type TModulus       is range L_MODULUS_BYTES .. U_MODULUS_BYTES;
   type TPrimeBits     is range L_PRIME_BITS .. U_PRIME_BITS;
   type TPrimeBytes    is range L_PRIME_BYTES .. U_PRIME_BYTES;
   type TMAXNNDigits   is range L_MAX_NN_DIGITS .. U_MAX_NN_DIGITS;
   type T2XMAXNNDigits is range L_2XMAX_NN_DIGITS .. U_2XMAX_NN_DIGITS;

   type TDigit         is range 0 .. NN_DIGIT_BITS - 1;
   type TCompare       is range -1 .. 1;

end RSATypes;
