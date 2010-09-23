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


with Key_4096; use Key_4096;
with RSATypes; use RSATypes;

package RSA_Frontend is

   type TPublicKey is record
      KeySize    : TKeySize;
      Modulus    : QuadByteMatrix.TData;
      Exponent   : QuadByteMatrix.TData;
      NumDigits  : QuadByteMatrixLen;
      ErrorCode  : BuildKeyError;
   end record;

   type TPrivateKey is record
      KeySize          : TKeySize;
      Modulus          : LongKeyString;   --  public modulus
      Public_Exponent  : LongKeyString;   --  public exponent
      Private_Exponent : LongKeyString;   --  d
      Prime_p          : HalfKeyString;   --  prime p
      Prime_q          : HalfKeyString;   --  prime q
      Prime_Exp_p      : HalfKeyString;   --  prime exponent CRTp
      Prime_Exp_q      : HalfKeyString;   --  prime exponent CRTq
      coefficient      : HalfKeyString;   --  prime coefficient CRTc
   end record;


   function Build_Public_Key (Modulus  : String;
                              Exponent : String) return TPublicKey;
   --  This function will convert a radix encoded string into an array of bytes
   --  and then convert that into an array of double-words (reverse order).
   --  The Public Key structure is then returned.


   function Cryption_Status return TCryptoError;
   --  This returns the error status of the previous encryption or decryption
   --  procedures (The four functions below).  Due to the indeterminate
   --  length of the return string, the status could not be returned as an
   --  "out" parameter of a procedure as hoped.


   procedure Decrypt_With_Private_Key (Private_Key   : in  TPrivateKey;
                                       Scrambled_R64 : in  String;
                                       Plain_Text    : out String;
                                       Status        : out TCryptoError);


   function Decrypt_With_Public_Key (Public_Key    : TPublicKey;
                                     Scrambled_R64 : String) return String;


   procedure Encrypt_With_Private_Key (Private_Key   : in  TPrivateKey;
                                       Scrambled_R64 : out String;
                                       Plain_Text    : in  String;
                                       Status        : out TCryptoError);


   procedure Encrypt_With_Public_Key (Public_Key    : in  TPublicKey;
                                      Scrambled_R64 : out String;
                                      Plain_Text    : in  String;
                                      Status        : out TCryptoError);


   function Get_Status_Message (Status : TCryptoError) return String;
   --  Returns a description of an error when provided the error code.


private

   function Decrypt_to_PKCS (Public_Key         : TPublicKey;
                             Encrypted_Bytecode : TBinaryString)
   return TBinaryString;
   --  Raw public key operation.  Revealed_Bytecode has same length of modulus.





end RSA_Frontend;