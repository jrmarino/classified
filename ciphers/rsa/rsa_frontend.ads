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

   type BuildKeyError is range 0 .. 5;
   type PrivateKeyError is range 0 .. 8;

   type TPublicKey is record
      KeySize   : TKeySize;
      MsgSize   : QuadByteMatrixLen;
      ErrorCode : BuildKeyError;
      Modulus   : QuadByteMatrix.TData;
      Exponent  : QuadByteMatrix.TData;
   end record;


   type TPrivateKey is record
      KeySize          : TKeySize;
      MsgSize          : QuadByteMatrixLen;
      ErrorCode        : PrivateKeyError;
      Modulus          : QuadByteMatrix.TData;   --  public modulus
      Public_Exponent  : QuadByteMatrix.TData;   --  public exponent
      Private_Exponent : QuadByteMatrix.TData;   --  d
      Prime_p          : QuadByteMatrix.TData;   --  prime p
      Prime_q          : QuadByteMatrix.TData;   --  prime q
      Prime_Exp_p      : QuadByteMatrix.TData;   --  prime exponent CRTp
      Prime_Exp_q      : QuadByteMatrix.TData;   --  prime exponent CRTq
      coefficient      : QuadByteMatrix.TData;   --  prime coefficient CRTc
   end record;


   function Build_Public_Key (Modulus  : String;
                              Exponent : String) return TPublicKey;
   --  This function will convert radix encoded strings into an array of bytes
   --  and then convert those into arrays of double-words (reverse order).
   --  The Public Key structure is then returned.


   function Build_Private_Key (Modulus          : String;
                               Public_Exponent  : String;
                               Private_Exponent : String;
                               Prime_p          : String;
                               Prime_q          : String;
                               Prime_Exp_p      : String;
                               Prime_Exp_q      : String;
                               coefficient      : String) return TPrivateKey;
   --  This function will convert radix encoded strings into an array of bytes
   --  and then convert those into arrays of double-words (reverse order).
   --  The Private Key structure is then returned.


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
   --  This function returns a plain text string after processing a radix64
   --  encoded, encrypted message.  The message is expected to be encoded by
   --  the private key complement to the provided public key.  Failure to
   --  decrypt will result in the text "ERROR!" and the error code can be
   --  checked with the cryption_status function.


   function Encrypt_With_Private_Key (Private_Key   : TPrivateKey;
                                      Plain_Text    : String) return String;
   --  This function returns a radix64 encoded message, which is the encrypted
   --  version of the plain text message provided along with the private key
   --  that is used to encrypt it.  Failure to encrypt will result in the
   --  plain text message "ERROR!" and the error code can be chcked with the
   --  cryption status function.


   procedure Encrypt_With_Public_Key (Public_Key    : in  TPublicKey;
                                      Scrambled_R64 : out String;
                                      Plain_Text    : in  String;
                                      Status        : out TCryptoError);


   function Get_Status_Message (Status : TCryptoError) return String;
   --  Returns a description of an error when provided the error code.


private

   function Public_Transformation (Public_Key : TPublicKey;
                                   Bytecode   : TBinaryString)
   return TBinaryString;
   --  Raw public key operation.  Returned Bytecode has same length of modulus.


   function Private_Transformation (Private_Key : TPrivateKey;
                                    Bytecode    : TBinaryString)
   return TBinaryString;
   --  Raw public key operation.  Returned Bytecode has same length of modulus.



end RSA_Frontend;