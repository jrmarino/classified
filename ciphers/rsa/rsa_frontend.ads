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

   type TErrorCode is range 0 .. 11;

   type TPublicKey is record
      KeySize  : TKeySize;
      Modulus  : LongKeyString;
      Exponent : LongKeyString;
   end record;


   type TPrivateKey is record
      KeySize  : TKeySize;
      Modulus  : LongKeyString;   --  public modulus
      Exponent : LongKeyString;   --  public exponent
      d        : LongKeyString;   --  private exponent
      p        : HalfKeyString;   --  prime p
      q        : HalfKeyString;   --  prime q
      CRTp     : HalfKeyString;   --  prime exponent p
      CRTq     : HalfKeyString;   --  prime exponent q
      CRTc     : HalfKeyString;   --  prime coefficient
   end record;


   procedure Decrypt_With_Private_Key (Private_Key    : in  TPrivateKey;
                                       Scrambled_Text : in  String;
                                       Plain_text     : out String;
                                       Status         : out TErrorCode);


   procedure Decrypt_With_Public_Key (Public_Key     : in  TPublicKey;
                                      Scrambled_Text : in  String;
                                      Plain_text     : out String;
                                      Status         : out TErrorCode);


   procedure Encrypt_With_Private_Key (Private_Key    : in  TPrivateKey;
                                       Scrambled_Text : out String;
                                       Plain_text     : in  String;
                                       Status         : out TErrorCode);


   procedure Encrypt_With_Public_Key (Public_Key     : in  TPublicKey;
                                      Scrambled_Text : out String;
                                      Plain_text     : in  String;
                                      Status         : out TErrorCode);


   function Get_Status_Message (Status : TErrorCode) return String;

private


   procedure Decrypt_to_PKCS (Public_Key         : in  TPublicKey;
                              encrypted_bytecode : in  ModExp_Matrix.TData;
                              ErrorCode          : out TErrorCode;
                              pkcs_block         : out ModExp_Matrix.TData);


   function NN_Decode (HexString : ModExp_Matrix.TData)
   return QuadByteMatrix.TData;
   --  Encodes a hexidecimal string (represented by array of bytes) into an
   --  array of 32-bit integers, but reverses the order such that the bits are
   --  arranges from Most Significant to Least Significant.
   --
   --  It pads the array with zeros for the elements of (hexstring length -
   --  NN_DIGIT_LEN * MAX_NN_DIGITS).  If the hexstring is too loog, it will
   --  truncate the most significant bytes.


end RSA_Frontend;