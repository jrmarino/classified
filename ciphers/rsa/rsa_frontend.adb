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


package body RSA_Frontend is



   --------------------------------
   --  Decrypt_With_Private_Key  --
   --------------------------------

   procedure Decrypt_With_Private_Key (Private_Key    : in  TPrivateKey;
                                       Scrambled_Text : in  String;
                                       Plain_text     : out String;
                                       Status         : out TErrorCode)
   is
   begin
      null;
   end Decrypt_With_Private_Key;



   -------------------------------
   --  Decrypt_With_Public_Key  --
   -------------------------------

   procedure Decrypt_With_Public_Key (Public_Key     : in  TPublicKey;
                                      Scrambled_Text : in  String;
                                      Plain_text     : out String;
                                      Status         : out TErrorCode)
   is
   begin
      null;
   end Decrypt_With_Public_Key;



   --------------------------------
   --  Encrypt_With_Private_Key  --
   --------------------------------

   procedure Encrypt_With_Private_Key (Private_Key    : in  TPrivateKey;
                                       Scrambled_Text : out String;
                                       Plain_text     : in  String;
                                       Status         : out TErrorCode)
   is
   begin
      null;
   end Encrypt_With_Private_Key;



   -------------------------------
   --  Encrypt_With_Public_Key  --
   -------------------------------

   procedure Encrypt_With_Public_Key (Public_Key     : in  TPublicKey;
                                      Scrambled_Text : out String;
                                      Plain_text     : in  String;
                                      Status         : out TErrorCode)
   is
   begin
      null;
   end Encrypt_With_Public_Key;



   -----------------------
   --  Decrypt_to_PKCS  --
   -----------------------

   procedure Decrypt_to_PKCS (Public_Key         : in  TPublicKey;
                              encrypted_bytecode : in  ModExp_Matrix.TData;
                              ErrorCode          : out TErrorCode;
                              pkcs_block         : out ModExp_Matrix.TData)
   is
      --  matrix_M : QuadByteMatrix.TData := QuadByteMatrix.Construct;
      --  matrix_E : QuadByteMatrix.TData := QuadByteMatrix.Construct;
      --  matrix_N : QuadByteMatrix.TData := QuadByteMatrix.Construct;
      --  matrix_C : QuadByteMatrix.TData := QuadByteMatrix.Construct;
   begin
      pkcs_block.Zero_Array;

   end Decrypt_to_PKCS;






end RSA_Frontend;