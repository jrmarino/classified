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


package body OpenPGP_Utilities is


   ----------------------
   --  MPI_Total_Bits  --
   ----------------------

   function MPI_Total_Bits (Octet_1 : TOctet;
                            Octet_2 : TOctet)
   return Natural is
      NOct1 : constant Natural := Natural (Octet_1) * 256;
      NOct2 : constant Natural := Natural (Octet_2);
   begin
      return NOct1 + NOct2;
   end MPI_Total_Bits;



   ---------------------
   --  MPI_Byte_Size  --
   ---------------------

   function MPI_Byte_Size (Octet_1 : TOctet;
                           Octet_2 : TOctet)
   return Natural is
   begin
      return (MPI_Total_Bits (Octet_1, Octet_2) + 7) / 8;
   end MPI_Byte_Size;



   ---------------------------
   --  Construct_Unix_Time  --
   ---------------------------

   function Construct_Unix_Time (Octet_1 : TOctet;
                                 Octet_2 : TOctet;
                                 Octet_3 : TOctet;
                                 Octet_4 : TOctet)
   return TUnixTime is
      NOct1 : constant TUnixTime := TUnixTime (Octet_1) * (2 ** 24);
      NOct2 : constant TUnixTime := TUnixTime (Octet_2) * (2 ** 16);
      NOct3 : constant TUnixTime := TUnixTime (Octet_3) * (2 ** 8);
      NOct4 : constant TUnixTime := TUnixTime (Octet_4);
   begin
      return NOct1 + NOct2 + NOct3 + NOct4;
   end Construct_Unix_Time;



   -------------------------------------
   --  Convert_Octet_To_PK_Algorithm  --
   -------------------------------------

   function Convert_Octet_To_PK_Algorithm (Octet : TOctet)
   return TPubKey_Algorithm is
   begin
      case Octet is
         when      1 => return RSA_Encrypt_Or_Sign;
         when      2 => return RSA_Encrypt_Only;
         when      3 => return RSA_Sign_Only;
         when     16 => return Elgamal_Encrypt_Only;
         when     17 => return DSA;
         when     18 => return Reserved_Elliptic_Curve;
         when     19 => return Reserved_ECDSA;
         when     20 => return Reserved_20;
         when     21 => return Reserved_Diffie_Hellman;
         when    100 => return Private_100;
         when    101 => return Private_101;
         when    102 => return Private_102;
         when    103 => return Private_103;
         when    104 => return Private_104;
         when    105 => return Private_105;
         when    106 => return Private_106;
         when    107 => return Private_107;
         when    108 => return Private_108;
         when    109 => return Private_109;
         when    110 => return Private_110;
         when others => return Undefined;
      end case;
   end Convert_Octet_To_PK_Algorithm;



   --------------------------------
   --  Convert_Octet_To_Hash_ID  --
   --------------------------------

   function Convert_Octet_To_Hash_ID (Octet : TOctet)
   return THash_Algorithm is
   begin
      case Octet is
         when      1 => return MD5;
         when      2 => return SHA_1;
         when      3 => return RIPE_MD160;
         when      4 => return reserved_4;
         when      5 => return reserved_5;
         when      6 => return reserved_6;
         when      7 => return reserved_7;
         when      8 => return SHA_256;
         when      9 => return SHA_384;
         when     10 => return SHA_512;
         when     11 => return SHA_224;
         when    100 => return Private_100;
         when    101 => return Private_101;
         when    102 => return Private_102;
         when    103 => return Private_103;
         when    104 => return Private_104;
         when    105 => return Private_105;
         when    106 => return Private_106;
         when    107 => return Private_107;
         when    108 => return Private_108;
         when    109 => return Private_109;
         when    110 => return Private_110;
         when others => return Undefined;
      end case;
   end Convert_Octet_To_Hash_ID;



   ------------------------
   --  Two_Octet_Length  --
   ------------------------

   function Two_Octet_Length (Octet_1 : TOctet;
                              Octet_2 : TOctet)
   return TBody_Length is
      shift8  : constant TBody_Length := 2 ** 8;
   begin
      return TBody_Length (Octet_1) - 192) * shift8 +
             TBody_Length (Octet_2) +
             192;
   end Two_Octet_Length;



   -------------------------
   --  Four_Octet_Length  --
   -------------------------

   function Four_Octet_Length (Octet_1 : TOctet;
                               Octet_2 : TOctet;
                               Octet_3 : TOctet;
                               Octet_4 : TOctet)
   return TBody_Length is
      shift24 : constant TBody_Length := 2 ** 24;
      shift16 : constant TBody_Length := 2 ** 16;
      shift8  : constant TBody_Length := 2 ** 8;
   begin
      return TBody_Length (Octet_1) * shift24 +
             TBody_Length (Octet_2) * shift16 +
             TBody_Length (Octet_3) * shift8  +
             TBody_Length (Octet_4);
   end Four_Octet_Length;


end OpenPGP_Utilities;
