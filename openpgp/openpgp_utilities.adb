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



   ---------------------------
   --  Breakdown_Unix_Time  --
   ---------------------------

   function Breakdown_Unix_Time (UnixTime : TUnixTime)
   return TOctet_Array is
      type TMod32 is mod 16#100000000#;
      result  : TOctet_Array (0 .. 3) := (others => 0);
      shift24 : constant TMod32 := 2 ** 24;
      shift16 : constant TMod32 := 2 ** 16;
      shift8  : constant TMod32 := 2 ** 8;
      Mask54  : constant TMod32 := 16#00FF0000#;
      Mask32  : constant TMod32 := 16#0000FF00#;
      Mask10  : constant TMod32 := 16#000000FF#;
      Mod32   : constant TMod32 := TMod32 (UnixTime);
   begin

      result (0) := TOctet  (Mod32             / shift24);
      result (1) := TOctet ((Mod32 and Mask54) / shift16);
      result (2) := TOctet ((Mod32 and Mask32) / shift8);
      result (3) := TOctet  (Mod32 and Mask10);

      return result;

   end Breakdown_Unix_Time;



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



   --------------------------------
   --  Convert_Hash_ID_To_Octet  --
   --------------------------------

   function Convert_Hash_ID_To_Octet (hash_Algorithm : THash_Algorithm)
   return TOctet is
   begin
      case hash_Algorithm is
         when MD5         => return 1;
         when SHA_1       => return 2;
         when RIPE_MD160  => return 3;
         when SHA_256     => return 8;
         when SHA_384     => return 9;
         when SHA_512     => return 10;
         when SHA_224     => return 11;
         when Private_100 => return 100;
         when Private_101 => return 101;
         when Private_102 => return 102;
         when Private_103 => return 103;
         when Private_104 => return 104;
         when Private_105 => return 105;
         when Private_106 => return 106;
         when Private_107 => return 107;
         when Private_108 => return 108;
         when Private_109 => return 109;
         when Private_110 => return 110;
         when reserved_4  => return 4;
         when reserved_5  => return 5;
         when reserved_6  => return 6;
         when reserved_7  => return 7;
         when others      => return 0;
      end case;
   end Convert_Hash_ID_To_Octet;


   ------------------------
   --  Two_Octet_Length  --
   ------------------------

   function Two_Octet_Length (Octet_1 : TOctet;
                              Octet_2 : TOctet)
   return TBody_Length is
      shift8  : constant TBody_Length := 2 ** 8;
   begin
      return TBody_Length ((Octet_1) - 192) * shift8 +
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



   --------------------------
   --  Encode_Body_Length  --
   --------------------------

   function Encode_Body_Length (Body_Length : TBody_Length)
   return TOctet_Array is
      type TModBL is mod 16#100000000#;
      Num_Octets : Positive;
      scratch : TOctet_Array (0 .. 4) := (others => 0);
      shift24 : constant TModBL := 2 ** 24;
      shift16 : constant TModBL := 2 ** 16;
      shift8  : constant TModBL := 2 ** 8;
      Mask54  : constant TModBL := 16#00FF0000#;
      Mask32  : constant TModBL := 16#0000FF00#;
      Mask10  : constant TModBL := 16#000000FF#;
      ModBL   : constant TModBL := TModBL (Body_Length);
   begin
      if Body_Length < 192 then
         Num_Octets := 1;
         scratch (0) := TOctet (Body_Length);
      elsif Body_Length < 8384 then
         Num_Octets := 2;
         declare
            Adbl : constant Natural := Natural (Body_Length) - 192;
            Oct1 : constant TOctet  := TOctet ((Adbl / 256) + 192);
            Oct2 : constant TOctet  := TOctet (Adbl rem 256);
         begin
            scratch (0) := Oct1;
            scratch (1) := Oct2;
         end;
      else
         Num_Octets := 5;
         scratch (0) := 255;
         scratch (1) := TOctet  (ModBL             / shift24);
         scratch (2) := TOctet ((ModBL and Mask54) / shift16);
         scratch (3) := TOctet ((ModBL and Mask32) / shift8);
         scratch (4) := TOctet  (ModBL and Mask10);
      end if;
      return scratch (0 .. Num_Octets - 1);
   end Encode_Body_Length;



   -----------------------------------
   --  Convert_Packet_Tag_To_Octet  --
   -----------------------------------

   function Convert_Packet_Tag_To_Octet (Packet_Tag : TPacket_Tag)
   return TOctet is
   begin
      case Packet_Tag is
         when Public_Key_Encrypted_Session           => return 1;
         when Signature                              => return 2;
         when Symmetic_Key_Encrypted_Session         => return 3;
         when One_Pass_Signature_Packet              => return 4;
         when Secret_Key                             => return 5;
         when Public_key                             => return 6;
         when Secret_Subkey                          => return 7;
         when Compressed_Data                        => return 8;
         when Symmetrically_Encrypted_Data           => return 9;
         when Marker                                 => return 10;
         when Literal_Data                           => return 11;
         when Trust                                  => return 12;
         when User_ID                                => return 13;
         when Public_Subkey                          => return 14;
         when User_Attribute                         => return 17;
         when Sym_Encrypted_Integrity_Protected_Data => return 18;
         when Modification_Detection_Code            => return 19;
         when Private_60                             => return 60;
         when Private_61                             => return 61;
         when Private_62                             => return 62;
         when Private_63                             => return 63;
         when others                                 => return 0;
      end case;
   end Convert_Packet_Tag_To_Octet;



   -------------------------------------
   --  convert_octet_array_to_string  --
   -------------------------------------

   function convert_octet_array_to_string (Block : TOctet_Array)
   return String is
      result : String (1 .. Block'Length);
      index  : Natural := 0;
   begin
      if Block'Length = 0 then
         return result;
      end if;
      for x in Natural range Block'First .. Block'Last loop
         index := index + 1;
         result (index) := Character'Val (Block (x));
      end loop;
      return result;
   end convert_octet_array_to_string;



   -----------------------------------------------
   --  convert_octet_array_to_unbounded_string  --
   -----------------------------------------------

   function convert_octet_array_to_unbounded_string (Block : TOctet_Array)
   return SU.Unbounded_String is
      tmp : constant String := (convert_octet_array_to_string (Block => Block));
   begin
      return SU.To_Unbounded_String (tmp);
   end convert_octet_array_to_unbounded_string;



   -----------------------------------------------
   --  convert_unbounded_string_to_octet_array  --
   -----------------------------------------------

   function convert_unbounded_string_to_octet_array (data : SU.Unbounded_String)
   return TOctet_Array is
      work   : constant String := SU.To_String (Source => data);
      result : TOctet_Array (0 .. work'Length - 1);
      index  : Natural := 0;
   begin
      for x in work'Range loop
         result (index) := TOctet (Character'Pos (work (x)));
         index := index + 1;
      end loop;
      return result;
   end convert_unbounded_string_to_octet_array;



   -------------------------------------
   --  convert_string_to_octet_array  --
   -------------------------------------

   function convert_string_to_octet_array (data : String)
   return TOctet_Array is
      result : TOctet_Array (0 .. data'Length - 1);
      index  : Natural := 0;
   begin
      for x in data'Range loop
         result (index) := TOctet (Character'Pos (data (x)));
         index := index + 1;
      end loop;
      return result;
   end convert_string_to_octet_array;



   ----------------------------------------------
   --  convert_octet_to_compression_algorithm  --
   ----------------------------------------------

   function convert_octet_to_compression_algorithm (Octet : TOctet)
   return TCompression_Algorithm is
   begin
      case Octet is
         when      0 => return Uncompressed;
         when      1 => return ZIP_1951;
         when      2 => return ZLIB_1950;
         when      3 => return BZIP2;
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
   end convert_octet_to_compression_algorithm;



   ----------------------------------------------
   --  convert_compression_algorithm_to_octet  --
   ----------------------------------------------

   function convert_compression_algorithm_to_octet
                                       (Algorithm : TCompression_Algorithm)
   return TOctet is
   begin
      case Algorithm is
         when Uncompressed => return 0;
         when ZIP_1951     => return 1;
         when ZLIB_1950    => return 2;
         when BZIP2        => return 3;
         when Private_100  => return 100;
         when Private_101  => return 101;
         when Private_102  => return 102;
         when Private_103  => return 103;
         when Private_104  => return 104;
         when Private_105  => return 105;
         when Private_106  => return 106;
         when Private_107  => return 107;
         when Private_108  => return 108;
         when Private_109  => return 109;
         when Private_110  => return 110;
         when others       => return 16#FF#;
      end case;
   end convert_compression_algorithm_to_octet;



   ---------------------
   --  mpi_byte_size  --
   ---------------------

   function mpi_byte_size (SU_MPI : TSU_MPI)
   return Natural is
      Len_SU_MPI   : constant Natural := SU.Length (SU_MPI);
      bit_length   : Natural;
      byte_length  : Natural;
      ones         : Natural;
      nxtv         : Natural;
   begin
      if Len_SU_MPI < 3 then
         return 0;
      end if;
      declare
         msb  : constant Character := SU.Element (Source => SU_MPI, Index => 1);
         lsb  : constant Character := SU.Element (Source => SU_MPI, Index => 2);
         next : constant Character := SU.Element (Source => SU_MPI, Index => 3);
         msbv : constant Natural   := Natural (Character'Pos (msb));
         lsbv : constant Natural   := Natural (Character'Pos (lsb));
      begin
         bit_length  := (msbv * 256) + lsbv;
         byte_length := (bit_length + 7) / 8;
         ones        := bit_length mod 8;
         nxtv := Natural (Character'Pos (next));
      end;
      if ones > 0 and then nxtv >= 2 ** ones then
         return 0;  -- more significant bits than specified present.
      end if;
      return byte_length;
   end mpi_byte_size;



   ----------------------------------
   --  convert_mpi_to_octet_array  --
   ----------------------------------

   function convert_mpi_to_octet_array (SU_MPI        : TSU_MPI;
                                        Number_Octets : Positive)
   return TOctet_Array is
      Len_SU_MPI  : constant Natural := SU.Length (SU_MPI);
      result      : TOctet_Array (0 .. Number_Octets - 1) := (others => 0);
      index       : Natural;
      single_char : Character;
   begin
      index := Number_Octets + 2 - Len_SU_MPI;
      for x in Positive range 3 .. Len_SU_MPI loop
         single_char := SU.Element (Source => SU_MPI, Index => x);
         result (index) := TOctet (Character'Pos (single_char));
         index := index + 1;
      end loop;
      return result;
   end convert_mpi_to_octet_array;



   -------------------
   --  extract_mpi  --
   -------------------

   function extract_mpi (start_index  : Natural;
                         octet_array  : TOctet_Array)
   return SU.Unbounded_String is
      remaining  : constant Natural := octet_array'Length - start_index + 1;
      size       : constant Natural := MPI_Byte_Size (
                            Octet_1 => octet_array (start_index),
                            Octet_2 => octet_array (start_index + 1));
      last_index : constant Natural := start_index + size - 1;
   begin
      if size > remaining then
         return SU.Null_Unbounded_String;
      end if;
      return convert_octet_array_to_unbounded_string (
                  Block => octet_array (start_index .. last_index));
   end extract_mpi;


end OpenPGP_Utilities;
