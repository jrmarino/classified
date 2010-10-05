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


package body Radix64 is

   ----------------------
   --  Hexstring2Byte  --
   ----------------------

   function Hexstring2Byte (Hex : OctetString)
   return TOctet is
      octs   : constant Character := Hex (1);
      ones   : constant Character := Hex (2);
      result : Integer := 0;
   begin
      case octs is
         when 'a' .. 'f' => result := (Character'Pos (octs) - 87) * 16;
         when 'A' .. 'F' => result := (Character'Pos (octs) - 55) * 16;
         when '0' .. '9' => result := (Character'Pos (octs) - 48) * 16;
         when others     => null;
      end case;
      case ones is
         when 'a' .. 'f' => result := result + (Character'Pos (ones) - 87);
         when 'A' .. 'F' => result := result + (Character'Pos (ones) - 55);
         when '0' .. '9' => result := result + (Character'Pos (ones) - 48);
         when others     => null;
      end case;

      return TOctet (result);

   end Hexstring2Byte;



   ------------------------
   --  Decode_HexString  --
   ------------------------

   function Decode_HexString (HexString : String)
   return TOctet_Array is
      OutputLen : constant Integer := (HexString'Length + 1) / 2;
      result : TOctet_Array (0 .. OutputLen - 1) := (others => 0);
      index : Integer := 1;
      Hex : OctetString;
   begin
      if (HexString'Length rem 2) > 0 then
         return result;
      end if;

      for x in Integer range 1 .. OutputLen loop
         Hex := HexString (index .. index + 1);
         result (x - 1) := Hexstring2Byte (Hex);
         index := index + 2;
      end loop;

      return result;

   end Decode_HexString;



   --------------
   --  Encode  --
   --------------

   function EncodeByte (c : TOctet)
   return Character is
      index : constant Integer := Integer (c and 16#3F#) + 1;
   begin
      return BIN2ASC (index);
   end EncodeByte;



   -------------------------
   --  Encode_to_Radix64  --
   -------------------------

   function Encode_to_Radix64 (BinaryString : TOctet_Array)
   return String is
      triples    : constant Integer := (BinaryString'Length + 2) / 3;
      Output_Len : constant Integer := triples * 4;
      Result     : String (1 .. Output_Len) := (others => ' ');
      index3     : Integer := 0;
      index4     : Integer := 1;
      count      : TCount;
   begin
      for x in Integer range 1 .. triples - 1 loop
         Result (index4 .. index4 + 3) :=
                Encode_Three_Bytes (BinaryString, index3, Count => 0);
         index3 := index3 + 3;
         index4 := index4 + 4;
      end loop;

      if index3 + 3 = BinaryString'Length then
         count := 0;
      else
         count := BinaryString'Length rem 3;
      end if;

      Result (index4 .. index4 + 3) :=
             Encode_Three_Bytes (BinaryString, index3, Count => count);
      return Result;
   end Encode_to_Radix64;



   --------------------------
   --  Encode_Three_Bytes  --
   --------------------------

   function Encode_Three_Bytes (BinaryString : TOctet_Array;
                                Index        : Natural;
                                Count        : TCount)
   return FourSequence is
      P0 : constant TOctet := BinaryString (Index);
      P1 : TOctet := 0;
      P2 : TOctet := 0;

      result : FourSequence;
   begin
      if Index + 1 < BinaryString'Length then
         P1 := BinaryString (Index + 1);
      end if;
      if Index + 2 < BinaryString'Length then
         P2 := BinaryString (Index + 2);
      end if;

      declare
         c1 : constant TOctet :=  Scroll_Right (P0, 2);
         c2 : constant TOctet := (Scroll_Left  (P0, 4) and 8#60#) or
                                (Scroll_Right (P1, 4) and 8#17#);
         c3 : constant TOctet := (Scroll_Left  (P1, 2) and 8#74#) or
                                (Scroll_Right (P2, 6) and 8#3#);
         c4 : constant TOctet := P2 and 8#77#;
      begin
         result (1) := EncodeByte (c1);
         result (2) := EncodeByte (c2);
         if Count = 1 then
            result (3) := Character'Val (PAD);
            result (4) := Character'Val (PAD);
         else
            result (3) := EncodeByte (c3);
            if Count = 2 then
               result (4) := Character'Val (PAD);
            else
               result (4) := EncodeByte (c4);
            end if;
         end if;
      end;
      return result;
   end Encode_Three_Bytes;



   ----------------------
   --  Decode_Radix64  --
   ----------------------

   function Decode_Radix64 (Radix64String : String)
   return TOctet_Array is
      triples    : constant Integer := (Radix64String'Length + 3) / 4;
      Max_Len    : constant Integer := triples * 3;
      WorkString : TOctet_Array (0 .. Max_Len - 1) := (others => 0);
      c          : array (1 .. 4) of TOctet := (others => 0);
      enc        : array (1 .. 4) of TOctet := (others => 0);
      index3     : Natural := 0;
      index4     : Positive := 1;
      msg_length : Natural := 0;
      error_msg  : constant TOctet_Array (0 .. 2) := (5, 5, 5);
   begin
      msg_length          := 0;
      Internal_Error_Code := 0;
      if Radix64String'Length = 0 then
         return error_msg;
      end if;
      if Radix64String'Length rem 4 > 0 then
         Internal_Error_Code := 1;  --  not divisible by 4
         return error_msg;
      end if;

      for x in Positive range 1 .. triples loop
         for y in Natural range 0 .. 3 loop
            c (1 + y) := TOctet (Character'Pos (Radix64String (index4 + y)));
            if c (1 + y) > TOctet (TAscii'Last) then
               Internal_Error_Code := 2;  -- 7th bit used
               return error_msg;
            end if;
            enc (1 + y) := ASC2BIN (TAscii (c (1 + y)));
            if c (1 + y) = PAD then
               if not ((x = triples) and then (y >= 2)) then
                  Internal_Error_Code := 4;  -- Pad not found at end (was 12)
                  return error_msg;
               end if;
            else
               if (enc (1 + y) and 16#80#) > 0 then
                  Internal_Error_Code := 2;  -- Mapped to non-valid character
                  return error_msg;
               end if;
            end if;
         end loop;

   --  need another check to ensure pad is at very end.

         if c (3) = PAD then
            if c (4) = PAD then
               enc (3) := 0;
               enc (4) := 0;
               msg_length := msg_length + 1;
            else
               Internal_Error_Code := 3;  -- missing 4th place pad
               return error_msg;
            end if;
            if (enc (2) and 15) > 0 then
               Internal_Error_Code := 5;  -- previous 4 bits not clear (13)
               return error_msg;
            end if;
         else
            if c (4) = PAD then
               if (enc (3) and 3) > 0 then
                  Internal_Error_Code := 5;  -- previous 2 bits not clear
                  return error_msg;
               end if;
               enc (4) := 0;
               msg_length := msg_length + 2;
            else
               msg_length := msg_length + 3;
            end if;
         end if;

         WorkString (index3)     := Scroll_Left  (enc (1), 2) or
                                    Scroll_Right (enc (2), 4);
         WorkString (index3 + 1) := Scroll_Left  (enc (2), 4) or
                                    Scroll_Right (enc (3), 2);
         WorkString (index3 + 2) := Scroll_Left  (enc (3), 6) or enc (4);

         index3 := index3 + 3;
         index4 := index4 + 4;
      end loop;

      return WorkString (0 .. msg_length - 1);
   end Decode_Radix64;



   -------------------
   --  Scroll_Left  --
   -------------------

   function Scroll_Left (original : TOctet;
                         bits     : ShiftRange)
   return TOctet is
      mask   : constant TOctet := TOctet (2 ** (8 - bits)) - 1;
      factor : constant TOctet := TOctet (2 ** bits);
      canvas : constant TOctet := original and mask;
   begin
      return canvas * factor;
   end Scroll_Left;


   --------------------
   --  Scroll_Right  --
   --------------------

   function Scroll_Right (original : TOctet;
                          bits     : ShiftRange)
   return TOctet is
      factor : constant TOctet := TOctet (2 ** bits);
   begin
      return original / factor;
   end Scroll_Right;



   -------------------------------
   --  Get_Radix_Coding_Status  --
   -------------------------------

   function Get_Radix_Coding_Status
   return Natural is
   begin
      return Internal_Error_Code;
   end Get_Radix_Coding_Status;



   -----------
   --  CRC  --
   -----------

   function CRC (BinaryString : TOctet_Array)
   return TCRC24 is
      type TCRC32 is mod 16#100000000#;
      CRC24_INIT   : constant TCRC32 := 16#B704CE#;
      CRC24_POLY   : constant TCRC32 := 16#1864CFB#;
      factor       : constant TCRC32 := 2 ** 16;
      checksum     : TCRC32 := CRC24_INIT;
      result       : TCRC24;
      shifted_byte : TCRC32;
   begin
      for x in Natural range 0 .. BinaryString'Last loop
         shifted_byte := TCRC32 (BinaryString (x)) * factor;
         checksum := checksum xor shifted_byte;
         for k in Positive range 1 .. 8 loop
            checksum := checksum * 2;  -- shift right 1 bit
            if (checksum and 16#1000000#) > 0 then
               checksum := checksum xor CRC24_POLY;
            end if;
         end loop;
      end loop;

      result := TCRC24 (checksum and 16#FFFFFF#);
      return result;
   end CRC;



   -------------------
   --  CRC_Radix64  --
   -------------------

   function CRC_Radix64 (BinaryString : TOctet_Array)
   return CRCR64String is
      result   : CRCR64String := "=AAAA";
      checksum : constant TCRC24 := CRC (BinaryString);
      factor4  : constant TCRC24 := 2 ** 16;
      factor2  : constant TCRC24 := 2 ** 8;
      scratch  : TOctet_Array (0 .. 2);
   begin
      scratch (0) := TOctet ((checksum and 16#FF0000#) / factor4);
      scratch (1) := TOctet ((checksum and 16#00FF00#) / factor2);
      scratch (2) := TOctet (checksum and 16#0000FF#);
      result (2 .. 5) := Encode_to_Radix64 (BinaryString => scratch);

      return result;
   end CRC_Radix64;



   ---------------------------------
   --  convert_CRCR64_To_Integer  --
   ---------------------------------

   function convert_CRCR64_To_Integer (Checksum : CRCR64String)
   return TCRC24 is
      shift8  : constant TCRC24 := 2 ** 8;
      shift16 : constant TCRC24 := 2 ** 16;
      scratch : constant String (1 .. 4) := Checksum (2 .. Checksum'Last);
      BS      : constant TOctet_Array := Decode_Radix64 (scratch);
   begin
      return TCRC24 (BS (0)) * shift16 +
             TCRC24 (BS (1)) * shift8 +
             TCRC24 (BS (2));

   end convert_CRCR64_To_Integer;

end Radix64;
