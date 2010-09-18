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

   Internal_Error_Code : TCryptoError := 0;
   Encrypted_Message_Length : Natural := 0;


   -------------------
   --  Octet2MByte  --
   -------------------

   function Octet2MByte (Octet : OctetString)
   return MByte is
      ones   : constant Character := Octet (1);
      octs   : constant Character := Octet (2);
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

      return MByte (result);

   end Octet2MByte;



   --------------
   --  Encode  --
   --------------

   function EncodeByte (c : MByte)
   return Character is
      index : constant Integer := Integer (c and 16#3F#) + 1;
   begin
      return BIN2ASC (index);
   end EncodeByte;



   -------------------------
   --  Encode_to_Radix64  --
   -------------------------

   function Encode_to_Radix64 (BinaryString : TBinaryString)
   return String is
      triples    : constant Integer := (BinaryString'Length + 2) / 3;
      Output_Len : constant Integer := triples * 4;
      Result     : String (1 .. Output_Len) := (others => ' ');
      index3     : Integer := 1;
      index4     : Integer := 1;
      count      : TCount;
   begin
      for x in Integer range 1 .. triples loop
         if index3 + 2 <= BinaryString'Length then
            count := 0;
         else
            count := BinaryString'Length rem 3;
         end if;

         Result (index4 .. index4 + 3) :=
                Encode_Three_Bytes (BinaryString, index3, count);
         index3 := index3 + 3;
         index4 := index4 + 4;
      end loop;
      return Result;
   end Encode_to_Radix64;



   --------------------------
   --  Encode_Three_Bytes  --
   --------------------------

   function Encode_Three_Bytes (BinaryString : TBinaryString;
                                Index        : Natural;
                                Count        : TCount)
   return FourSequence is
      c1 : constant MByte := BinaryString (Index) / MByte (4);  -- shr 4
      c2 : constant MByte :=
            ((BinaryString (Index)     * MByte (16)) and 8#60#) or
            ((BinaryString (Index + 1) / MByte (16)) and 8#17#);
      c3 : constant MByte :=
            ((BinaryString (Index + 1) * MByte (4)) and 8#74#) or
            ((BinaryString (Index + 2) / MByte (64)) and 8#3#);
      c4 : constant MByte := BinaryString (Index + 2) and 8#77#;
      result : FourSequence;
   begin
      result (1) := EncodeByte (c1);
      result (2) := EncodeByte (c2);
      if Count = 1 then
         result (3) := PAD;
         result (4) := PAD;
      else
         result (3) := EncodeByte (c3);
         if Count = 2 then
            result (4) := PAD;
         else
            result (4) := EncodeByte (c4);
         end if;
      end if;
      return result;
   end Encode_Three_Bytes;



   ----------------------
   --  Decode_Radix64  --
   ----------------------

   function Decode_Radix64 (Radix64String : String)
   return TBinaryString is
      triples      : constant Integer := (Radix64String'Length + 3) / 4;
      Output_Len   : constant Integer := triples * 3;
      BinaryString : TBinaryString (0 .. Output_Len - 1) := (others => 0);
      c            : array (1 .. 4) of MByte := (others => 0);
      enc          : array (1 .. 4) of MByte := (others => 0);
      index3       : Natural := 0;
      index4       : Positive := 1;
   begin
      Internal_Error_Code      := 0;
      Encrypted_Message_Length := 0;
      if Radix64String'Length = 0 then
         return BinaryString;
      end if;
      if Radix64String'Length rem 4 > 0 then
         Internal_Error_Code := 1;  --  not divisible by 4
         return BinaryString;
      end if;

      for x in Integer range 1 .. triples loop
         for y in Integer range 0 .. 3 loop
            c (1 + y) := MByte (Character'Pos (Radix64String (index4 + y)));
            if c (1 + y) > MByte (TAscii'Last) then
               Internal_Error_Code := 2;  -- 7th bit used
               return BinaryString;
            end if;
            enc (1 + y) := ASC2BIN (TAscii (c (1 + y)));
            if (enc (1 + y) and 16#80#) > 0 then
               Internal_Error_Code := 2;  -- Mapped to non-valid character
               return BinaryString;
            end if;
         end loop;

         if Character'Val (c (3)) = PAD then
            if Character'Val (c (4)) = PAD then
               enc (3) := 0;
               enc (4) := 0;
               Encrypted_Message_Length := Encrypted_Message_Length + 1;
            else
               Internal_Error_Code := 3;  -- missing 4th place pad
               return BinaryString;
            end if;
         else
            if Character'Val (c (4)) = PAD then
               enc (4) := 0;
               Encrypted_Message_Length := Encrypted_Message_Length + 2;
            else
               Encrypted_Message_Length := Encrypted_Message_Length + 3;
            end if;
         end if;

         declare
            shift2 : constant MByte := MByte (2 ** 2);
            shift4 : constant MByte := MByte (2 ** 4);
            shift6 : constant MByte := MByte (2 ** 6);
            HexFF  : constant MByte := MByte'Last;  -- $FF
         begin
            BinaryString (index3)     := ((enc (1) * shift2) and HexFF) or
                                          (enc (2) / shift4);
            BinaryString (index3 + 1) := ((enc (2) * shift4) and HexFF) or
                                          (enc (3) / shift2);
            BinaryString (index3 + 2) := ((enc (3) * shift6) and HexFF) or
                                           enc (4);
         end;

         index3 := index3 + 3;
         index4 := index4 + 4;
      end loop;

      return BinaryString;
   end Decode_Radix64;



   ----------------------------
   --  Print_Status_Message  --
   ----------------------------

   function Get_Status_Message (Status : TCryptoError)
   return String is
   begin
      case Status is
         when  0 => return "No errors.";
         when  1 => return "Radix-64 message length not divisible by 4.";
         when  2 => return "Radix-64 encoding bad, 7th bit used or mismapped.";
         when  3 => return "Radix-64 encoding bad, 4th place pad doesn't " &
                           "follow 3rd place pad.";
         when  4 => return "Encrypted message is larger than key modulus.";
         when  5 => return "PKCS block is not same length as key modulus.";
         when  6 => return "PKCS block does not start with <01>.";
         when  7 => return "PKCS block separator pattern is not <FFFFFF??00>.";
         when  8 => return "Plain text output is more than 11 chars longer " &
                           "than key modulus.";
         when  9 => return "Data Mismatch, message length > key modulus.";
         when 10 => return "Encryption: message length doesn't match modulus.";
         when 11 => return "Encryption: Message too long for modulus.";
      end case;
   end Get_Status_Message;


end Radix64;