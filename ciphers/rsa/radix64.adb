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

   function Encode_Three_Bytes (BinaryString : TBinaryString;
                                Index        : Natural;
                                Count        : TCount)
   return FourSequence is
      P0 : constant MByte := BinaryString (Index);
      P1 : MByte := 0;
      P2 : MByte := 0;

      result : FourSequence;
   begin
      if Index + 1 < BinaryString'Length then
         P1 := BinaryString (Index + 1);
      end if;
      if Index + 2 < BinaryString'Length then
         P2 := BinaryString (Index + 2);
      end if;

      declare
         c1 : constant MByte :=  Scroll_Right (P0, 2);
         c2 : constant MByte := (Scroll_Left  (P0, 4) and 8#60#) or
                                (Scroll_Right (P1, 4) and 8#17#);
         c3 : constant MByte := (Scroll_Left  (P1, 2) and 8#74#) or
                                (Scroll_Right (P2, 6) and 8#3#);
         c4 : constant MByte := P2 and 8#77#;
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
   return TBinaryString is
      triples    : constant Integer := (Radix64String'Length + 3) / 4;
      Max_Len    : constant Integer := triples * 3;
      WorkString : TBinaryString (0 .. Max_Len - 1) := (others => 0);
      c          : array (1 .. 4) of MByte := (others => 0);
      enc        : array (1 .. 4) of MByte := (others => 0);
      index3     : Natural := 0;
      index4     : Positive := 1;
      msg_length : Natural := 0;
      error_msg  : constant TBinaryString (0 .. 2) := (5, 5, 5);
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

      for x in Integer range 1 .. triples loop
         for y in Integer range 0 .. 3 loop
            c (1 + y) := MByte (Character'Pos (Radix64String (index4 + y)));
            if c (1 + y) > MByte (TAscii'Last) then
               Internal_Error_Code := 2;  -- 7th bit used
               return error_msg;
            end if;
            enc (1 + y) := ASC2BIN (TAscii (c (1 + y)));
            if (c (1 + y) = PAD) then
               if not ((x = triples) and y >= 2) then
                  Internal_Error_Code := 12;  -- Pad not found at end
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
               Internal_Error_Code := 13;  -- previous 4 bits not clear
               return error_msg;
            end if;
         else
            if c (4) = PAD then
               if (enc (3) and 3) > 0 then
                  Internal_Error_Code := 13;  -- previous 2 bits not clear
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
         when 12 => return "Radix-64 encoding bad, found pad away from end.";
         when 13 => return "Radix-64 encoding bad, character previous to " &
                           "pad doesn't have clear trailing bits.";
      end case;
   end Get_Status_Message;



   ------------------
   --  Scroll_Left  --
   ------------------

   function Scroll_Left (original : MByte;
                         bits     : ShiftRange)
   return MByte is
      mask   : constant MByte := MByte (2 ** (8 - bits)) - 1;
      factor : constant MByte := MByte (2 ** bits);
      canvas : constant MByte := original and mask;
   begin
      return canvas * factor;
   end Scroll_Left;


   -------------------
   --  Scroll_Right  --
   -------------------

   function Scroll_Right (original : MByte;
                          bits     : ShiftRange)
   return MByte is
      mask   : constant MByte := MByte ((2 ** (8 - bits)) - 1);
      factor : constant MByte := MByte (2 ** bits);
      canvas : constant MByte := original / factor;
   begin
      return canvas and mask;
   end Scroll_Right;

end Radix64;