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



end Radix64;