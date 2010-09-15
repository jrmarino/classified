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


package body Key_4096 is



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


   ----------------------------
   --  Hex2Byte  (Half-key)  --
   ----------------------------

   function Hex2Byte (HalfKey : HalfKeyString)
   return Prime_Matrix.TData is
      index  : Integer := 1;
      octet  : OctetString;
      result : Prime_Matrix.TData := Prime_Matrix.Construct;
   begin
      for x in result.Matrix'Range loop
         octet (1) := HalfKey (index);
         octet (2) := HalfKey (index + 1);

         result.Matrix (x) := Octet2MByte (octet);
         index := index + 2;
      end loop;
      result.CurrentLen := PrimeMatrixLen (result.Matrix'Last) + 1;
      return result;

   end Hex2Byte;


   ----------------------------
   --  Hex2Byte  (Long-key)  --
   ----------------------------

   function Hex2Byte (LongKey : LongKeyString)
   return ModExp_Matrix.TData is
      index  : Integer := 1;
      octet  : OctetString;
      result : ModExp_Matrix.TData := ModExp_Matrix.Construct;
   begin
      for x in result.Matrix'Range loop
         octet (1) := LongKey (index);
         octet (2) := LongKey (index + 1);

         result.Matrix (x) := Octet2MByte (octet);
         index := index + 2;
      end loop;
      result.CurrentLen := ModExpMsgMatrixLen (result.Matrix'Last) + 1;
      return result;

   end Hex2Byte;


end Key_4096;
