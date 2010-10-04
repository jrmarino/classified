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


with Ada.Strings.Fixed;

package body ASCII_Armor is


   -----------------------
   --  Fortify_Message  --
   -----------------------

   function Fortify_Message (Armor_Type  : TArmor_Type;
                             Keypair_Set : TArmor_Keypair_Set;
                             Message_R64 : String;
                             CRC24_R64   : String;
                             Part_X      : Positive;
                             Sum_Parts   : Positive)
   return String is
   begin
      return Outer_Line (Line_Type  => armor_head,
                         Armor_Type => Armor_Type,
                         Part_X     => Part_X,
                         Sum_Parts  => Sum_Parts)
           & format_keypairs (Keypair_Set => Keypair_Set)
           & Format_Message_in_76_char_lines (Message => Message_R64)
           & CRC24_R64
           & Character'Val (10)
           & Outer_Line (Line_Type  => armor_tail,
                         Armor_Type => Armor_Type,
                         Part_X     => Part_X,
                         Sum_Parts  => Sum_Parts);
   end Fortify_Message;



   --------------------------------------
   --  Fortify_Message  (Alternate 1)  --
   --------------------------------------

   function Fortify_Message (Armor_Type  : TArmor_Type;
                             Keypair_Set : TArmor_Keypair_Set;
                             Message_R64 : String;
                             CRC24_R64   : String;
                             Part_X      : Positive)
   return String is
   begin
      return Fortify_Message (Armor_Type  => Armor_Type,
                              Keypair_Set => Keypair_Set,
                              Message_R64 => Message_R64,
                              CRC24_R64   => CRC24_R64,
                              Part_X      => Part_X,
                              Sum_Parts   => Part_X);
   end Fortify_Message;



   --------------------------------------
   --  Fortify_Message  (Alternate 2)  --
   --------------------------------------

   function Fortify_Message (Armor_Type  : TArmor_Type;
                             Keypair_Set : TArmor_Keypair_Set;
                             Message_R64 : String;
                             CRC24_R64   : String)    return String is
   begin
      return Fortify_Message (Armor_Type  => Armor_Type,
                              Keypair_Set => Keypair_Set,
                              Message_R64 => Message_R64,
                              CRC24_R64   => CRC24_R64,
                              Part_X      => 1,
                              Sum_Parts   => 1);
   end Fortify_Message;



   ------------------
   --  Outer_Line  --
   ------------------

   function Outer_Line (Line_Type  : TOuterLayer;
                        Armor_Type : TArmor_Type;
                        Part_X     : Positive;
                        Sum_Parts  : Positive)
   return String is
      Dashes : constant String := "-----";
      CR     : constant String (1 .. 1) := (1 => Character'Val (10));
      function Title (Armor_Type2 : TArmor_Type;
                      X           : Positive;
                      Y           : Positive) return String;
      function Prefix (Line_Type2 : TOuterLayer) return String;


      function Title (Armor_Type2 : TArmor_Type;
                      X           : Positive;
                      Y           : Positive) return String is
         PartX : constant String := Ada.Strings.Fixed.Trim (
                                                Source => Integer'Image (X),
                                                Side   => Ada.Strings.Left);
         SumY  : constant String := Ada.Strings.Fixed.Trim (
                                                Source => Integer'Image (Y),
                                                Side   => Ada.Strings.Left);
      begin
         case Armor_Type2 is
            when armor_message          => return "PGP MESSAGE";
            when armor_signature        => return "PGP SIGNATURE";
            when armor_public_key       => return "PGP PUBLIC KEY BLOCK";
            when armor_private_key      => return "PGP PRIVATE KEY BLOCK";
            when armor_multipart_x      => return "PGP MESSAGE, PART " &
                                                   PartX;
            when armor_multipart_x_of_y => return "PGP MESSAGE, PART " &
                                                   PartX & "/" & SumY;
         end case;
      end Title;

      function Prefix (Line_Type2 : TOuterLayer) return String is
      begin
         case Line_Type2 is
            when armor_head => return "BEGIN ";
            when armor_tail => return "END ";
         end case;
      end Prefix;
   begin
      return Dashes &
             Prefix (Line_Type2 => Line_Type) &
             Title (Armor_Type2 => Armor_Type, X => Part_X, Y => Sum_Parts) &
             Dashes &
             CR;
   end Outer_Line;



   ---------------------------------------
   --  Format_Message_in_76_char_lines  --
   ---------------------------------------

   function Format_Message_in_76_char_lines (Message : String)
   return String is
      maxWidth : constant Positive := 76;
      numCR    : constant Positive := 1 + ((Message'Length + 75) / 76);
      newLen   : constant Positive := Message'Length + numCR;
      result   : String (1 .. newLen) := (others => Character'Val (10));
      index    : Positive := 2;
      arrow    : Natural  := 0;
      segment  : Positive;
   begin

      while arrow < Message'Length loop
         segment := Message'Length - arrow;
         if segment > maxWidth then
            segment := maxWidth;
         end if;
         result (index .. index + segment - 1) :=
               Message (arrow + 1 .. arrow + segment);
         arrow := arrow + segment;
         index := index + segment + 1;
      end loop;
      return result;

      --  For 160 character Message, result len = 160 + 1 + 3 = 164
      --  start: index=2 arrow=0
      --  pass1: segment=160-0 => 76.  result (2 .. 77) = msg (1 .. 76)
      --         index+=77=79, arrow+=76=76
      --  pass2: segment=160-76=84 => 76, result (79 .. 154) = msg (77 .. 152)
      --         index+=77=156, arrow+=76=152
      --  pass3: segment=160-152=8, result (156 .. 163) = msg (153 .. 160)
      --         index+=9=165, array+=160
      --  CR remain at position 1, 78, 155, 164  check.

   end Format_Message_in_76_char_lines;


   function format_keypairs (Keypair_Set : TArmor_Keypair_Set)
   return String is
      function Key (Armor_Header : TArmor_Headers) return String;

      function Key (Armor_Header : TArmor_Headers)
      return String is
      begin
         case Armor_Header is
            when Header_Version   => return "Version: ";
            when Header_Comment   => return "Comment: ";
            when Header_MessageID => return "MessageID: ";
            when Header_Hash      => return "Hash: ";
            when Header_Charset   => return "Charset: ";
            when others           => return "";
         end case;
      end Key;

      Selection_Size : Natural := 0;
      EmptyMessage   : constant String := "";
   begin
      for x in Keypair_Set'Range loop
         declare
            label     : constant String := Key (Keypair_Set (x).Header);
            labelSize : constant Natural := label'Length;
         begin
            if labelSize > 0 then
               Selection_Size := Selection_Size + labelSize +
                                 SU.Length (Keypair_Set (x).Value) + 1;
            end if;
         end;
      end loop;
      if Selection_Size = 0 then
         return EmptyMessage;
      end if;

      declare
         result : String (1 .. Selection_Size);
         index  : Positive := 1;
      begin
         for x in Keypair_Set'Range loop
            declare
               label     : constant String := Key (Keypair_Set (x).Header);
               labelSize : constant Natural := label'Length;
               line      : constant String := label
                                   & SU.To_String (Keypair_Set (x).Value)
                                   & Character'Val (10);
            begin
               if labelSize > 0 then
                  result (index .. index + line'Length - 1) := line;
                  index := index + line'Length;
               end if;
            end;
         end loop;
         return result;
      end;

   end format_keypairs;

end ASCII_Armor;
