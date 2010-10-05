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


with OpenPGP_Utilities; use OpenPGP_Utilities;
with Ada.Strings.Fixed;
with Ada.Characters.Latin_1;
with GNAT.Regpat;

package body ASCII_Armor is

   package Regex renames GNAT.Regpat;

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
           & Ada.Characters.Latin_1.LF
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
             Ada.Characters.Latin_1.LF;
   end Outer_Line;



   ---------------------------------------
   --  Format_Message_in_76_char_lines  --
   ---------------------------------------

   function Format_Message_in_76_char_lines (Message : String)
   return String is
      maxWidth : constant Positive := 76;
      numCR    : constant Positive := 1 + ((Message'Length + 75) / 76);
      newLen   : constant Positive := Message'Length + numCR;
      result   : String (1 .. newLen) := (others => Ada.Characters.Latin_1.LF);
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
               linex     : constant String := label
                                   & SU.To_String (Keypair_Set (x).Value)
                                   & Ada.Characters.Latin_1.LF;
            begin
               if labelSize > 0 then
                  result (index .. index + linex'Length - 1) := linex;
                  index := index + linex'Length;
               end if;
            end;
         end loop;
         return result;
      end;

   end format_keypairs;


   ----------------------------
   --  Scan_Armored_Message  --
   ----------------------------

   procedure Scan_Armored_Message (Armored_Message : in  SU.Unbounded_String;
                                   Armor_Type      : out TArmor_Type;
                                   Keypair_String  : out SU.Unbounded_String;
                                   Checksum        : out Radix64.TCRC24;
                                   Payload_String  : out SU.Unbounded_String;
                                   Part_Number     : out Positive;
                                   Total_Parts     : out Natural;
                                   Scan_Error      : out TArmor_Scan_Error)
   is
      AARE : constant String := "(^|\n)-----BEGIN PGP (.+)-----\n" &
                                 "([[:cntrl:][:graph:]]+\n|)" &
                                 "\n" &
                                 "([[:ascii:]]+\n)" &
                                 "=([[:ascii:]]{4})\n" &
                                 "-----END PGP (.+)-----\n";
      Matcher : constant Regex.Pattern_Matcher := Regex.Compile (AARE);
      Matches : Regex.Match_Array (0 .. Regex.Paren_Count (Matcher));
      work    : constant String := SU.To_String (Armored_Message);
   begin
      Checksum       := 0;
      Part_Number    := 1;
      Total_Parts    := 1;
      Armor_Type     := armor_message;
      Keypair_String := SU.Null_Unbounded_String;
      Payload_String := SU.Null_Unbounded_String;
      Scan_Error     := no_error;
      Regex.Match (Matcher, work, Matches);
      if Matches (0).Last = 0 and then Matches (0).First = 0 then
         Scan_Error := message_corrupt;
         return;
      end if;

      declare
         headstr : constant String :=
                            work (Matches (2).First .. Matches (2).Last);
         tailstr : constant String :=
                            work (Matches (6).First .. Matches (6).Last);
         slash_position : Natural := 0;
         rest_numbers   : Boolean := True;
      begin
         if headstr /= tailstr then
            Scan_Error := message_corrupt;
            return;
         end if;
         if headstr = "MESSAGE" then
            Armor_Type := armor_message;
         elsif headstr = "SIGNATURE" then
            Armor_Type := armor_signature;
         elsif headstr = "PUBLIC KEY BLOCK" then
            Armor_Type := armor_public_key;
         elsif headstr = "PRIVATE KEY BLOCK" then
            Armor_Type := armor_private_key;
         elsif headstr (1 .. 14) = "MESSAGE, PART " then
            for x in Positive range 15 .. headstr'Last loop
               if headstr (x) = '/' then
                  slash_position := x;
               elsif headstr (x) < '0' or else
                     headstr (x) > '9' then
                  rest_numbers := False;
               end if;
            end loop;
            if not rest_numbers or else
               slash_position = 15 or else
               slash_position = headstr'Last then
               Scan_Error := type_not_recognized;
               return;
            end if;
            Part_Number := Integer'Value (headstr (15 .. slash_position - 1));
            if slash_position > 0 then
               Armor_Type  := armor_multipart_x_of_y;
               Total_Parts := Integer'Value (
                              headstr (slash_position + 1 .. headstr'Last));
            else
               Armor_Type  := armor_multipart_x;
               Total_Parts := 0;
            end if;
         else
            Scan_Error := type_not_recognized;
            return;
         end if;
      end;


      Keypair_String := SU.To_Unbounded_String (
                              work (Matches (3).First .. Matches (3).Last));
      declare
         use Radix64;

         tmppl   : constant SU.Unbounded_String := SU.To_Unbounded_String (
                            work (Matches (4).First .. Matches (4).Last));
         tmppl2  : constant String := condense_payload (tmppl);
         tmppl3  : constant TOctet_Array := Decode_Radix64 (tmppl2);
         msgcrc  : constant String := "=" &
                            work (Matches (5).First .. Matches (5).Last);
         xmsgcrc : constant TCRC24 := convert_CRCR64_To_Integer (msgcrc);
      begin
         Checksum := CRC (BinaryString => tmppl3);
         Payload_String := SU.To_Unbounded_String (tmppl2);

         if Checksum /= xmsgcrc then
            Scan_Error := checksum_failed;
         end if;
      end;

   end Scan_Armored_Message;



   -----------------------------
   --  convert_R64_to_Binary  --
   -----------------------------

   function convert_R64_to_Binary (Payload : String)
   return TOctet_Array is
   begin
      return convert_string_to_octet_array (Payload);
   end convert_R64_to_Binary;



   ------------------------
   --  condense_payload  --
   ------------------------

   function condense_payload (provision : SU.Unbounded_String)
   return String is
      numCR : Natural := 0;
   begin
      for x in Positive range 1 .. SU.Length (provision) loop
         if SU.Element (Source => provision, Index  => x) =
            Ada.Characters.Latin_1.LF then
            numCR := numCR + 1;
         end if;
      end loop;
      if numCR >= SU.Length (provision) then
         return "";
      end if;
      declare
         LastIndex : constant Positive := SU.Length (provision) - numCR;
         scratch   : String (1 .. LastIndex);
         index     : Positive := 1;
         paychar   : Character;
      begin
         for x in Positive range 1 .. SU.Length (provision) loop
            paychar := SU.Element (Source => provision, Index  => x);
            if paychar /= Ada.Characters.Latin_1.LF then
               scratch (index) := paychar;
               index := index + 1;
            end if;
         end loop;
         return scratch;
      end;
   end condense_payload;



   --------------------------
   --  Break_Down_Headers  --
   --------------------------

   function Break_Down_Headers (Data : SU.Unbounded_String)
   return TArmor_Keypair_Set is
      datastr : constant String := SU.To_String (Data);
      nothing : TArmor_Keypair_Set (1 .. 0);
      numLF   : Natural := 0;
   begin
      if datastr'Length = 0 then
         return nothing;
      end if;
      for x in Positive range datastr'Range loop
         if datastr (x) = Ada.Characters.Latin_1.LF then
            numLF := numLF + 1;
         end if;
      end loop;

      declare
         result  : TArmor_Keypair_Set (1 .. numLF);
         pattern : constant String :=
                            "^(Version|Comment|MessageID|Hash|Charset): (.+)$";
         mach    : constant Regex.Pattern_Matcher := Regex.Compile (pattern);
         matches : Regex.Match_Array (0 .. Regex.Paren_Count (mach));
         start   : Positive := 1;
         counter : Positive := 1;
      begin
         for x in Positive range datastr'Range loop
            if datastr (x) = Ada.Characters.Latin_1.LF then
               declare
                  MyLine : constant String := datastr (start .. x - 1);
               begin
                  Regex.Match (mach, MyLine, matches);
                  if matches (0).Last = 0 and then matches (1).First = 0 then
                     result (counter).Header := Header_Unrecognized;
                     result (counter).Value  := SU.To_Unbounded_String (MyLine);
                  else
                     declare
                        word : constant String :=
                               MyLine (matches (1).First .. matches (1).Last);
                     begin
                        if word = "Version" then
                           result (counter).Header := Header_Version;
                        elsif word = "Comment" then
                           result (counter).Header := Header_Comment;
                        elsif word = "MessageID" then
                           result (counter).Header := Header_MessageID;
                        elsif word = "Hash" then
                           result (counter).Header := Header_Hash;
                        elsif word = "Charset" then
                           result (counter).Header := Header_Charset;
                        else
                           result (counter).Header := Header_Unrecognized;
                           --  this is impossible to reach
                        end if;
                     end;
                     result (counter).Value := SU.To_Unbounded_String (
                           MyLine (matches (2).First .. matches (2).Last));
                  end if;
               end;
               start := x + 1;
               counter := counter + 1;
            end if;
         end loop;

         return result;
      end;

   end Break_Down_Headers;


end ASCII_Armor;
