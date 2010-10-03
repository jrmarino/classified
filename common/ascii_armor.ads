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


with Ada.Strings.Unbounded;

package ASCII_Armor is

   package SU renames Ada.Strings.Unbounded;

   type TArmor_Type is (
         armor_message,
         armor_signature,
         armor_public_key,
         armor_private_key,
         armor_multipart_x,
         armor_multipart_x_of_y
   );

   type TArmor_Headers is (
      Header_Version,
      Header_Comment,
      Header_MessageID,
      Header_Hash,
      Header_Charset,
      Header_Unrecognized
   );

   type TArmor_Keypair is record
      Header : TArmor_Headers;
      Value  : SU.Unbounded_String;
   end record;

   type TArmor_Keypair_Set is array (Positive range <>) of TArmor_Keypair;

   function Fortify_Message (Armor_Type  : TArmor_Type;
                             Keypair_Set : TArmor_Keypair_Set;
                             Message_R64 : String;
                             CRC24_R64   : String;
                             Part_X      : Positive;
                             Sum_Parts   : Positive) return String;
   --  This is the function that builds ASCII armored messages.
   --  This base version has the most options, including the ability
   --  to specify the Xth part of a known total message parts.
   --  Both the message and CRC checksum are passed already encoded in Radix64


   function Fortify_Message (Armor_Type  : TArmor_Type;
                             Keypair_Set : TArmor_Keypair_Set;
                             Message_R64 : String;
                             CRC24_R64   : String;
                             Part_X      : Positive) return String;
   --  Overloaded version for Xth part of unspecified # of messages


   function Fortify_Message (Armor_Type  : TArmor_Type;
                             Keypair_Set : TArmor_Keypair_Set;
                             Message_R64 : String;
                             CRC24_R64   : String) return String;
   --  Overloaded version for known single message


private

   type TOuterLayer is (armor_head, armor_tail);

   function Outer_Line (Line_Type  : TOuterLayer;
                        Armor_Type : TArmor_Type;
                        Part_X     : Positive;
                        Sum_Parts  : Positive) return String;
   --  This function produces one of the 6 standard header and trailing lines.
   --  It needs to know if it's a header or footer, and which type of message
   --  will be transmitted.  Also supports multipart X of Y


   function Format_Message_in_76_char_lines (Message : String) return String;
   --  The maximum length of a line is 76 characters, and the remainder wraps
   --  to the next line.  This continues until the end of the message has been
   --  reached.



   function format_keypairs (Keypair_Set : TArmor_Keypair_Set) return String;
   --  This function formats all the provided message headers for the
   --  ASCII armor formatting.


end ASCII_Armor;
