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


with OpenPGP_Types; use OpenPGP_Types;
with Ada.Strings.Unbounded;
with Radix64;

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
   type TArmor_Scan_Error is (
      no_error,
      message_corrupt,
      type_not_recognized,
      checksum_failed
   );


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


   procedure Scan_Armored_Message (Armored_Message : in  SU.Unbounded_String;
                                   Armor_Type      : out TArmor_Type;
                                   Keypair_String  : out SU.Unbounded_String;
                                   Checksum        : out Radix64.TCRC24;
                                   Payload_String  : out SU.Unbounded_String;
                                   Part_Number     : out Positive;
                                   Total_Parts     : out Natural;
                                   Scan_Error      : out TArmor_Scan_Error);
   --  This procedure will accept an ASCII armored message and will return
   --  all the information that it contains.  Messages without part numbers
   --  will have part_number=1, total_parts=1.  Messages with only Part X will
   --  have part_number=X, total_parts=0.


   function convert_R64_to_Binary (Payload : String) return TOctet_Array;
   --  This function takes the payload from the ASCII armored message, it
   --  strips out the newline characters, and then converts the whole thing
   --  to a string of octets.

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


   function condense_payload (provision : SU.Unbounded_String) return String;
   --  This internal function takes the 76-character limited-width block of
   --  Radix64 encoding and converts it to a string without newline characters.


end ASCII_Armor;
