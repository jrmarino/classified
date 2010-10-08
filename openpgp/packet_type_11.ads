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

package Packet_Type_11 is

   package SU renames Ada.Strings.Unbounded;

   type TP11_Error is (
      no_error,
      unrecognized_format,
      message_too_short
   );


   type TDataFormat is (binary, text, utf8_text);
   type TPacket_11_Fixed is record
      DataFormat      : TDataFormat         := binary;
      Filename_Length : TOctet              := 0;
      Filename        : SU.Unbounded_String := SU.Null_Unbounded_String;
      Timestamp       : TUnixTime           := 0;
      Literal_Data    : SU.Unbounded_String := SU.Null_Unbounded_String;
      Error           : TP11_Error          := no_error;
   end record;


   function Construct_Type_11_Packet (input_data : TPacket_11_Fixed)
   return TOctet_Array;
   --  This function returns a complete type 11 literal packet based on the
   --  data provided.


   function Scan_Packet (Header : TPacket_Header;
                         Packet : TOctet_Array) return TPacket_11_Fixed;
   --  This function returns all the available information, there is no
   --  variable data to pull separately.  However, the binary will be stored
   --  within an unbounded string and it will need to get translated back into
   --  a usable form (assuming it's not already usable as an unbounded string.)


   function convert_octet_to_data_format (Octet : TOctet) return TDataFormat;
   --  Converts octet value to one of three recognized formats.  If the value
   --  isn't recognized, it will default to binary.


   function convert_data_format_to_octet (Data_Format : TDataFormat)
   return TOctet;
   --  Converts data format enumeration to the corresponding octet value.


end Packet_Type_11;
