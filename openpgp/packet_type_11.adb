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

package body Packet_Type_11 is

   --------------------------------
   --  Construct_Type_11_Packet  --
   --------------------------------

   function Construct_Type_11_Packet (input_data : TPacket_11_Fixed)
   return TOctet_Array is
      fn_size    : Natural := SU.Length (input_data.Filename);
   begin
      if fn_size > 255 then
         fn_size := 255;
      end if;
      declare
         Body_Length : constant TBody_Length :=  TBody_Length (
                       1 + 1 + fn_size + 4 +
                       SU.Length (input_data.Literal_Data));
         enclen     : constant TOctet_Array := Encode_Body_Length (Body_Length);
         Last_Index : constant Natural := Natural (Body_Length) + enclen'Length;
         Result     : TOctet_Array (0 .. Last_Index) := (others => 0);
         index      : Natural;
         ndxf       : Natural;
         safety     : constant TOctet_Array :=
                               convert_unbounded_to_array (input_data.Filename);
      begin

         Result (0) := Convert_Packet_Tag_To_Octet (Literal_Data);
         Result (1 .. enclen'Length) := enclen;

         index  := enclen'Length + 1;
         Result (index) := convert_data_format_to_octet (input_data.DataFormat);
         Result (index + 1) := TOctet (fn_size);

         index := index + 2;
         ndxf  := index + fn_size - 1;
         Result (index .. ndxf) := safety (1 .. fn_size);

         index := ndxf + 1;
         ndxf  := index + 3;
         Result (index .. ndxf) := Breakdown_Unix_Time (input_data.Timestamp);

         index := ndxf + 1;
         Result (index .. Result'Last) :=
            convert_unbounded_to_array (input_data.Literal_Data);

         return Result;
      end;

   end Construct_Type_11_Packet;



   -------------------
   --  Scan_Packet  --
   -------------------

   function Scan_Packet (Header : TPacket_Header;
                         Packet : TOctet_Array)
   return TPacket_11_Fixed is
      index  : Natural := Header.Body_Starts;
      result : TPacket_11_Fixed;
      MaxNDX : Natural;
   begin
      result.DataFormat      := convert_octet_to_data_format (Packet (index));
      result.Filename_Length := Packet (index + 1);
      MaxNDX                 := index + Natural (result.Filename_Length);
      result.Filename        := convert_octet_array_to_unbounded_string (
                                Block => Packet (index + 2 .. MaxNDX));

      index := MaxNDX + 1;
      result.Timestamp       := Construct_Unix_Time (Packet (index),
            Packet (index + 1), Packet (index + 2), Packet (index + 3));

      index := index + 4;
      result.Literal_Data    := convert_octet_array_to_unbounded_string (
                                Packet (index .. Packet'Last));

      return result;
   end Scan_Packet;



   ------------------------------------
   --  convert_octet_to_data_format  --
   ------------------------------------

   function convert_octet_to_data_format (Octet : TOctet)
   return TDataFormat is
   begin
      case Octet is
         when 16#62# => return binary;
         when 16#74# => return text;
         when 16#75# => return utf8_text;
         when others => return binary;
      end case;
   end convert_octet_to_data_format;



   ------------------------------------
   --  convert_data_format_to_octet  --
   ------------------------------------

   function convert_data_format_to_octet (Data_Format : TDataFormat)
   return TOctet is
   begin
      case Data_Format is
         when binary    => return 16#62#;
         when text      => return 16#74#;
         when utf8_text => return 16#75#;
      end case;
   end convert_data_format_to_octet;



   ----------------------------------
   --  convert_unbounded_to_array  --
   ----------------------------------

   function convert_unbounded_to_array (data : SU.Unbounded_String)
   return TOctet_Array is
      work   : constant String := SU.To_String (Source => data);
      result : TOctet_Array (0 .. work'Length - 1);
      index  : Natural := 0;
   begin
      for x in work'Range loop
         result (index) := TOctet (Character'Pos (work (x)));
         index := index + 1;
      end loop;
      return result;
   end convert_unbounded_to_array;


   -----------------------------------------------
   --  convert_octet_array_to_unbounded_string  --
   -----------------------------------------------

   function convert_octet_array_to_unbounded_string (Block : TOctet_Array)
   return SU.Unbounded_String is
      scratch : constant String := convert_octet_array_to_string (Block);
   begin
      return SU.To_Unbounded_String (scratch);
   end convert_octet_array_to_unbounded_string;

end Packet_Type_11;
