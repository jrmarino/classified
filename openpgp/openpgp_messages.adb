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

package body OpenPGP_Messages is


   --------------------------
   --  Scan_Packet_Header  --
   --------------------------

   function Scan_Packet_Header (Packet : TOctet_Array)
   return TPacket_Header_Set is
      subtype TLenFormat is Natural range 0 .. 3;
      function old_style_length (ndx : Natural) return TBody_Length;
      function new_style_length (ndx : Natural) return TBody_Length;
      function old_style_start  (ndx : Natural) return Natural;
      function new_style_start  (ndx : Natural) return Natural;
      function tag_type (packet_tag : TOctet)   return TPacket_Tag;


      Nothing      : TPacket_Header_Set (1 .. 0);
      index        : Natural := 0;
      format_octet : TOctet;
      starts       : Natural;
      pack_length  : TBody_Length;
      num_packets  : Natural := 0;


      function old_style_length (ndx : Natural)
      return TBody_Length is
         B2         : constant Natural := 2 ** 8;
         B3         : constant Natural := 2 ** 16;
         B4         : constant Natural := 2 ** 24;
         result     : TBody_Length;
         len_format : TLenFormat;
      begin
         len_format := Natural (Packet (ndx) and 3);
         case len_format is
            when 0 => result := TBody_Length (Natural (Packet (ndx + 1)));
            when 1 => result := TBody_Length (
                                    Natural (Packet (ndx + 1)) * B2 +
                                    Natural (Packet (ndx + 2)));
            when 2 => result := TBody_Length (
                                    Natural (Packet (ndx + 1)) * B4 +
                                    Natural (Packet (ndx + 2)) * B3 +
                                    Natural (Packet (ndx + 3)) * B2 +
                                    Natural (Packet (ndx + 4)));
            when 3 => result := TBody_Length (Packet'Length - 1);
         end case;
         return result;
      end old_style_length;


      function new_style_length (ndx : Natural)
      return TBody_Length is
         result : TBody_Length;
      begin
         case Packet (ndx + 1) is
            when   0 .. 191 => result := TBody_Length (Packet (ndx + 1));
            when 192 .. 223 => result := Two_Octet_Length (
                                             Octet_1 => Packet (ndx + 1),
                                             Octet_2 => Packet (ndx + 2));
            when        255 => result := Four_Octet_Length (
                                             Octet_1 => Packet (ndx + 2),
                                             Octet_2 => Packet (ndx + 3),
                                             Octet_3 => Packet (ndx + 4),
                                             Octet_4 => Packet (ndx + 5));
            when 224 .. 254 =>  --  Indeterminate length
               declare
                  power : constant Natural :=
                                   Natural (Packet (ndx + 1) and 16#1F#);
               begin
                  result := TBody_Length (2 ** power);
               end;
         end case;
         return result;
      end new_style_length;


      function old_style_start (ndx : Natural)
      return Natural is
         result     : Natural;
         len_format : TLenFormat;
      begin
         len_format := Natural (Packet (ndx) and 3);
         case len_format is
            when 0 => result := ndx + 2;
            when 1 => result := ndx + 3;
            when 2 => result := ndx + 5;
            when 3 => result := ndx + 2;
         end case;
         return result;
      end old_style_start;


      function new_style_start (ndx : Natural) return Natural is
         result : Natural;
      begin
         case Packet (ndx + 1) is
            when   0 .. 191 => result := ndx + 2;
            when 192 .. 223 => result := ndx + 3;
            when        255 => result := ndx + 5;
            when 224 .. 254 => result := ndx + 2;
         end case;
         return result;
      end new_style_start;

      function tag_type (packet_tag : TOctet) return TPacket_Tag is
      begin
         case packet_tag is
            when  1 => return Public_Key_Encrypted_Session;
            when  2 => return Signature;
            when  3 => return Symmetic_Key_Encrypted_Session;
            when  4 => return One_Pass_Signature_Packet;
            when  5 => return Secret_Key;
            when  6 => return Public_key;
            when  7 => return Secret_Subkey;
            when  8 => return Compressed_Data;
            when  9 => return Symmetrically_Encrypted_Data;
            when 10 => return Marker;
            when 11 => return Literal_Data;
            when 12 => return Trust;
            when 13 => return User_ID;
            when 14 => return Public_Subkey;
            when 17 => return User_Attribute;
            when 18 => return Sym_Encrypted_Integrity_Protected_Data;
            when 19 => return Modification_Detection_Code;
            when 60 => return Private_60;
            when 61 => return Private_61;
            when 62 => return Private_62;
            when 63 => return Private_63;
            when others => return Undefined;
         end case;
      end tag_type;

   begin
      if Packet'Length < 2 then
         return Nothing;
      end if;

      while index < Packet'Length loop
         format_octet := Packet (index);
         if (format_octet and 64) > 0 then  --  new format
            pack_length := new_style_length (index);
            starts      := new_style_start (index);
         else
            pack_length := old_style_length (index);
            starts      := old_style_start (index);
         end if;
         if index + Natural (pack_length) < Packet'Length then
            num_packets := num_packets + 1;
         end if;
         index := starts + Natural (pack_length);
      end loop;

      declare
         result    : TPacket_Header_Set (1 .. num_packets);
         tag_value : TOctet;
         pkindex   : Natural := 0;
      begin
         index := 0;
         while index < Packet'Length loop
            format_octet := Packet (index);
            if (format_octet and 64) > 0 then  --  new format
               tag_value := format_octet and 63;
               pack_length := new_style_length (index);
               starts      := new_style_start (index);
            else
               tag_value := (format_octet and 63) / 4;
               pack_length := old_style_length (index);
               starts      := old_style_start (index);
            end if;
            if index + Natural (pack_length) < Packet'Length then
               pkindex := pkindex + 1;
               result (pkindex).Packet_Tag  := tag_type (tag_value);
               result (pkindex).Body_Length := pack_length;
               result (pkindex).Body_Starts := starts;
            end if;
            index := starts + Natural (pack_length);
         end loop;
         return result;
      end;

   end Scan_Packet_Header;


end OpenPGP_Messages;
