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


package body OpenPGP_Messages is


   --------------------------
   --  Scan_Packet_Header  --
   --------------------------

   function Scan_Packet_Header (Packet : TOctet_Array)
   return TPacket_Header is
      blank   : TPacket_Header;
      work    : TPacket_Header;
      shift24 : constant TBody_Length := 2 ** 24;
      shift16 : constant TBody_Length := 2 ** 16;
      shift8  : constant TBody_Length := 2 ** 8;
   begin
      if Packet'Length < 2 then
         return blank;
      end if;

      case Packet (0) is
         when  1 => work.Packet_Tag := Public_Key_Encrypted_Session;
         when  2 => work.Packet_Tag := Signature;
         when  3 => work.Packet_Tag := Symmetic_Key_Encrypted_Session;
         when  4 => work.Packet_Tag := One_Pass_Signature_Packet;
         when  5 => work.Packet_Tag := Secret_Key;
         when  6 => work.Packet_Tag := Public_key;
         when  7 => work.Packet_Tag := Secret_Subkey;
         when  8 => work.Packet_Tag := Compressed_Data;
         when  9 => work.Packet_Tag := Symmetrically_Encrypted_Data;
         when 10 => work.Packet_Tag := Marker;
         when 11 => work.Packet_Tag := Literal_Data;
         when 12 => work.Packet_Tag := Trust;
         when 13 => work.Packet_Tag := User_ID;
         when 14 => work.Packet_Tag := Public_Subkey;
         when 17 => work.Packet_Tag := User_Attribute;
         when 18 => work.Packet_Tag := Sym_Encrypted_Integrity_Protected_Data;
         when 19 => work.Packet_Tag := Modification_Detection_Code;
         when 60 => work.Packet_Tag := Private_60;
         when 61 => work.Packet_Tag := Private_61;
         when 62 => work.Packet_Tag := Private_62;
         when 63 => work.Packet_Tag := Private_63;
         when others => null;
      end case;

      if Packet (1) < 192 then
         work.Body_Length := TBody_Length (Packet (1));
         work.Body_Starts := 2;
      elsif Packet (1) < 223 then
         if Packet'Length < 3 then
            return blank;
         end if;
         work.Body_Length := ((TBody_Length (Packet (1)) - 192) * shift8) +
                               TBody_Length (Packet (2)) + 192;
         work.Body_Starts := 3;
      elsif Packet (1) = 255 then
         if Packet'Length < 6 then
            return blank;
         end if;
         work.Body_Length := (TBody_Length (Packet (2)) * shift24) +
                             (TBody_Length (Packet (3)) * shift16) +
                             (TBody_Length (Packet (4)) * shift8)  +
                              TBody_Length (Packet (5));
         work.Body_Starts := 6;
      else
         --  Indeterminate length
         declare
            power : constant Natural := Natural (Packet (1) and 16#1F#);
         begin
            work.Body_Length := TBody_Length (2 ** power);
            work.Body_Starts := 2;
         end;
         if Packet'Length < work.Body_Length + 2 then
            return blank;
         end if;
      end if;

      return work;

   end Scan_Packet_Header;


end OpenPGP_Messages;
