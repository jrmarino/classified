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

package body Packet_Type_18 is


   ------------------------
   --  Retreive_Version  --
   ------------------------

   function Retreive_Version (Header : TPacket_Header;
                              Packet : TOctet_Array)
   return Natural is
      index : constant Natural := Header.Body_Starts;
   begin
      return Natural (Packet (index));
   end Retreive_Version;



   -------------------------------
   --  Retrieve_Encrypted_Data  --
   -------------------------------

   function Retrieve_Encrypted_Data (Header : TPacket_Header;
                                     Packet : TOctet_Array)
   return TOctet_Array is
      index : constant Natural := Header.Body_Starts;
   begin
      return Packet (index + 1 .. Packet'Last);
   end Retrieve_Encrypted_Data;



   --------------------------------
   --  Construct_Type_19_Packet  --
   --------------------------------

   function Construct_Type_18_Packet (Encrypted_Data : TOctet_Array)
   return TOctet_Array is
      Body_Length : constant TBody_Length := Encrypted_Data'Length + 1;
      enclen      : constant TOctet_Array := Encode_Body_Length (Body_Length);
      result      : TOctet_Array (0 .. enclen'Length + Natural (Body_Length));
      index       : Natural;
   begin
      result (0) := Convert_Packet_Tag_To_Octet
                    (Sym_Encrypted_Integrity_Protected_Data);
      result (1 .. enclen'Length) := enclen;
      index      := enclen'Length + 1;

      result (index) := 1;    --  version
      result (index + 1 .. result'Last) := Encrypted_Data;
      return result;

   end Construct_Type_18_Packet;


end Packet_Type_18;
