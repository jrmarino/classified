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

package body Packet_Type_1 is


   function Scan_Packet (Header : TPacket_Header;
                         Packet : TOctet_Array)
   return TPacket_1_Fixed is
      index : constant Natural := Header.Body_Starts;
      result : TPacket_1_Fixed;
      Bodylen : Natural := 0;
   begin

      MPI_1st  := (Head => 0, Tail => 0, Length => 0);
      MPI_2nd  := (Head => 0, Tail => 0, Length => 0);
      Raw_data := (others => 0);

      if Header.Body_Length < 20 then
         result.Error := body_too_short;
         return result;
      end if;

      if Packet (index) /= 3 then
         result.Error := invalid_version_number;
         return result;
      end if;

      result.Version   := 3;
      result.KeyID     := Packet (index + 1 .. index + 8);
      result.Algorithm := Convert_Octet_To_PK_Algorithm (Packet (index + 9));

      --  RFC 4880 is very unclear regarding the format of the DSA Encrypted
      --  session key.  It specifically addresses Elgamal and RSA.  It appears
      --  DSA is not a valid algorithm for an encrypted session.  Therefore the
      --  body contains either one or 2 MPI values;

      if (result.Algorithm /= RSA_Encrypt_Or_Sign) and then
         (result.Algorithm /= RSA_Encrypt_Only) and then
         (result.Algorithm /= Elgamal_Encrypt_Only) then
            result.Error := illegal_algorithm;
            return result;
      end if;

      declare
         --  all known algorithms have at least one MPI, check that it is valid
         Size : constant Natural := MPI_Byte_Size (
                                       Octet_1 => Packet (index + 10),
                                       Octet_2 => Packet (index + 11));
         Remaining : constant Natural := Packet'Length - index - 11;
      begin
         if Size > Remaining then
            result.Error := MPI_1_missing_data;
            return result;
         end if;

         if         (result.Algorithm = RSA_Encrypt_Or_Sign)
            or else (result.Algorithm = RSA_Encrypt_Only) then
            if Size < 128 then  --  1024 bits
               result.Error := session_too_short;
               return result;
            end if;
            if Size > 512 then  --  4096 bits
               result.Error := session_too_long;
               return result;
            end if;
         elsif result.Algorithm = Elgamal_Encrypt_Only then  -- redundant
            if Size < 128 then  --  1024 bits
               result.Error := session_too_short;
               return result;
            end if;
            if Size > 256 then  --  2048 bits
               result.Error := session_too_long;
               return result;
            end if;
         end if;
         Bodylen := Size + 2;
         MPI_1st := (Head   => index + 10,
                     Tail   => index + 11 + Size,
                     Length => Bodylen);
      end;

      if (result.Algorithm = RSA_Encrypt_Or_Sign)
      or else (result.Algorithm = RSA_Encrypt_Only) then
         declare
            index2 : constant Natural := MPI_1st.Tail + 1;
            Size : constant Natural := MPI_Byte_Size (
                                       Octet_1 => Packet (index2),
                                       Octet_2 => Packet (index2 + 1));
            Remaining : constant Natural := Packet'Length - MPI_1st.Tail;
         begin
            if Size > Remaining then
               result.Error := MPI_2_missing_data;
               return result;
            end if;

            if         (result.Algorithm = RSA_Encrypt_Or_Sign)
               or else (result.Algorithm = RSA_Encrypt_Only) then
               if Size < 128 then  --  1024 bits
                  result.Error := session2_too_short;
                  return result;
               end if;
               if Size > 512 then  --  4096 bits
                  result.Error := session2_too_long;
                  return result;
               end if;
            end if;
            Bodylen := Bodylen + Size + 2;
            MPI_2nd := (Head   => index2,
                        Tail   => index2 + Size + 1,
                        Length => Size + 2);
         end;
      end if;

      Raw_data (0 .. Bodylen - 1) := Packet (10 .. 9 + Bodylen);
      return result;

   end Scan_Packet;



   ------------------
   --  First_TMPI  --
   ------------------

   function First_TMPI
   return TMPI is
      result : constant TMPI (0 .. MPI_1st.Length - 1) :=
               Raw_data (MPI_1st.Head .. MPI_1st.Tail);
   begin
      return result;
   end First_TMPI;



   -------------------
   --  Second_TMPI  --
   -------------------

   function Second_TMPI
   return TMPI is
      result : constant TMPI (0 .. MPI_2nd.Length - 1) :=
               Raw_data (MPI_2nd.Head .. MPI_2nd.Tail);
   begin
      return result;
   end Second_TMPI;



   ------------------------
   --  Retrieve_RSA_men  --
   ------------------------

   function Retrieve_RSA_men
   return TMPI is
   begin
      return First_TMPI;
   end Retrieve_RSA_men;



   ----------------------------
   --  Retrieve_Elgamal_gkp  --
   ----------------------------

   function Retrieve_Elgamal_gkp
   return TMPI is
   begin
      return First_TMPI;
   end Retrieve_Elgamal_gkp;



   -----------------------------
   --  Retrieve_Elgamal_mykp  --
   -----------------------------

   function Retrieve_Elgamal_mykp
   return TMPI is
   begin
      return Second_TMPI;
   end Retrieve_Elgamal_mykp;



   -----------------------------------
   --  Construct_Type_1_RSA_Packet  --
   -----------------------------------

   function Construct_Type_1_RSA_Packet (KeyID     : TKeyID;
                                         Value_men : TMPI)
   return TOctet_Array is
      Last_Index : constant Natural := Value_men'Length + 9;
      Result : TOctet_Array (0 .. Last_Index) := (others => 0);
   begin

      Result (0)                := 3;     --  Version 3
      Result (1 .. 8)           := KeyID;
      Result (9)                := 1;     --  RSA_Encrypt_Or_Sign
      Result (10 .. Last_Index) := Value_men;

      return Result;

   end Construct_Type_1_RSA_Packet;



   ---------------------------------------
   --  Construct_Type_1_Elgamal_Packet  --
   ---------------------------------------

   function Construct_Type_1_Elgamal_Packet (KeyID      : TKeyID;
                                             Value_gkp  : TMPI;
                                             Value_mykp : TMPI)
   return TOctet_Array is
      Last_Index : constant Natural := Value_gkp'Length +
                                       Value_mykp'Length + 9;
      Mid_Index  : constant Natural := Value_gkp'Length + 9;
      Result : TOctet_Array (0 .. Last_Index) := (others => 0);
   begin

      Result (0)                := 3;     --  Version 3
      Result (1 .. 8)           := KeyID;
      Result (9)                := 16;    --  Elgamal_Encrypt_Only
      Result (10 .. Mid_Index)  := Value_gkp;
      Result (Mid_Index + 1 .. Last_Index) := Value_mykp;

      return Result;

   end Construct_Type_1_Elgamal_Packet;



end Packet_Type_1;
