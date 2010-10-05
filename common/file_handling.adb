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


with Ada.Strings.Unbounded.Text_IO;
with Ada.Characters.Latin_1;

package body File_Handling is

   package SUTIO renames Ada.Strings.Unbounded.Text_IO;

   -------------------------
   --  File_Get_Contents  --
   -------------------------

   function File_Get_Contents (filename : String)
   return SU.Unbounded_String is
      File_Handle : TIO.File_Type;
      result      : SU.Unbounded_String := SU.Null_Unbounded_String;
   begin
      declare
         File_Not_Found : constant SU.Unbounded_String :=
                           SU.To_Unbounded_String ("FILE NOT FOUND ERROR: " &
                           filename);
      begin
         TIO.Open (File => File_Handle,
                   Mode => TIO.In_File,
                   Name => filename);
      exception
         when others => return File_Not_Found;
      end;

      while not TIO.End_Of_File (File_Handle) loop
         SU.Append (
            Source   => result,
            New_Item => SUTIO.Get_Line (File_Handle));
         SU.Append (
            Source   => result,
            New_Item => Ada.Characters.Latin_1.LF);
      end loop;

      TIO.Close (File_Handle);
      return result;

   end File_Get_Contents;


end File_Handling;
