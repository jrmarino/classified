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


with Ada.Text_IO;
with Ada.Strings.Unbounded;

package File_Handling is

   package SU renames Ada.Strings.Unbounded;
   package TIO renames Ada.Text_IO;

   function File_Get_Contents (filename : String)
   return SU.Unbounded_String;
   --  Reads an entire file at once, line by line.  Newline characters of "\n"
   --  are added by the function, so a windows files with CRLF terminations
   --  will be altered to LF terminations.  For unix formats, there should be
   --  no difference between the file and the memory versions.

end File_Handling;
