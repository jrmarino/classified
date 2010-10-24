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


with Binding_Zlib;
with Ada.Streams.Stream_IO;

package Archive is

   package IC   renames Binding_Zlib.IC;
   package ICS  renames Binding_Zlib.ICS;
   package ASIO renames Ada.Streams.Stream_IO;

   type TStrategy is (
      default_strategy,
      filtered,
      huffman,
      rle,
      fixed
   );

   type TFlush is (
      no_flush,
      partial_flush,
      sync_flush,
      full_flush,
      finish,
      block,
      trees
   );
   type TCompression_Levels is new Integer range -1 .. 9;
   type TWindowBits         is new Integer range 8 .. 15;
   type TMemLevel           is new Integer range 1 .. 9;
   type TStream_Unit        is mod 2 ** 8;

   DEFAULT_MEMLEVEL   : constant TMemLevel   := 8;
   DEFAULT_WINDOWBITS : constant TWindowBits := 15;

   type TZipByte is mod 2 ** 8;
   subtype TZipStream is ASIO.Stream_Access;



   function ZLib_Version return String;
   --  Returns the version string of the zlib library being used.



private

   pragma Assert (Ada.Streams.Stream_Element'Size = TZipByte'Size);


end Archive;
