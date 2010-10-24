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


package body Binding_Zlib is


   -------------------
   --  inflateInit  --
   -------------------

   function inflateInit (strm : z_streamp)
   return intf is
   begin
      return inflateInit_u (
                  strm        => strm,
                  version     => ICS.New_String (ZLIB_VERSION),
                  stream_size => z_stream'Size);
   end inflateInit;



   -------------------
   --  deflateInit  --
   -------------------

   function deflateInit (
                  strm        : z_streamp;
                  level       : intf)
   return intf is
   begin
      return deflateInit_u (
                  strm        => strm,
                  level       => level,
                  version     => ICS.New_String (ZLIB_VERSION),
                  stream_size => z_stream'Size);
   end deflateInit;



   --------------------
   --  inflateInit2  --
   --------------------

   function inflateInit2 (
                  strm        : z_streamp;
                  windowBits  : intf)
   return intf is
   begin
      return inflateInit2_u (
                  strm        => strm,
                  windowBits  => windowBits,
                  version     => ICS.New_String (ZLIB_VERSION),
                  stream_size => z_stream'Size);
   end inflateInit2;



   --------------------
   --  deflateInit2  --
   --------------------

   function deflateInit2 (
                  strm        : z_streamp;
                  level       : intf;
                  method      : intf;
                  windowBits  : intf;
                  memLevel    : intf;
                  strategy    : intf)
   return intf is
   begin
      return deflateInit2_u (
                  strm        => strm,
                  level       => level,
                  method      => method,
                  windowBits  => windowBits,
                  memLevel    => memLevel,
                  strategy    => strategy,
                  version     => ICS.New_String (ZLIB_VERSION),
                  stream_size => z_stream'Size);
   end deflateInit2;



   -----------------------
   --  inflateBackInit  --
   -----------------------

   function inflateBackInit (
                  strm        : z_streamp;
                  windowBits  : intf;
                  window      : access IC.unsigned_char)
   return intf is
   begin
      return inflateBackInit_u (
                  strm        => strm,
                  windowBits  => windowBits,
                  window      => window,
                  version     => ICS.New_String (ZLIB_VERSION),
                  stream_size => z_stream'Size);
   end inflateBackInit;



end Binding_Zlib;
