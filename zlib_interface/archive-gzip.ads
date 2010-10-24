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


with Ada.Strings.Unbounded;

package Archive.GZip is

   package SU renames Ada.Strings.Unbounded;

   type TGZip is tagged private;

   STANDARD_GZIP_EXTENSION : constant String := ".gz";

   default_buffer_size : constant Ada.Streams.Stream_Element_Offset :=
                         Ada.Streams.Stream_Element_Offset (1024 * 256);
   --  Large buffer significantly improves speed - regardless of
   --  kind of working streams!  Use 256 kb as the default size.


   function compress (gzip : TGZip; data_string : String) return String;
   --  Returns a gzip-formatted compressed string given an standard string


   function compress (gzip : TGZip; data_string : SU.Unbounded_String)
   return SU.Unbounded_String;
   --  Returns a gzip-formatted compressed unbounded string given an
   --  unbounded string


   procedure compress (gzip              : in TGZip;
                       source_file       : in String;
                       erase_source_file : in Boolean := True);
   --  Creates a new compressed version of the given source file.  The new
   --  file will be put in the same directory and have the same name with
   --  the extension ".gz" appended to it.  By default, the source file will
   --  be deleted, but this can be overridden.


   procedure compress (gzip              : in TGZip;
                       source_file       : in String;
                       destination       : in String;
                       erase_source_file : in Boolean := True);
   --  Creates a new compressed version of the given source file.  The new
   --  file path (includes file name) is defined by the "destination" input.
   --  By default, the source file will be deleted, but this can be overridden.


   function decompress (gzip : TGZip; data_string : String) return String;
   --  Returns an uncompressed string given an gzip-formatted string


   function decompress (gzip : TGZip; data_string : SU.Unbounded_String)
   return SU.Unbounded_String;
   --  Returns an uncompressed unbounded string given an gzip-formatted
   --  unbounded string


   procedure decompress (gzip              : in TGZip;
                         source_file       : in String;
                         erase_source_file : in Boolean := True);
   --  Creates a new uncompressed version of the given source file.  The new
   --  file will be put in the same directory and have the same name with
   --  the extension ".gz" removed from it.  By default, the source file will
   --  be deleted, but this can be overridden.


   procedure decompress (gzip              : in TGZip;
                         source_file       : in String;
                         destination       : in String;
                         erase_source_file : in Boolean := True);
   --  Creates a new decompressed version of the given source file.  The new
   --  file path (includes file name) is defined by the "destination" input.
   --  By default, the source file will be deleted, but this can be overridden.


   procedure change_compression_level (
                  gzip      : in out TGZip;
                  new_level : in     TCompression_Levels);
   --  Changes compression from currently set level


   procedure change_encoding_stategy (
                  gzip         : in out TGZip;
                  new_strategy : in     TStrategy);
   --  Changes encoding strategy from currently set value.


   procedure change_flush_policy (
                  gzip       : in out TGZip;
                  new_policy : in     TFlush);
   --  Changes flush policy from currently set value.


   procedure change_gzip_suffix (
                  gzip       : in out TGZip;
                  new_suffix : in     String);
   --  Changes gzip suffix from currently set value.


   procedure change_buffer_size (
                  gzip            : in out TGZip;
                  new_buffer_size : in     Ada.Streams.Stream_Element_Offset);
   --  Changes buffer size from currently set value.


   function create (
                  compression_level : TCompression_Levels := 6;
                  strategy          : TStrategy := default_strategy;
                  flush_policy      : TFlush    := no_flush;
                  gz_suffix         : String    := STANDARD_GZIP_EXTENSION;
                  buffer_size       : Ada.Streams.Stream_Element_Offset  :=
                                      default_buffer_size)
   return TGZip;
   --  GZip object constructor


private

   type TGZip is tagged record
      compression_level : TCompression_Levels := 6;
      strategy          : TStrategy := default_strategy;
      flush_policy      : TFlush    := no_flush;
      gz_suffix         : SU.Unbounded_String :=
                          SU.To_Unbounded_String (STANDARD_GZIP_EXTENSION);
      buffer_size       : Ada.Streams.Stream_Element_Offset :=
                          default_buffer_size;
   end record;


   procedure low_level_compress (
                  gzip      : in TGZip;
                  in_stream : in TZipStream;
                  out_file  : in Binding_Zlib.gzFile);
   --  Compresses a stream of bytes into an open file descriptor.  All the
   --  variations of compressions eventually use this.


   procedure low_level_compress (
                  gzip                 : in TGZip;
                  source_filename      : in String;
                  destination_filename : in String);
   --  Compresses data from an existing file and put it into a gzip-formatted
   --  second file.

end Archive.GZip;
