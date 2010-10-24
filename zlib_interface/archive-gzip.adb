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
with Ada.Exceptions;
with Ada.Directories;
with Ada.Strings.Fixed;

package body Archive.GZip is



   function compress (gzip : TGZip; data_string : String) return String
   is
   begin
   --  Returns a gzip-formatted compressed string given an standard string
      return "";
   end compress;


   function compress (gzip : TGZip; data_string : SU.Unbounded_String)
   return SU.Unbounded_String is
   begin
   --  Returns a gzip-formatted compressed unbounded string given an
   --  unbounded string
      return SU.Null_Unbounded_String;
   end compress;


   function decompress (gzip : TGZip; data_string : String)
   return String is
   begin
   --  Returns an uncompressed string given an gzip-formatted string
      return "";
   end decompress;


   function decompress (gzip : TGZip; data_string : SU.Unbounded_String)
   return SU.Unbounded_String is
   begin
   --  Returns an uncompressed unbounded string given an gzip-formatted
   --  unbounded string
      return SU.Null_Unbounded_String;
   end decompress;


   procedure decompress (gzip              : in TGZip;
                         source_file       : in String;
                         erase_source_file : in Boolean := True)
   is
   begin
   --  Creates a new uncompressed version of the given source file.  The new
   --  file will be put in the same directory and have the same name with
   --  the extension ".gz" removed from it.  By default, the source file will
   --  be deleted, but this can be overridden.
      null;
   end decompress;


   procedure decompress (gzip              : in TGZip;
                         source_file       : in String;
                         destination       : in String;
                         erase_source_file : in Boolean := True)
   is
   begin
   --  Creates a new decompressed version of the given source file.  The new
   --  file path (includes file name) is defined by the "destination" input.
   --  By default, the source file will be deleted, but this can be overridden.
      null;
   end decompress;


















   ---------------------------
   --  compress (version 2) --
   ---------------------------

   procedure compress (gzip              : in TGZip;
                       source_file       : in String;
                       erase_source_file : in Boolean := True)
   is
      dest_filename : constant String :=
                      source_file & SU.To_String (gzip.gz_suffix);
   begin
      gzip.compress (
            source_file       => source_file,
            destination       => dest_filename,
            erase_source_file => erase_source_file);
   end compress;


   ---------------------------
   --  compress (version 1) --
   ---------------------------

   procedure compress (gzip              : in TGZip;
                       source_file       : in String;
                       destination       : in String;
                       erase_source_file : in Boolean := True)
   is
   begin
      if not Ada.Directories.Exists (Name => source_file) then
         Ada.Text_IO.Put_Line ("Compress(File): File does not exist.");
         return;
      end if;

      gzip.low_level_compress (source_filename      => source_file,
                               destination_filename => destination);

      if erase_source_file then
         Ada.Directories.Delete_File (Name => source_file);
      end if;

   exception

      when Ada.Directories.Name_Error => Ada.Text_IO.Put_Line (
                         "Compress(File): Illegal file name " & source_file);
      when Ada.Directories.Use_Error  => Ada.Text_IO.Put_Line (
                         "Compress(File): Failed to delete " & source_file);
   end compress;


   ------------------------------------
   --  low_level_compress version 2  --
   ------------------------------------

   procedure low_level_compress (
                  gzip                 : in TGZip;
                  source_filename      : in String;
                  destination_filename : in String)
   is
      mode : constant String := 'w' & Ada.Strings.Fixed.Trim (
               Source => Integer'Image (Integer (gzip.compression_level)),
               Side   => Ada.Strings.Left);
      outgzfile      : Binding_Zlib.gzFile;
      FileHandle     : ASIO.File_Type;
      source_stream  : TZipStream;
      closure        : Binding_Zlib.intf;
      Gzopen_Failure : exception;
      Stream_Failure : exception;
      File_Failure   : exception;
      use type Binding_Zlib.voidp;
   begin
      outgzfile := Binding_Zlib.gzopen (
                     path => ICS.New_String (destination_filename),
                     mode => ICS.New_String (mode));
      if outgzfile = Binding_Zlib.nullp then
         Ada.Exceptions.Raise_Exception (
               E       => Gzopen_Failure'Identity,
               Message => "Failed to gzopen " & source_filename);
      end if;
      ASIO.Open (
            File => FileHandle,
            Mode => ASIO.In_File,
            Name => source_filename);

      source_stream := ASIO.Stream (FileHandle);
      gzip.low_level_compress (in_stream => source_stream,
                               out_file  => outgzfile);
      closure := Binding_Zlib.gzclose_w (file => outgzfile);
      if Integer (closure) /= Integer (Binding_Zlib.Z_OK) then
         case closure is
            when Binding_Zlib.Z_STREAM_ERROR =>
                  Ada.Exceptions.Raise_Exception (
                     E       => Stream_Failure'Identity,
                     Message => "gzclose stream failure");
            when Binding_Zlib.Z_ERRNO =>
                  Ada.Exceptions.Raise_Exception (
                     E       => File_Failure'Identity,
                     Message => "gzclose file operation error");
            when others => null;
         end case;
      end if;
      ASIO.Close (File => FileHandle);
      if ASIO.Is_Open (File => FileHandle) then
         null;   --  to silence compiler about unreferenced FileHandle
      end if;

   exception

      when Fail : Gzopen_Failure |
                  Stream_Failure |
                  File_Failure => Ada.Text_IO.Put_Line (
                                   "Compress(Stream->Stream): " &
                                   Ada.Exceptions.Exception_Message (Fail));

   end low_level_compress;



   --------------------------------------
   --  low_level_compress (version 1)  --
   --------------------------------------

   procedure low_level_compress (
                  gzip      : in TGZip;
                  in_stream : in TZipStream;
                  out_file  : in Binding_Zlib.gzFile)
   is
      use Ada.Streams;

      buffer     : Stream_Element_Array (1 .. gzip.buffer_size);
      caboose    : Stream_Element_Offset;

      --  when nothing is read, Last because buffer'First - 1 = 0
      terminated : constant Stream_Element_Offset := Stream_Element_Offset (
                       Integer (buffer'First) - 1);
      writelen   : Binding_Zlib.intf;
      PtrBuffer  : constant Binding_Zlib.voidpc :=
                            Binding_Zlib.voidpc (buffer'Address);
      Write_Failure : exception;
   begin

      Rebecca :
         loop
            Ada.Streams.Read (
                  Stream => in_stream.all,
                  Item   => buffer,
                  Last   => caboose);
            exit Rebecca when caboose = terminated;
            writelen := Binding_Zlib.gzwrite (
                           file => out_file,
                           buf  => PtrBuffer,
                           len  => IC.unsigned (caboose));
            if Stream_Element_Offset (writelen) /= caboose then
               declare
                  error_code : aliased Binding_Zlib.intf;
                  msg : constant String := ICS.Value (
                                 Binding_Zlib.gzerror (
                                    file   => out_file,
                                    errnum => error_code'Access));
               begin
                  Ada.Exceptions.Raise_Exception (
                     E       => Write_Failure'Identity,
                     Message => "gzwrite error: " & msg);
               end;
            end if;

         end loop Rebecca;

   exception

      when Fail : Write_Failure => Ada.Text_IO.Put_Line (
                                   "Compress(Stream->gzFile): " &
                                   Ada.Exceptions.Exception_Message (Fail));

   end low_level_compress;



   --------------
   --  create  --
   --------------

   function create (
                  compression_level : TCompression_Levels := 6;
                  strategy          : TStrategy := default_strategy;
                  flush_policy      : TFlush    := no_flush;
                  gz_suffix         : String    := STANDARD_GZIP_EXTENSION;
                  buffer_size       : Ada.Streams.Stream_Element_Offset :=
                                      default_buffer_size)
   return TGZip is
      result : TGZip;
   begin
      result.compression_level := compression_level;
      result.strategy          := strategy;
      result.flush_policy      := flush_policy;
      result.gz_suffix         := SU.To_Unbounded_String (gz_suffix);
      result.buffer_size       := buffer_size;

      return result;
   end create;



   --------------------------------
   --  change_compression_level  --
   --------------------------------

   procedure change_compression_level (
                  gzip      : in out TGZip;
                  new_level : in     TCompression_Levels) is
   begin
      gzip.compression_level := new_level;
   end change_compression_level;



   -------------------------------
   --  change_encoding_stategy  --
   -------------------------------

   procedure change_encoding_stategy (
                  gzip         : in out TGZip;
                  new_strategy : in     TStrategy) is
   begin
      gzip.strategy := new_strategy;
   end change_encoding_stategy;



   ---------------------------
   --  change_flush_policy  --
   ---------------------------

   procedure change_flush_policy (
                  gzip       : in out TGZip;
                  new_policy : in     TFlush)
   is
   begin
      gzip.flush_policy := new_policy;
   end change_flush_policy;



   --------------------------
   --  change_gzip_suffix  --
   --------------------------

   procedure change_gzip_suffix (
                  gzip       : in out TGZip;
                  new_suffix : in     String)
   is
   begin
      gzip.gz_suffix := SU.To_Unbounded_String (new_suffix);
   end change_gzip_suffix;



   --------------------------
   --  change_buffer_size  --
   --------------------------

   procedure change_buffer_size (
                  gzip            : in out TGZip;
                  new_buffer_size : in     Ada.Streams.Stream_Element_Offset)
   is
   begin
      gzip.buffer_size := new_buffer_size;
   end change_buffer_size;


end Archive.GZip;
