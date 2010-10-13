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


package body Node_Attributes is

   -------------
   --  clear  --
   -------------

   procedure clear (Attributes : in out TAttributes)
   is
      doomed : Acc_RecAttribute;
   begin
      while Attributes.LIFO /= null loop
         doomed := Attributes.LIFO;
         Attributes.LIFO := Attributes.LIFO.next;
         Free (doomed);
      end loop;
      Attributes.LIFO := null;
   end clear;



   -------------
   --  count  --
   -------------

   function count (Attributes : TAttributes)
   return Natural is
      arrow  : Acc_RecAttribute := Attributes.LIFO;
      result : Natural := 0;
   begin
      while arrow /= null loop
         result := result + 1;
         arrow := arrow.next;
      end loop;
      return result;
   end count;



   ----------------
   --  Finalize  --
   ----------------

   overriding
   procedure Finalize (Attributes : in out TAttributes)
   is
   begin
      Attributes.clear;
   end Finalize;



   --------------------------
   --  value  (version 1)  --
   --------------------------

   function value (Attributes : TAttributes;
                   index      : Positive)
   return SU.Unbounded_String is
      arrow       : Acc_RecAttribute := Attributes.LIFO;
      arrow_index : Natural := 0;
      result      : SU.Unbounded_String := SU.Null_Unbounded_String;

      use SU;
   begin
      while result = SU.Null_Unbounded_String
            and then arrow /= null loop
         arrow_index := arrow_index + 1;
         if arrow_index = index then
            result := arrow.value;
         end if;
         arrow := arrow.next;
      end loop;
      return result;
   end value;



   --------------------------
   --  value  (version 2)  --
   --------------------------

   function value (Attributes : TAttributes;
                   key        : SU.Unbounded_String)
   return SU.Unbounded_String is
      arrow  : Acc_RecAttribute    := Attributes.LIFO;
      result : SU.Unbounded_String := SU.Null_Unbounded_String;

      use SU;
   begin
      while result = SU.Null_Unbounded_String
            and then arrow /= null loop
         if arrow.key = key then
            result := arrow.value;
         end if;
         arrow := arrow.next;
      end loop;
      return result;
   end value;



   -------------
   --  index  --
   -------------

   function index (Attributes : TAttributes;
                   key        : SU.Unbounded_String)
   return Natural is
      arrow       : Acc_RecAttribute := Attributes.LIFO;
      arrow_index : Natural := 0;
      result      : Natural := 0;

      use SU;
   begin
      while result = 0 and then arrow /= null loop
         arrow_index := arrow_index + 1;
         if arrow.key = key then
            result := arrow_index;
         end if;
         arrow := arrow.next;
      end loop;
      return result;
   end index;


   ---------------------
   --  add_attribute  --
   ---------------------

   procedure add_attribute (Attributes : in out TAttributes;
                            key        : in     SU.Unbounded_String;
                            value      : in     SU.Unbounded_String)
   is
   begin
      Attributes.LIFO := new RecAttribute'(
                              key   => key,
                              value => value,
                              next  => Attributes.LIFO);
   end add_attribute;


end Node_Attributes;
