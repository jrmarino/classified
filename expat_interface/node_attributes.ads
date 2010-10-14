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


with Ada.Finalization;
with Ada.Strings.Unbounded;
with Ada.Unchecked_Deallocation;

package Node_Attributes is

   package SU renames Ada.Strings.Unbounded;

   type TAttributes is tagged limited private;
   type Acc_Attributes is access TAttributes;


   procedure clear (Attributes : in out TAttributes);
   --  Deletes the list of attributes and frees the memory it consumed


   procedure add_attribute (Attributes : in out TAttributes;
                            key        : in     SU.Unbounded_String;
                            value      : in     SU.Unbounded_String);
   --  Creates memory for the attribute record and appends the record to the
   --  end of the internal list.  A key that is a duplicate of a previous
   --  attribute will not cause the previous attribute to be updated.  The
   --  duplicate would be allowed.


   function value (Attributes : TAttributes;
                   index      : Positive) return SU.Unbounded_String;
   --  Returns the attribute value given its index.  The index starts at 1
   --  If the index is out of range, a blank string is returned.


   function key (Attributes : TAttributes;
                 index      : Positive) return SU.Unbounded_String;
   --  Returns the attribute key given its index.  The index starts at 1
   --  If the index is out of range, a blank string is returned.


   function value (Attributes : TAttributes;
                   key        : SU.Unbounded_String)
   return SU.Unbounded_String;
   --  Returns the attribute value given its key string.   If the key doesn't
   --  exist, a blank string is returned.


   function index (Attributes : TAttributes;
                   key        : SU.Unbounded_String) return Natural;
   --  Returns the index of an attribute given a key string.  If the key wasn't
   --  previously defined, a zero is returned.


   function count (Attributes : TAttributes) return Natural;
   --  Returns the number of attributes currently defined within the set.


private
   type RecAttribute;
   type Acc_RecAttribute is access all RecAttribute;

   type RecAttribute is record
      key   : SU.Unbounded_String := SU.Null_Unbounded_String;
      value : SU.Unbounded_String := SU.Null_Unbounded_String;
      next  : Acc_RecAttribute;
   end record;

   type TAttributes is new Ada.Finalization.Limited_Controlled with
      record
         LIFO : Acc_RecAttribute := null;
      end record;

   overriding
   procedure Finalize (Attributes : in out TAttributes);

   procedure Free is
      new Ada.Unchecked_Deallocation (RecAttribute, Acc_RecAttribute);

end Node_Attributes;
