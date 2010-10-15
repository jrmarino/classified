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
with Ada.Unchecked_Deallocation;
with Ada.Strings.Unbounded;

package Condensed_Text is

   package SU renames Ada.Strings.Unbounded;
   type TTextPacker is tagged limited private;

   procedure insert_word (TextPacker : in out TTextPacker;
                          word       : in     SU.Unbounded_String);
   --  This procedure will first search the data base to see if the word
   --  exists already, and if not it will be placed in the list.  It is not
   --  placed at the end, but in alphabetical order.  The package must be
   --  in the "construction" state for this to work.  Nothing will be done
   --  if the package isn't in this state.


   procedure clear (TextPacker : in out TTextPacker);
   --  This procedure will wipe the database and set the package back to
   --  the "construction" state.


   function get_index (TextPacker : TTextPacker;
                       word       : SU.Unbounded_String) return Natural;
   --  The procedure will accept text and then do a binary search on the
   --  database and returns the "index" of the word (1+).  If the word is not
   --  found, then zero is returned.  This is considered misuse because we
   --  intend that all words are pre-entered during the construction phase.
   --  This function returns 0 always if the package is not in "active" state.


   function get_text (TextPacker : TTextPacker;
                      index      : Positive) return SU.Unbounded_String;
   --  This function accepts a positive index and returns the associated
   --  text.  If the index is out of range, a null string will be returned.
   --  Again, this is not operationally expected.


   procedure set_active_state (TextPacker : in out TTextPacker);
   --  This procedures internally changes the package from "construction" to
   --  "active", allowing text to be quickly retrieved.


   procedure dump (TextPacker : in TTextPacker);
   --  Used for development and debug purposes only.

private

   type RecData;
   type Acc_RecData is access all RecData;

   type RecData is record
      value : SU.Unbounded_String := SU.Null_Unbounded_String;
      next  : Acc_RecData := null;
   end record;

   type TDataArray is array (Positive range <>) of Acc_RecData;
   type Acc_DataArray is access all TDataArray;

   type TState is (active, construction);
   type TFound is (search_lower, nailed_it, search_higher);

   type TTextPacker is new Ada.Finalization.Limited_Controlled with
      record
         Linear   : Acc_DataArray := null;
         List     : Acc_RecData   := null;
         ListSize : Natural       := 0;
         State    : TState        := construction;
      end record;

   overriding
   procedure Finalize (TextPacker : in out TTextPacker);


   procedure FreeList is
      new Ada.Unchecked_Deallocation (RecData, Acc_RecData);


   procedure FreeLinear is
      new Ada.Unchecked_Deallocation (TDataArray, Acc_DataArray);


   function standard_binary_search (TextPacker : TTextPacker;
                                    Candidate  : SU.Unbounded_String)
   return Natural;
   --  This is a standard binary search that is run on the "linear" database
   --  when the package is active.  If the "Candidate" is found, the positive
   --  index is returned, otherwise zero is returned.  Under normal use, the
   --  candidate is always found.


   procedure linked_list_brute_search (TextPacker : in TTextPacker;
                                       Candidate  : in SU.Unbounded_String;
                                       found      : out Boolean;
                                       insert_at  : out Positive);
   --  This is a brute-force search that is used during the scanning phase.
   --  If the found result comes back False, the "insert_at" result will
   --  give the instruction where to add the new word.  It basically scans the
   --  alphabetical list, ejecting when the Candidate is higher than the
   --  scanned element, when the candidate is found, or when the end of the
   --  alphabetical list is reached.  Previously this was attempted to be
   --  a binary search, but it never worked right, and the amount of pointer
   --  hopping to transverse the list negated the benefits.  It probably
   --  would have required a double-linked list to be superior.


end Condensed_Text;
