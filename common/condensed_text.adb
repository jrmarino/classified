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


with Ada.Text_IO; use Ada.Text_IO;

package body Condensed_Text is


   -----------
   --  dump --
   -----------

   procedure dump (TextPacker : in TTextPacker)
   is
      sum : Natural := 0;
   begin
      if TextPacker.State = construction then
         return;
      end if;
      for x in Positive range 1 .. TextPacker.ListSize loop
         sum := sum + SU.Length (TextPacker.Linear (x).value);
         Put_Line (Integer'Image (x) & " " &
                   SU.To_String (TextPacker.Linear (x).value));
      end loop;
      Put_Line ("Total Bytes:" & Integer'Image (sum));
   end dump;


   ---------------------------------
   --  linked_list_binary_search  --
   ---------------------------------

   procedure linked_list_brute_search (TextPacker : in TTextPacker;
                                        Candidate  : in SU.Unbounded_String;
                                        found      : out Boolean;
                                        insert_at  : out Positive)
   is
      LS            : constant Natural := TextPacker.ListSize;
      arrow         : Acc_RecData;
      search_result : TFound;

      function found_list (index : Positive; word : SU.Unbounded_String)
      return TFound;

      function found_list (index : Positive; word : SU.Unbounded_String)
      return TFound is
         indexed : SU.Unbounded_String := SU.Null_Unbounded_String;
         use SU;
      begin
         --  We only increment by one since the arrow is already in place
         --  from the previous run.
         if index > 1 then
            arrow := arrow.next;
         end if;
         indexed := arrow.value;

         if indexed = word then
            return nailed_it;
         end if;
         if word > indexed then
            return search_higher;
         else
            return search_lower;
         end if;
      end found_list;
   begin
      found     := True;
      insert_at := Positive'Last;
      if TextPacker.State = active or else LS = 0 then
         return;
      end if;


      arrow := TextPacker.List;
      for x in Positive range 1 .. LS loop
         search_result := found_list (index => x, word => Candidate);
         if search_result = nailed_it then
            return;
         end if;
         if search_result = search_lower then
            found     := False;
            insert_at := x;
            return;
         end if;
      end loop;
      found     := False;
      insert_at := TextPacker.ListSize + 1;

   end linked_list_brute_search;



   -------------------
   --  insert_word  --
   -------------------

   procedure insert_word (TextPacker : in out TTextPacker;
                          word       : in     SU.Unbounded_String)
   is
      found : Boolean;
      index : Positive;
   begin
      if TextPacker.State = active then
         return;
      end if;
      if TextPacker.ListSize = 0 then
         TextPacker.List := new RecData'(value => word, next => null);
         TextPacker.ListSize := 1;
         return;
      end if;

      TextPacker.linked_list_brute_search (Candidate => word,
                                           found     => found,
                                           insert_at => index);
      if found then
         return;
      end if;

      --  The word isn't on the list yet, so we need to insert it into a
      --  specific slot to keep it alphabetized.

      declare
         new_word : constant Acc_RecData :=
                    new RecData'(value => word, next => null);
         data     : Acc_RecData := TextPacker.List;
      begin
         if index = 1 then
            new_word.next   := TextPacker.List;
            TextPacker.List := new_word;
         else
            for x in Positive range 2 .. index - 1 loop
               data := data.next;
            end loop;
            new_word.next   := data.next;
            data.next       := new_word;
         end if;
         TextPacker.ListSize := TextPacker.ListSize + 1;
      end;

   end insert_word;



   ------------------------------
   --  standard_binary_search  --
   ------------------------------

   function standard_binary_search (TextPacker : TTextPacker;
                                    Candidate  : SU.Unbounded_String)
   return Natural is
      LS            : constant Natural := TextPacker.ListSize;
      midpoint      : Natural := (LS + 1) / 2;
      lowpoint      : Natural := 1;
      maxpoint      : Natural := LS;
      search_result : TFound;

      function found (index : Positive; word  : SU.Unbounded_String)
      return TFound;

      function found (index : Positive; word  : SU.Unbounded_String)
      return TFound is
         indexed : constant SU.Unbounded_String :=
                            TextPacker.Linear (index).value;
         use SU;
      begin
         if indexed = word then
            return nailed_it;
         end if;
         if word > indexed then
            return search_higher;
         else
            return search_lower;
         end if;
      end found;
   begin
      if TextPacker.State = construction or else LS = 0 then
         return 0;
      end if;

      Kathy :
         loop
            search_result := found (index => midpoint, word => Candidate);
            if search_result = nailed_it then
               return midpoint;
            end if;
            if search_result = search_higher then
               lowpoint := midpoint + 1;
            else
               maxpoint := midpoint - 1;
            end if;
            exit Kathy when maxpoint - lowpoint < 2;
            midpoint := lowpoint + ((maxpoint - lowpoint) / 2);
         end loop Kathy;

      --  If we get here, the word hasn't been found yet.
      search_result := found (index => lowpoint, word => Candidate);
      if search_result = nailed_it then
         return lowpoint;
      end if;
      if maxpoint /= lowpoint then
         search_result := found (index => maxpoint, word => Candidate);
         if search_result = nailed_it then
            return maxpoint;
         end if;
      end if;

      return 0;  --  No match was found.

   end standard_binary_search;



   -----------------
   --  get_index  --
   -----------------

   function get_index (TextPacker : TTextPacker;
                       word       : SU.Unbounded_String)
   return Natural is
   begin

      pragma Assert (
         Check   => TextPacker.State = active,
         Message => "CT::get_index used during construction state.");

      return TextPacker.standard_binary_search (Candidate  => word);

   end get_index;



   ----------------
   --  get_text  --
   ----------------

   function get_text (TextPacker : TTextPacker;
                      index      : Positive)
   return SU.Unbounded_String is
   begin
      if TextPacker.State = construction then
         return SU.Null_Unbounded_String;
      end if;
      if index > TextPacker.ListSize then
         return SU.Null_Unbounded_String;
      end if;

      return TextPacker.Linear (index).value;

   end get_text;



   ------------------------
   --  set_active_state  --
   ------------------------

   procedure set_active_state (TextPacker : in out TTextPacker)
   is
   begin

      TextPacker.State  := active;
      if TextPacker.ListSize < 1 then
         return;
      end if;

      TextPacker.Linear := new TDataArray (1 .. TextPacker.ListSize);
      TextPacker.Linear (1) := TextPacker.List;
      for x in Positive range 2 .. TextPacker.ListSize loop
         TextPacker.Linear (x) := TextPacker.Linear (x - 1).next;
      end loop;

   end set_active_state;



   -------------
   --  clear  --
   -------------

   procedure clear (TextPacker : in out TTextPacker)
   is
   begin
      FreeList   (TextPacker.List);
      FreeLinear (TextPacker.Linear);

      TextPacker.State    := construction;
      TextPacker.ListSize := 0;
      TextPacker.List     := null;
      TextPacker.Linear   := null;

   end clear;



   ----------------
   --  Finalize  --
   ----------------

   overriding
   procedure Finalize (TextPacker : in out TTextPacker)
   is
   begin
      TextPacker.clear;
   end Finalize;


end Condensed_Text;
