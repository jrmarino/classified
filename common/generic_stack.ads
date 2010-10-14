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

generic

   type TItem is private;
   DEPLETED_STACK_ERROR : TItem;

package Generic_Stack is

   type TStack is tagged limited private;

   procedure clear (stack :    out TStack);
   procedure push  (stack : in out TStack; item :  in TItem);
   procedure pop   (stack : in out TStack);

   function  peek  (stack : TStack) return TItem;

private

   type RecData;
   type Acc_RecData is access all RecData;

   type RecData is record
      value : TItem;
      next  : Acc_RecData := null;
   end record;

   type TStack is new Ada.Finalization.Limited_Controlled with
      record
         LIFO : Acc_RecData := null;
      end record;

   overriding
   procedure Finalize (stack : in out TStack);

   procedure Free is
      new Ada.Unchecked_Deallocation (RecData, Acc_RecData);

   function is_empty (stack : TStack) return Boolean;

end Generic_Stack;
