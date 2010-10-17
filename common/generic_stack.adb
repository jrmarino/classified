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


with Ada.Exceptions;
with Ada.Text_IO;

package body Generic_Stack is


   ------------
   --  peek  --
   ------------

   function peek  (stack : TStack)
   return TItem is
   begin
      if stack.LIFO = null then
         return DEPLETED_STACK_ERROR;
      end if;
      return stack.LIFO.value;
   end peek;



   ------------
   --  push  --
   ------------

   procedure push  (stack : in out TStack; item :  in TItem)
   is
   begin
      stack.LIFO := new RecData'(value => item, next => stack.LIFO);
   end push;



   -----------
   --  pop  --
   -----------

   procedure pop (stack : in out TStack)
   is
      old_stack : Acc_RecData := stack.LIFO;
   begin
      if stack.is_empty then
         return;
      end if;

      stack.LIFO := stack.LIFO.next;

      Free (old_stack);
      if old_stack = null then
         null;  --  to silence useless assignment warning
      end if;
   end pop;



   ----------------------
   --  stack_is_empty  --
   ----------------------

   function is_empty (stack : TStack) return Boolean
   is
   begin
      declare
         Empty_Stack : exception;
      begin
         if stack.LIFO = null then
            Ada.Exceptions.Raise_Exception (Empty_Stack'Identity,
               "ERROR: Attempted to pop an empty stack.");
         end if;
      exception
         when Fail : Empty_Stack =>
            Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Message (Fail));
            return True;
      end;
      return False;
   end is_empty;



   -------------
   --  clear  --
   -------------

   procedure clear (stack : out TStack)
   is
   begin
      while stack.LIFO /= null loop
         stack.pop;
      end loop;
      stack.LIFO := null;
   end clear;



   ----------------
   --  Finalize  --
   ----------------

   overriding
   procedure Finalize (stack : in out TStack)
   is
   begin
      stack.clear;
   end Finalize;


end Generic_Stack;
