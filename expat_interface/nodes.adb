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


package body Nodes is


   --------------------
   --  expand_scope  --
   --------------------

   procedure expand_scope (Node    : in out TNode;
                           Node_ID : in     TNodeIndex)
   is
   begin
      Node.Scope := Node_ID;
   end expand_scope;



   ----------------------
   --  set_basic_data  --
   ----------------------

   procedure set_basic_data (Node       : in out TNode;
                             Self_ID    : in     TNodeIndex;
                             Parent_ID  : in     TNodeIndex;
                             Value      : in     SU.Unbounded_String;
                             Identifier : in     SU.Unbounded_String)
   --  This procedure is run once to establish basic node data.  The number of
   --  child nodes will be set to 0, the scope will be set to Self_ID, and the
   --  remaining relations will be set to the defaults.  Attributes are cleared.
   is
   begin
      Node.ID                := Self_ID;
      Node.Parent_ID         := Parent_ID;
      Node.Next_Sibling_ID   := NO_FOLLOWING_BROTHER;
      Node.Prev_Sibling_ID   := NO_PREVIOUS_BROTHER;
      Node.First_Child_ID    := NO_OFFSPRING;
      Node.Value             := Value;
      Node.Identifier        := Identifier;
      Node.Num_Children      := 0;
      Node.Scope             := Self_ID;
      Node.Attributes.clear;
   end set_basic_data;



   ---------------------
   --  signal_parent  --
   ---------------------

   procedure signal_parent (Node     : in out TNode;
                            Child_ID : in     TNodeIndex)
   is
   begin
      Node.Num_Children := Node.Num_Children + 1;
      if Node.First_Child_ID = NO_OFFSPRING then
         Node.First_Child_ID := Child_ID;
      end if;
   end signal_parent;



   ----------------------------
   --  signal_older_brother  --
   ----------------------------

   procedure signal_older_brother (Node       : in out TNode;
                                   Brother_ID : in     TNodeIndex)
   is
   begin
      Node.Prev_Sibling_ID := Brother_ID;
   end signal_older_brother;



   ------------------------------
   --  signal_younger_brother  --
   ------------------------------

   procedure signal_younger_brother (Node       : in out TNode;
                                     Brother_ID : in     TNodeIndex)
   is
   begin
      Node.Next_Sibling_ID := Brother_ID;
   end signal_younger_brother;



   ---------------------------
   --  attribute_clear_all  --
   ---------------------------

   procedure attribute_clear_all (Node : in out TNode) is
   begin
      Node.Attributes.clear;
   end attribute_clear_all;



   ---------------------
   --  attribute_add  --
   ---------------------

   procedure attribute_add (Node  : in out TNode;
                            key   : in     SU.Unbounded_String;
                            value : in     SU.Unbounded_String) is
   begin
      Node.Attributes.add_attribute (key => key, value => value);
   end attribute_add;



   -------------------------
   --  value (version 1)  --
   -------------------------

   function attribute_value (Node : TNode; index : Positive)
   return SU.Unbounded_String is
   begin
      return Node.Attributes.value (index => index);
   end attribute_value;



   ----------------------------------
   --  attribute_value (version 2) --
   ----------------------------------

   function attribute_value (Node : TNode; key : SU.Unbounded_String)
   return SU.Unbounded_String is
   begin
      return Node.Attributes.value (key => key);
   end attribute_value;



   ------------------------
   --  attributes_index  --
   ------------------------

   function attribute_index (Node : TNode;
                             key  : SU.Unbounded_String)
   return Natural is
   begin
      return Node.Attributes.index (key => key);
   end attribute_index;



   ----------
   --  id  --
   ----------

   function id (Node : TNode) return SU.Unbounded_String is
   begin
      return Node.Identifier;
   end id;



   -------------
   --  index  --
   -------------

   function index (Node : TNode)
   return TNodeIndex is
   begin
      return Node.ID;
   end index;



   ------------------------
   --  child_node_count  --
   ------------------------

   function child_node_count (Node : TNode)
   return Natural is
   begin
      return Node.Num_Children;
   end child_node_count;



   -----------------------
   --  attribute_count  --
   -----------------------

   function attribute_count (Node : TNode)
   return Natural is
   begin
      return Node.Attributes.count;
   end attribute_count;



   -------------------
   --  first_child  --
   -------------------

   function first_child (Node : TNode)
   return TNodeIndex is
   begin
      return Node.First_Child_ID;
   end first_child;



   -------------------------
   --  next_sibling_node  --
   -------------------------

   function next_sibling_node (Node : TNode)
   return TNodeIndex is
   begin
      return Node.Next_Sibling_ID;
   end next_sibling_node;



   -----------------------------
   --  previous_sibling_node  --
   -----------------------------

   function previous_sibling_node (Node : TNode)
   return TNodeIndex is
   begin
      return Node.Prev_Sibling_ID;
   end previous_sibling_node;

   -------------------
   --  parent_node  --
   -------------------

   function parent_node (Node : TNode)
   return TNodeIndex is
   begin
      return Node.Parent_ID;
   end parent_node;



   -----------------------
   --  has_child_nodes  --
   -----------------------

   function has_child_nodes (Node : TNode)
   return Boolean is
      result : constant Boolean := Node.Num_Children > 0;
   begin
      return result;
   end has_child_nodes;



   -------------------------
   --  has_sibling_nodes  --
   -------------------------

   function has_sibling_nodes (Node : TNode)
   return Boolean is
      no_brothers : constant Boolean :=
                     (Node.Prev_Sibling_ID = NO_PREVIOUS_BROTHER) and then
                     (Node.Next_Sibling_ID = NO_FOLLOWING_BROTHER);
      result : constant Boolean := no_brothers = False;
   begin
      return result;
   end has_sibling_nodes;



   ----------------------
   --  has_attributes  --
   ----------------------

   function has_attributes (Node : TNode)
   return Boolean is
      result : constant Boolean := Node.Attributes.count > 0;
   begin
      return result;
   end has_attributes;



end Nodes;
