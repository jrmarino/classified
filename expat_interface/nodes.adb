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


   ---------------------------
   --  set_element_content  --
   ---------------------------

   procedure set_element_content (Node    : in out TNode;
                                  content : SU.Unbounded_String)
   is
      use SU;
   begin
      if Node.Contents = SU.Null_Unbounded_String then
         Node.Contents := content;
      else
         Node.Contents := Node.Contents & content;
      end if;
   end set_element_content;



   ------------------------
   --  attr_insert_word  --
   ------------------------

   procedure attr_insert_word (word : in SU.Unbounded_String)
   is
   begin
      Node_Attributes.attr_insert_word (word => word);
   end attr_insert_word;



   -----------------------------
   --  attr_set_active_state  --
   -----------------------------

   procedure attr_set_active_state
   is
   begin
      Node_Attributes.attr_set_active_state;
   end attr_set_active_state;



   -----------------
   --  attr_dump  --
   -----------------

   procedure attr_dump
   is
   begin
      Node_Attributes.attr_dump;
   end attr_dump;


   ------------------------
   --  tags_insert_word  --
   ------------------------

   procedure tags_insert_word (word : in SU.Unbounded_String)
   is
   begin
      Node_Shared.tags.insert_word (word => word);
   end tags_insert_word;



   -----------------------------
   --  tags_set_active_state  --
   -----------------------------

   procedure tags_set_active_state
   is
   begin
      Node_Shared.tags.set_active_state;
   end tags_set_active_state;



   -----------------
   --  tags_dump  --
   -----------------

   procedure tags_dump
   is
   begin
      Node_Shared.tags.dump;
   end tags_dump;



   --------------------
   --  end_of_scope  --
   --------------------

   function end_of_scope (Node : TNode)
   return TNodeIndex is
   begin
      return Node.Scope;
   end end_of_scope;



   --------------------
   --  expand_scope  --
   --------------------

   procedure expand_scope (Node    : in out TNode;
                           Node_ID : in     TNodeIndex)
   is
   begin
      Node.Scope    := Node_ID;
      Node.Tag_Open := False;
   end expand_scope;



   -------------------
   --  tag_is_open  --
   -------------------

   function tag_is_open (Node : in TNode)
   return Boolean is
   begin
      return Node.Tag_Open;
   end tag_is_open;



   ----------------------
   --  set_basic_data  --
   ----------------------

   procedure set_basic_data (Node       : in out TNode;
                             Self_ID    : in     TNodeIndex;
                             Parent_ID  : in     TNodeIndex;
                             TagName    : in     SU.Unbounded_String;
                             Identifier : in     SU.Unbounded_String)
   --  This procedure is run once to establish basic node data.  The number of
   --  child nodes will be set to 0, the scope will be set to Self_ID, and the
   --  remaining relations will be set to defaults.  Attributes are cleared.
   is
   begin
      Node.ID                := Self_ID;
      Node.Parent_ID         := Parent_ID;
      Node.Next_Sibling_ID   := NO_FOLLOWING_BROTHER;
      Node.Prev_Sibling_ID   := NO_PREVIOUS_BROTHER;
      Node.First_Child_ID    := NO_OFFSPRING;
      Node.TagName           := Node_Shared.tags.get_index (word => TagName);
      Node.Identifier        := Identifier;
      Node.Num_Children      := 0;
      Node.Scope             := Self_ID;
      Node.Tag_Open          := True;
      Node.Contents          := SU.Null_Unbounded_String;
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
   --  signal_prev_brother  --
   ----------------------------

   procedure signal_prev_brother (Node       : in out TNode;
                                  Brother_ID : in     TNodeIndex)
   is
   begin
      Node.Prev_Sibling_ID := Brother_ID;
   end signal_prev_brother;



   ------------------------------
   --  signal_next_brother  --
   ------------------------------

   procedure signal_next_brother (Node       : in out TNode;
                                  Brother_ID : in     TNodeIndex)
   is
   begin
      Node.Next_Sibling_ID := Brother_ID;
   end signal_next_brother;



   ---------------------------
   --  attribute_clear_all  --
   ---------------------------

   procedure attribute_clear_all (Node : in out TNode) is
   begin
      Node.Attributes.clear;
   end attribute_clear_all;



   -------------------------
   --  clear_shared_data  --
   -------------------------

   procedure clear_shared_data is
   begin
      Node_Shared.tags.clear;
      Node_Attributes.clear_shared_attributes;
   end clear_shared_data;


   ---------------------
   --  attribute_add  --
   ---------------------

   procedure attribute_add (Node  : in out TNode;
                            key   : in     SU.Unbounded_String;
                            value : in     SU.Unbounded_String) is
   begin
      Node.Attributes.add_attribute (key => key, value => value);
   end attribute_add;



   -----------------------------------
   --  attribute_value (version 1)  --
   -----------------------------------

   function attribute_value (Node : TNode; index : Positive)
   return SU.Unbounded_String is
   begin
      return Node.Attributes.value (index => index);
   end attribute_value;



   ----------------------------------
   --  attribute_value (version 2) --
   ----------------------------------

   function attribute_value (Node : TNode; key : String)
   return SU.Unbounded_String is
   begin
      return Node.Attributes.value (key => key);
   end attribute_value;



   ------------------------
   --  attributes_index  --
   ------------------------

   function attribute_index (Node : TNode;
                             key  : String)
   return Natural is
   begin
      return Node.Attributes.index (key => key);
   end attribute_index;



   ---------------------
   --  attribute_key  --
   ---------------------

   function attribute_key (Node  : TNode;
                           index : Positive)
   return SU.Unbounded_String is
   begin
      return Node.Attributes.key (index => index);
   end attribute_key;


   ----------
   --  id  --
   ----------

   function id (Node : TNode) return SU.Unbounded_String is
   begin
      return Node.Identifier;
   end id;



   ----------------
   --  tag_name  --
   ----------------

   function tag_name (Node : TNode) return SU.Unbounded_String is
   begin
      return Node_Shared.tags.get_text (index => Node.TagName);
   end tag_name;



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



   -------------------------
   --  first_child_index  --
   -------------------------

   function first_child_index (Node : TNode)
   return TNodeIndex is
   begin
      return Node.First_Child_ID;
   end first_child_index;



   -------------------------------
   --  next_sibling_node_index  --
   -------------------------------

   function next_sibling_node_index (Node : TNode)
   return TNodeIndex is
   begin
      return Node.Next_Sibling_ID;
   end next_sibling_node_index;



   -----------------------------------
   --  previous_sibling_node_index  --
   -----------------------------------

   function previous_sibling_node_index (Node : TNode)
   return TNodeIndex is
   begin
      return Node.Prev_Sibling_ID;
   end previous_sibling_node_index;



   -------------------------
   --  parent_node_index  --
   -------------------------

   function parent_node_index (Node : TNode)
   return TNodeIndex is
   begin
      return Node.Parent_ID;
   end parent_node_index;



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
