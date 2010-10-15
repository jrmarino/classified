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


with Condensed_Text;
with Node_Attributes; use Node_Attributes;

package Nodes is

   NO_INDEX_DEFINED     : constant Integer :=  0;
   NO_PARENT            : constant Integer := -1;  --  root only
   NO_PREVIOUS_BROTHER  : constant Integer := -2;
   NO_FOLLOWING_BROTHER : constant Integer := -3;
   NO_OFFSPRING         : constant Integer := -4;
   CHILD_RANGE_EXCEEDED : constant Integer := -5;
   NODE_RANGE_EXCEEDED  : constant Integer := -6;
   POPPED_EMPTY_STACK   : constant Integer := -7;  -- always last

   subtype TNodeIndex is Integer range POPPED_EMPTY_STACK .. Integer'Last;
   type TNode is tagged limited private;

   function has_attributes (Node : TNode) return Boolean;
   --  Returns True if node has defined attributes.


   function has_child_nodes (Node : TNode) return Boolean;
   --  Returns True if node has any children.


   function has_sibling_nodes (Node : TNode) return Boolean;
   --  Returns True if any other nodes have the same parent node.


   function first_child_index (Node : TNode) return TNodeIndex;
   --  Returns the index of the first child node (1 or higher)
   --  If no child node exists, the result is negative (-4).


   function parent_node_index (Node : TNode) return TNodeIndex;
   --  Returns the index of the parent node (1 or higher)
   --  If requested on the root node, the result will be negative (-1).


   function next_sibling_node_index (Node : TNode) return TNodeIndex;
   --  Returns the index of the next node that shares same parent. (1+)
   --  If no later sibling exists, the index will be negative (-3 to be exact)


   function previous_sibling_node_index (Node : TNode) return TNodeIndex;
   --  Returns the index of the previous node that shares same parent. (1+)
   --  If no later sibling exists, the index will be negative (-2 to be exact)


   function index                 (Node : TNode) return TNodeIndex;
   --  Returns the index of the node (1 or higher)


   function end_of_scope          (Node : TNode) return TNodeIndex;
   --  Returns the end of the range the comprise the descendent nodes.  The
   --  begining of the range is the node index.


   function child_node_count      (Node : TNode) return Natural;
   --  Returns the number of child nodes belonging to the node (0 or more).


   function id (Node : TNode) return SU.Unbounded_String;
   --  This function returns the "id" string of the node.


   function tag_name (Node : TNode) return SU.Unbounded_String;
   --  This function returns the tag name of the node.


   function attribute_count       (Node : TNode) return Natural;
   --  Returns the number of attributes defined for the node (0 or more).


   function attribute_index (Node : TNode;
                             key  : String) return Natural;
   --  Returns the index of an attribute when given its key string.  A zero
   --  is returned if the key doesn't exist.


   function attribute_value (Node : TNode;
                             key  : String)
   return SU.Unbounded_String;
   --  Returns the attribute value given a valid key string.  An invalid key
   --  will yield a null-string result.


   function attribute_value (Node  : TNode;
                             index : Positive) return SU.Unbounded_String;
   --  Returns the attribute value given a valid index (range starts with 1).
   --  An invalid index causes a null-string to be returned.


   function attribute_key (Node  : TNode;
                           index : Positive) return SU.Unbounded_String;
   --  Returns the attribute key given a valid index (range starts with 1).
   --  An invalid index causes a null-string to be returned.


   procedure attribute_add (Node  : in out TNode;
                            key   : in     SU.Unbounded_String;
                            value : in     SU.Unbounded_String);
   --  This procedure inserts attribute data at the end of the node's list.


   procedure attribute_clear_all (Node : in out TNode);
   --  This procedure wipes out all attributes from the node.  This is not
   --  needed for read-only functionality.


   procedure clear_shared_data;
   --  This procedure clears out the attribute and tag name data shared by
   --  all nodes.  This isn't needed for read-only functionality.


   procedure set_basic_data (Node       : in out TNode;
                             Self_ID    : in     TNodeIndex;
                             Parent_ID  : in     TNodeIndex;
                             TagName    : in     SU.Unbounded_String;
                             Identifier : in     SU.Unbounded_String);
   --  This procedure is run once to establish basic node data.  The number of
   --  child nodes will be set to 0, the scope will be set to Self_ID, and the
   --  remaining relations will be set to defaults.  Attributes are cleared.


   procedure signal_parent (Node     : in out TNode;
                            Child_ID : in     TNodeIndex);
   --  Running this procedure will affect the "First_Child" property as well as
   --  the "Num_Children" setting.


   procedure signal_next_brother (Node       : in out TNode;
                                  Brother_ID : in     TNodeIndex);
   --  Running this procedure will affect the "Next_Sibling_ID" of the
   --  previous sibling node.


   procedure signal_prev_brother (Node       : in out TNode;
                                  Brother_ID : in     TNodeIndex);
   --  Running this procedure will affect the "Prev_Sibling_ID" of the
   --  next sibling node.


   procedure expand_scope (Node    : in out TNode;
                           Node_ID : in     TNodeIndex);
   --  This procedure needs to be executed on parents, grandparents, etc, all
   --  the way back to the root node.


   procedure tags_insert_word (word : in SU.Unbounded_String);
   procedure attr_insert_word (word : in SU.Unbounded_String);
   --  Wrappers for condensed_text::insert_word


   procedure tags_set_active_state;
   procedure attr_set_active_state;
   --  Wrappers for condensed_text::set_active_state


   procedure tags_dump;
   procedure attr_dump;
   --  Wrappers for condensed_text::dump


private

   type TNode is tagged limited record
      ID              : TNodeIndex := NO_INDEX_DEFINED;
      Parent_ID       : TNodeIndex := NO_PARENT;
      Next_Sibling_ID : TNodeIndex := NO_FOLLOWING_BROTHER;
      Prev_Sibling_ID : TNodeIndex := NO_PREVIOUS_BROTHER;
      First_Child_ID  : TNodeIndex := NO_OFFSPRING;
      Scope           : TNodeIndex := NO_INDEX_DEFINED;
      Num_Children    : Natural    := 0;
      TagName         : Positive   := 1;
      Identifier      : SU.Unbounded_String := SU.Null_Unbounded_String;
      Attributes      : aliased TAttributes;
   end record;

   type TNode_Shared is record
      tags       : Condensed_Text.TTextPacker;
   end record;

   Node_Shared : TNode_Shared;

end Nodes;
