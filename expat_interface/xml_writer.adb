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


with Ada.Characters.Latin_1;

package body xml_writer is

   package Latin renames Ada.Characters.Latin_1;


   -----------------
   --  close_tag  --
   -----------------

   function close_tag (Node  : Nodes.TNode;
                       depth : Natural)
   return SU.Unbounded_String is
      line : SU.Unbounded_String := SU.Null_Unbounded_String;
      use SU;
   begin
      for x in Natural range 1 .. depth loop
         SU.Append (Source => line, New_Item => "  ");
      end loop;

      SU.Append (Source => line, New_Item => Latin.Less_Than_Sign &
                                             Latin.Solidus &
                                             Node.tag_name &
                                             Latin.Greater_Than_Sign &
                                             Latin.LF);
      return line;
   end close_tag;



   ----------------------
   --  elaborated_tag  --
   ----------------------

   function elaborated_tag (Node  : Nodes.TNode;
                            depth : Natural)
   return SU.Unbounded_String is
      line    : SU.Unbounded_String := SU.Null_Unbounded_String;
      content : constant SU.Unbounded_String := Node.get_element_content;
      use SU;
   begin
      for x in Natural range 1 .. depth loop
         SU.Append (Source => line, New_Item => "  ");
      end loop;

      SU.Append (Source => line, New_Item => Latin.Less_Than_Sign);
      SU.Append (Source => line, New_Item => Node.tag_name);

      if Node.id /= SU.Null_Unbounded_String then
         SU.Append (Source => line, New_Item =>
            " id=" & Latin.Quotation & Node.id & Latin.Quotation);
      end if;

      for x in Natural range 1 .. Node.attribute_count loop
         SU.Append (Source => line, New_Item =>
            ' ' &  Node.attribute_key (x) &  '=' &
            Latin.Quotation &  Node.attribute_value (x) &  Latin.Quotation);
      end loop;

      --  We only close the tag if there are no children.
      if Node.end_of_scope = Node.index then
         if content = SU.Null_Unbounded_String then
            SU.Append (Source   => line,
                       New_Item => Latin.Solidus & Latin.Greater_Than_Sign);
         else
            SU.Append (Source => line, New_Item => Latin.Greater_Than_Sign);
            SU.Append (Source => line, New_Item => content);
            SU.Append (Source => line, New_Item => Latin.Less_Than_Sign &
                                                   Latin.Solidus &
                                                   Node.tag_name &
                                                   Latin.Greater_Than_Sign);
         end if;
      else
         SU.Append (Source   => line, New_Item => Latin.Greater_Than_Sign);
      end if;

      SU.Append (Source => line, New_Item => Latin.LF);
      return line;

   end elaborated_tag;


   ---------------------------
   --  recursive_next_node  --
   ---------------------------

   procedure recursive_next_node (DOM           : in     TDOM;
                                  node_in_focus : in     Nodes.TNode;
                                  next_node     :    out Nodes.TNodeIndex;
                                  output_text   : in out SU.Unbounded_String)
   is
      parent     : Acc_RecData;
      copydepth  : Natural := GR.depth;
   begin
      next_node := Nodes.NO_INDEX_DEFINED;
      if node_in_focus.child_node_count > GR.kid_leaf.peek then
         --  There are some child nodes to exploit.
         --  Path is do all descendents of first child in order before
         --  moving to second child.  Repeat until all primary children
         --  have been elaborated.

         declare
            next_kid  : constant Nodes.TNodeIndex := GR.kid_leaf.peek + 1;
            kid_index : Nodes.TNodeIndex := node_in_focus.first_child_index;
         begin
            for x in Positive range 2 .. next_kid loop
               kid_index := DOM.node_reference
                               (kid_index).Node.next_sibling_node_index;
            end loop;
            next_node := kid_index;
            GR.kid_leaf.pop;
            GR.kid_leaf.push (item => next_kid);
            GR.kid_leaf.push (item => 0);  --  next_kid's kids
            GR.depth := GR.depth + 1;
         end;
      else
         --  We've checked out all the children (or the node just doesn't
         --  have any) so we can move back up one level.
          if GR.depth > 0 then
            GR.kid_leaf.pop;
            GR.depth := GR.depth - 1;

            --  close all tags that show an end of scope of tag in focus.
            while GR.pending.peek /= Nodes.POPPED_EMPTY_STACK and then
                  DOM.node_reference (GR.pending.peek).Node.end_of_scope =
                  node_in_focus.end_of_scope loop
               declare
                  enode_id : constant Nodes.TNodeIndex := GR.pending.peek;
                  node_ref : constant Acc_RecData :=
                                         DOM.node_reference (enode_id);
               begin
                  if node_ref.Node.end_of_scope = node_in_focus.index then
                     GR.pending.pop;
                     copydepth := copydepth - 1;
                     SU.Append (Source   => output_text,
                                New_Item => close_tag (
                                            Node  => node_ref.Node,
                                            depth => copydepth));
                  end if;
               end;
            end loop;

            parent := DOM.node_reference (node_in_focus.parent_node_index);
            DOM.recursive_next_node (node_in_focus => parent.Node,
                                     next_node     => next_node,
                                     output_text   => output_text);
          end if;
      end if;

   end recursive_next_node;



   -----------------------------
   --  tagged_representation  --
   -----------------------------

   function tagged_representation (DOM : TDOM)
   return SU.Unbounded_String is
      xml_tag : constant String :=
                "<?xml version="
                & Latin.Quotation & "1.0" & Latin.Quotation
                & " encoding="
                & Latin.Quotation & "UTF-8" & Latin.Quotation
                & "?>" & Latin.LF;
      result   : SU.Unbounded_String := SU.To_Unbounded_String (xml_tag);
      Node_ID  : Nodes.TNodeIndex := 1;
      FNode    : Acc_RecData;
   begin
      if DOM.Document = null then
         return result;
      end if;

      GR.depth := 0;
      GR.kid_leaf.push (item => 0);
      loop

         exit when Node_ID <= Nodes.NO_INDEX_DEFINED;

         FNode := DOM.node_reference (Node_ID => Node_ID);
         SU.Append (Source   => result,
                    New_Item => elaborated_tag (FNode.Node, GR.depth));

         if Node_ID /= FNode.Node.end_of_scope then
            GR.pending.push (item => Node_ID);
         end if;

         DOM.recursive_next_node (node_in_focus => FNode.Node,
                                  next_node     => Node_ID,
                                  output_text   => result);
      end loop;
      return result;

   end tagged_representation;



   ---------------------
   --  add_attribute  --
   ---------------------

   procedure add_attribute (DOM             : in out TDOM;
                            Node_ID         : in     Nodes.TNodeIndex;
                            Attribute_key   : in     SU.Unbounded_String;
                            Attribute_value : in     SU.Unbounded_String)
   is
      myNode : constant Acc_RecData := DOM.node_reference (Node_ID => Node_ID);
   begin
      pragma Assert (Check   => myNode /= null,
                     Message => "xml_writer::add_attribute: Node_ID invalid");

      myNode.Node.attribute_add (key   => Attribute_key,
                                 value => Attribute_value);
   end add_attribute;



   ----------------------
   --  node_reference  --
   ----------------------

   function node_reference (DOM     : TDOM;
                            Node_ID : Nodes.TNodeIndex)
   return Acc_RecData is
      result : Acc_RecData := null;
      arrow  : Acc_RecData := DOM.Document;
   begin

      while result = null and then arrow /= null loop
         if arrow.Node.index = Node_ID then
            result := arrow;
         end if;
         arrow := arrow.next;
      end loop;
      return result;

   end node_reference;



   -------------------------
   --  create_child_node  --
   -------------------------

   procedure create_child_node (DOM        : in out TDOM;
                                Parent_ID  : in     Nodes.TNodeIndex;
                                TagName    : in     SU.Unbounded_String;
                                Identifier : in     SU.Unbounded_String;
                                Contents   : in     SU.Unbounded_String;
                                Child_ID   :    out Nodes.TNodeIndex)
   is
      parent   : Acc_RecData       := null;
      new_node : Acc_RecData       := null;
   begin
      if DOM.Document = null then
         pragma Assert (
               Check   => Parent_ID = Nodes.NO_PARENT,
               Message => "xml_writer: First node must be parent-less");
         null;
      else
         pragma Assert (
               Check   => Parent_ID > Nodes.NO_INDEX_DEFINED,
               Message => "xml_writer: Illegal parent ID");

         parent := DOM.node_reference (Node_ID => Parent_ID);

         pragma Assert (
               Check   => parent /= null,
               Message => "xml_writer: Undefined parent ID");
      end if;

      Child_ID       := GR.total_nodes + 1;
      GR.total_nodes := Child_ID;
      new_node       := new RecData;

      new_node.Node.set_basic_data (
            Self_ID    => Child_ID,
            Parent_ID  => Parent_ID,
            TagName    => TagName,
            Identifier => Identifier);

      new_node.Node.set_element_content (content => Contents);
      new_node.next := DOM.Document;

      DOM.Document := new_node;

      if Child_ID > 1 then
         declare
            FC  : Acc_RecData;
            LB  : Acc_RecData;
            PNC : constant Natural := parent.Node.child_node_count;
         begin
            if parent.Node.has_child_nodes then
               FC := DOM.node_reference (parent.Node.first_child_index);
               if PNC = 1 then
                  new_node.Node.signal_prev_brother (FC.Node.index);
                  FC.Node.signal_next_brother       (Child_ID);
               else
                  LB := FC;
                  for x in Positive range 2 .. PNC loop
                     LB := DOM.node_reference (
                           LB.Node.next_sibling_node_index);
                  end loop;
                  new_node.Node.signal_prev_brother (LB.Node.index);
                  LB.Node.signal_next_brother       (Child_ID);
               end if;
            end if;
         end;
         parent.Node.signal_parent (Child_ID => Child_ID);
      end if;

      --  expand the scope for all ancestors
      declare
         ancestor_index : Nodes.TNodeIndex := new_node.Node.parent_node_index;
         ancestor       : Acc_RecData;
      begin
         while ancestor_index /= Nodes.NO_PARENT loop
            ancestor := DOM.node_reference (ancestor_index);
            ancestor.Node.expand_scope (Node_ID => Child_ID);
            ancestor_index := ancestor.Node.parent_node_index;
         end loop;
      end;

      --  special handling for root node
      if Child_ID = 1 then
         declare
            val1 : constant String :=
                            "http://www.w3.org/2001/XMLSchema-instance";
            use SU;
         begin
            new_node.Node.attribute_add (
               key   => SU.To_Unbounded_String (ROOT_NS),
               value => SU.To_Unbounded_String (val1)
            );
            if DOM.schema_location /= SU.Null_Unbounded_String then
               new_node.Node.attribute_add (
                  key   => SU.To_Unbounded_String (ROOT_SCHEMA),
                  value => DOM.schema_location
               );
            end if;
         end;
      end if;

   end create_child_node;



   ------------------------------
   --  define_schema_location  --
   ------------------------------

   procedure define_schema_location (DOM      : in out TDOM;
                                     location : in String)
   is
   begin

      DOM.schema_location := SU.To_Unbounded_String (location);

   end define_schema_location;



   --------------------
   --  clear_canvas  --
   --------------------

   procedure clear_canvas (DOM : in out TDOM)
   is
      old_node : Acc_RecData := DOM.Document;
   begin

      GR.total_nodes := 0;
      GR.kid_leaf.clear;
      DOM.schema_location := SU.Null_Unbounded_String;
      while DOM.Document /= null loop
         DOM.Document := DOM.Document.next;

         Free (old_node);
      end loop;
      DOM.Document := null;

   end clear_canvas;



   ----------------
   --  Finalize  --
   ----------------

   overriding
   procedure Finalize (DOM : in out TDOM) is
   begin

      DOM.clear_canvas;

   end Finalize;

end xml_writer;
