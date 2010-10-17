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


with Nodes;
with Ada.Finalization;
with Ada.Unchecked_Deallocation;
with Ada.Strings.Unbounded;
with Generic_Stack;

--  The XML writer has a major limitation.  Even though you can physically
--  define nodes in any order, the close tags will not work right if this is
--  done.  The problem goes back to code reused from the reading module which
--  reads all the nodes sequentially by descendents and thus defines a "scope"
--  where the end of the scope is where the closing tag would be.  If the
--  scope isn't build contingously, then the scheme collapses.

package xml_writer is

   package SU renames Ada.Strings.Unbounded;

   type TDOM is tagged limited private;

   ROOT_NS     : constant String := "xmlns:xsi";
   ROOT_SCHEMA : constant String := "xsi:noNamespaceSchemaLocation";


   procedure clear_canvas (DOM : in out TDOM);
   --  Clears the kid stack and frees all the memory used by created nodes.
   --  It clears the schema location.
   --  It is called by finalize as well.


   procedure create_child_node (DOM        : in out TDOM;
                                Parent_ID  : in     Nodes.TNodeIndex;
                                TagName    : in     SU.Unbounded_String;
                                Identifier : in     SU.Unbounded_String;
                                Contents   : in     SU.Unbounded_String;
                                Child_ID   :    out Nodes.TNodeIndex);
   --  Appends the list of child nodes with a new child given the parent's
   --  node index and some basic properties.  The newly created child index
   --  ID will be returned.  The first call must send the Parent_ID of
   --  "NO_PARENT" as this is the root node.


   procedure add_attribute (DOM             : in out TDOM;
                            Node_ID         : in     Nodes.TNodeIndex;
                            Attribute_key   : in     SU.Unbounded_String;
                            Attribute_value : in     SU.Unbounded_String);
   --  This is the way to add an unlimited number of attribute key pairs to
   --  the node.  Just pass the node index ID along with the key pair.


   procedure define_schema_location (DOM      : in out TDOM;
                                     location : in String);
   --  Sets the xsi:noNameSpaceSchemaLocation attribute.


   function tagged_representation (DOM : TDOM) return SU.Unbounded_String;
   --  Internally builds an XML file.


   --  procedure save_xml (filename : in SU.Unbounded_String);
   --  Saves the tagged representation as a file with the given name.

private

   type RecData;
   type Acc_RecData is access all RecData;

   type RecData is record
      Node : Nodes.TNode;
      next : Acc_RecData := null;
   end record;

   type TDOM is new Ada.Finalization.Limited_Controlled with
      record
         schema_location : SU.Unbounded_String := SU.Null_Unbounded_String;
         Document        : Acc_RecData := null;
      end record;

   package Kid_Stack is new Generic_Stack (
            TItem                => Nodes.TNodeIndex,
            DEPLETED_STACK_ERROR => Nodes.POPPED_EMPTY_STACK);

   type Rec_Global is record
      kid_leaf     : Kid_Stack.TStack;
      pending      : Kid_Stack.TStack;
      depth        : Natural := 0;
      total_nodes  : Natural := 0;
      Access_DOM   : access TDOM;
   end record;

   GR : Rec_Global;    --  "Global Record"

   overriding
   procedure Finalize (DOM : in out TDOM);


   procedure Free is
      new Ada.Unchecked_Deallocation (RecData, Acc_RecData);


   function node_reference (DOM     : TDOM;
                            Node_ID : Nodes.TNodeIndex) return Acc_RecData;
   --  Returns the pointer to the node given it's index identity.


   function elaborated_tag (Node  : Nodes.TNode;
                            depth : Natural) return SU.Unbounded_String;
   --  Returns the string representation of the node (perhaps open tag only).


   function close_tag (Node  : Nodes.TNode;
                       depth : Natural) return SU.Unbounded_String;
   --  Returns the string representation of a solitary closing tag.


   procedure recursive_next_node (DOM           : in     TDOM;
                                  node_in_focus : in     Nodes.TNode;
                                  next_node     :    out Nodes.TNodeIndex;
                                  output_text   : in out SU.Unbounded_String);
   --  This function will move up and down the tree as necessary to figure out
   --  which leave should be printed out next.  The priority is the children,
   --  so all the decendents of the first child are searched before the
   --  second child (e.g. root -> first child -> first child -> etc...


end xml_writer;
