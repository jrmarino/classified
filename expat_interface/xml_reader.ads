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
with expat;
with Ada.Finalization;
with Ada.Unchecked_Deallocation;
with Ada.Strings.Unbounded;
with Generic_Stack;


package XML_Reader is

   package SU renames Ada.Strings.Unbounded;

   type TNodeSet is array (Positive range <>) of aliased Nodes.TNode;
   type TNodeSet_Access is access TNodeSet;
   type TNodeGroup is array (Positive range <>) of access Nodes.TNode;
   type TDOM is new Ada.Finalization.Limited_Controlled with
      record
         DocLength : Natural := 0;
         Document  : TNodeSet_Access := null;
      end record;

   procedure load_xml_from_file (DOM       : in out TDOM;
                                 file_name : in     String);
   --  This procedure takes a path to a file which is assumed to be of XML
   --  format.  The file will be scanned and populated, and a DOM Object
   --  will be allocated.  Internally it just loads the file into an unbounded
   --  string and calls the "load_xml_from_string" procedure.


   procedure load_xml_from_string (DOM          : in out TDOM;
                                   input_string : in     SU.Unbounded_String);
   --  This procedure interfaces with the expat library to build and populate
   --  the DOM object represented by the provided string input.  A DOM object
   --  is allocated.  The string is not checked for well-formedness or
   --  validity, it's just expected to be correct.


   function child_node_index (DOM     : TDOM;
                              Node_ID : Nodes.TNodeIndex;
                              Child   : Positive) return Nodes.TNodeIndex;
   --  This function returns a specific child node given a node with the index
   --  of Node_ID.  The "Child" must be at least one.  If the node doesn't have
   --  any child nodes, the function returns NO_OFFSPRING and if the "Child"
   --  input is too high, the function returns the value of
   --  CHILD_RANGE_EXCEEDED.


   function child_node (DOM   : TDOM;
                        Node  : Nodes.TNode;
                        Child : Positive) return access Nodes.TNode;
   --  Returns access to the child node


   function parent_node (DOM  : TDOM;
                         Node : Nodes.TNode) return access Nodes.TNode;
   --  Returns access to the parent node


   function first_child (DOM  : TDOM;
                         Node : Nodes.TNode) return access Nodes.TNode;
   --  Returns access to the first child


   function previous_sibling_node (DOM  : TDOM;
                                   Node : Nodes.TNode)
   return access Nodes.TNode;
   --  Returns access to the previous sibling


   function next_sibling_node (DOM  : TDOM;
                               Node : Nodes.TNode)
   return access Nodes.TNode;
   --  Returns access to the next sibling


   function children (DOM  : TDOM;
                      Node : Nodes.TNode) return TNodeGroup;
   --  Returns an array of nodes which are the children of the given node.


   function get_element_by_id (DOM : TDOM;
                               ID  : String) return access Nodes.TNode;
   --  Returns access to the node with the given ID.
   --  Returns null if the ID isn't found.


   function get_elements_by_tag_name (DOM      : TDOM;
                                      tag_name : String) return TNodeGroup;
   --  Returns all elements with the same tag name for the entire document
   --  as a group.  It is equivalent to get_child_elements_by_tag_name
   --  performed on the root element.


   function get_child_elements_by_tag_name (
                  DOM      : TDOM;
                  Node     : Nodes.TNode;
                  tag_name : String) return TNodeGroup;
   --  Returns all elements with the same tag name for all the descendents of
   --  a given element.


   pragma Warnings (Off);
   procedure call_tag_start (
         userData : in out expat.Access_Void;
         name     : in     expat.Access_XML_Char;
         atts     : in     expat.Access_XML_Char_Array);
   pragma Convention (C, call_tag_start);
   pragma Warnings (On);
   --  This is a callback function passed to the expat library.  It is
   --  triggered when the parser reaches the start of a new element.


   procedure call_tag_end (
         userData : in out expat.Access_Void;
         name     : in     expat.Access_XML_Char);
   pragma Convention (C, call_tag_end);
   --  This is a callback function passed to the expat library.  It is
   --  triggered when the parser reaches the end of a new element.

private

   package Parent_Stack is new Generic_Stack (
            TItem                => Nodes.TNodeIndex,
            DEPLETED_STACK_ERROR => Nodes.POPPED_EMPTY_STACK);

   type Rec_Global is record
      scanning     : Boolean;
      total_nodes  : Natural;
      depth        : Natural;
      Access_DOM   : access TDOM;
      heritage     : Parent_Stack.TStack;
   end record;

   Background : Rec_Global;

   overriding
   procedure Finalize (DOM : in out TDOM);


   procedure Free is
      new Ada.Unchecked_Deallocation (TNodeSet, TNodeSet_Access);


end XML_Reader;
