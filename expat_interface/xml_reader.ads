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

package XML_Reader is

   package SU renames Ada.Strings.Unbounded;
   type TDOM is tagged limited private;

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
   --  is allocated.  The string is not checked for well-formedness or validity,
   --  it's just expected to be correct.


   function child_node (DOM     : TDOM;
                        Node_ID : Nodes.TNodeIndex;
                        Child   : Positive) return Nodes.TNodeIndex;
   --  This function returns a specific child node given a node with the index
   --  of Node_ID.  The "Child" must be at least one.  If the node doesn't have
   --  any child nodes, the function returns NO_OFFSPRING and if the "Child"
   --  input is too high, the function returns the value of
   --  CHILD_RANGE_EXCEEDED.

private


   type TNodeSet is array (Positive range <>) of Nodes.TNode;
   type TNodeSet_Access is access TNodeSet;


   type TDOM is new Ada.Finalization.Limited_Controlled with
      record
         Document : TNodeSet_Access := null;
      end record;


   overriding
   procedure Finalize (DOM : in out TDOM);


   procedure Free is
      new Ada.Unchecked_Deallocation (TNodeSet, TNodeSet_Access);


end XML_Reader;
