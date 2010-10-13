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


--  with expat_h;
with File_Handling;

package body XML_Reader is

   ----------------
   --  Finalize  --
   ----------------

   overriding
   procedure Finalize (DOM : in out TDOM)
   is
   begin
      Free (DOM.Document);
   end Finalize;



   --------------------------
   --  load_xml_from_file  --
   --------------------------

   procedure load_xml_from_file (DOM       : in out TDOM;
                                 file_name : in     String)
   is
      contents : constant SU.Unbounded_String :=
                 File_Handling.File_Get_Contents (filename => file_name);
      use SU;
   begin
      if contents = SU.Null_Unbounded_String then
         DOM.Document := new TNodeSet (1 .. 0);
      end if;
      DOM.load_xml_from_string (input_string => contents);
   end load_xml_from_file;



   ----------------------------
   --  load_xml_from_string  --
   ----------------------------

   procedure load_xml_from_string (DOM          : in out TDOM;
                                   input_string : in     SU.Unbounded_String)
   is
   begin
   --  This procedure interfaces with the expat library to build and populate
   --  the DOM object represented by the provided string input.  A DOM object
   --  is returned.  The string is not checked for well-formedness or validity,
   --  it's just expected to be correct.
      null;
   end load_xml_from_string;



   ------------------
   --  child_node  --
   ------------------

   function child_node (DOM     : TDOM;
                        Node_ID : Nodes.TNodeIndex;
                        Child   : Positive)
   return Nodes.TNodeIndex is
      numNodes : constant Natural := DOM.Document'Length;
      result   : Nodes.TNodeIndex;
   begin
      if Node_ID > numNodes then
         return Nodes.NODE_RANGE_EXCEEDED;
      end if;
      if not DOM.Document (Node_ID).has_child_nodes then
         return Nodes.NO_OFFSPRING;
      end if;
      if Child > DOM.Document (Node_ID).child_node_count then
         return Nodes.CHILD_RANGE_EXCEEDED;
      end if;
      result := DOM.Document (Node_ID).first_child;
      if Child = 1 then
         return result;
      end if;

      for x in Positive range 2 .. Child loop
         result := DOM.Document (result).next_sibling_node;
      end loop;
      return result;

   end child_node;

end XML_Reader;
