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


with System;
with File_Handling;
with Interfaces.C.Strings;
with Ada.Text_IO; use Ada.Text_IO;

package body XML_Reader is

   package IC renames Interfaces.C;
   package ICS renames Interfaces.C.Strings;


   --------------------------------
   --  get_elements_by_tag_name  --
   --------------------------------

   function get_elements_by_tag_name (DOM      : TDOM;
                                      tag_name : String)
   return TNodeGroup is
   begin
      return DOM.get_child_elements_by_tag_name (
                  Node     => DOM.Document (1),
                  tag_name => tag_name);
   end get_elements_by_tag_name;



   --------------------------------------
   --  get_child_elements_by_tag_name  --
   --------------------------------------

   function get_child_elements_by_tag_name (DOM      : TDOM;
                                            Node     : Nodes.TNode;
                                            tag_name : String)
   return TNodeGroup is
      tag_count  : Natural := 0;
      start_tag  : constant Nodes.TNodeIndex := Node.first_child_index;
      end_tag    : constant Nodes.TNodeIndex := Node.end_of_scope;
      tagname_us : constant SU.Unbounded_String :=
                            SU.To_Unbounded_String (tag_name);
      use SU;
   begin
      for x in Nodes.TNodeIndex range start_tag .. end_tag loop
         if DOM.Document (x).tag_name = tagname_us then
            tag_count := tag_count + 1;
         end if;
      end loop;
      declare
         result : TNodeGroup (1 .. tag_count);
         kindex : Natural := 0;
      begin
         if tag_count > 0 then
            for x in Nodes.TNodeIndex range start_tag .. end_tag loop
               if DOM.Document (x).tag_name = tagname_us then
                  kindex := kindex + 1;
                  result (kindex) := DOM.Document (x)'Access;
               end if;
            end loop;
         end if;

         return result;
      end;

   end get_child_elements_by_tag_name;



   -------------------------
   --  get_element_by_id  --
   -------------------------

   function get_element_by_id (DOM : TDOM;
                               ID  : String)
   return access Nodes.TNode is
      index      : Natural := 0;
      node_count : constant Natural := DOM.Document'Length;
      found      : Boolean := False;
      id_us      : constant SU.Unbounded_String := SU.To_Unbounded_String (ID);

      use SU;
   begin
      while not found and then index < node_count loop
         index := index + 1;
         if DOM.Document (index).id = id_us then
            found := True;
         end if;
      end loop;

      if found then
         return DOM.Document (index)'Access;
      else
         return null;
      end if;

   end get_element_by_id;



   -------------------
   --  parent_node  --
   -------------------

   function parent_node (DOM  : TDOM;
                         Node : Nodes.TNode)
   return access Nodes.TNode is
      parent_id : constant Nodes.TNodeIndex := Node.parent_node_index;
   begin
      pragma Assert (Check => parent_id > Nodes.NO_INDEX_DEFINED);

      return DOM.Document (parent_id)'Access;
   end parent_node;



   -------------------
   --  first_child  --
   -------------------

   function first_child (DOM  : TDOM;
                         Node : Nodes.TNode)
   return access Nodes.TNode is
      first_child_id : constant Nodes.TNodeIndex := Node.first_child_index;
   begin
      pragma Assert (Check => first_child_id > Nodes.NO_INDEX_DEFINED);

      return DOM.Document (first_child_id)'Access;
   end first_child;



   -----------------------------
   --  previous_sibling_node  --
   -----------------------------

   function previous_sibling_node (DOM  : TDOM;
                                   Node : Nodes.TNode)
   return access Nodes.TNode is
      sibling_id : constant Nodes.TNodeIndex :=
                            Node.previous_sibling_node_index;
   begin
      pragma Assert (Check => sibling_id > Nodes.NO_INDEX_DEFINED);

      return DOM.Document (sibling_id)'Access;
   end previous_sibling_node;



   -------------------------
   --  next_sibling_node  --
   -------------------------

   function next_sibling_node (DOM  : TDOM;
                               Node : Nodes.TNode)
   return access Nodes.TNode is
      sibling_id : constant Nodes.TNodeIndex :=
                            Node.next_sibling_node_index;
   begin
      pragma Assert (Check => sibling_id > Nodes.NO_INDEX_DEFINED);

      return DOM.Document (sibling_id)'Access;
   end next_sibling_node;



   ------------------
   --  child_node  --
   ------------------

   function child_node (DOM   : TDOM;
                        Node  : Nodes.TNode;
                        Child : Positive)
   return access Nodes.TNode is
      child_id : constant Nodes.TNodeIndex := DOM.child_node_index (
                          Node_ID => Node.index,
                          Child   => Child);
   begin
      pragma Assert (Check => child_id > Nodes.NO_INDEX_DEFINED);

      return DOM.Document (child_id)'Access;
   end child_node;



   ----------------
   --  children  --
   ----------------

   function children (DOM  : TDOM;
                      Node : Nodes.TNode)
   return TNodeGroup is
      kids   : constant Natural := Node.child_node_count;
      result : TNodeGroup (1 .. kids);
   begin
      if kids = 0 then
         return result;
      end if;

      result (1) := DOM.first_child (Node);
      for x in Positive range 1 .. kids - 1 loop
         result (x + 1) := DOM.next_sibling_node (result (x).all);
      end loop;
      return result;
   end children;



   ----------------
   --  Finalize  --
   ----------------

   overriding
   procedure Finalize (DOM : in out TDOM)
   is
   begin
      if DOM.Document /= null then
         Free (DOM.Document);
      end if;
      DOM.DocLength := 0;
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
      if SU.To_String (contents) (1 .. 14) = "FILE NOT FOUND" then
         DOM.Document := new TNodeSet (1 .. 0);
         Ada.Text_IO.Put_Line (SU.To_String (contents));
         return;
      end if;
      DOM.load_xml_from_string (input_string => contents);
   end load_xml_from_file;



   ----------------------
   --  call_tag_start  --
   ----------------------

   procedure call_tag_start (
         userData : in out expat.Access_Void;
         name     : in     expat.Access_XML_Char;
         atts     : in     expat.Access_XML_Char_Array)
   is
      name_string : constant String := ICS.Value (Item => name);
      index : IC.size_t := 0;
      use IC;
      use ICS;
      use System;
   begin
      if Background.scanning then
         Background.depth       := Background.depth + 1;
         Background.total_nodes := Background.total_nodes + 1;

         jenny :
            loop
               exit jenny when atts (index) = Null_Ptr;
               Nodes.attr_insert_word (
                  word => SU.To_Unbounded_String (Value (atts (index))));
               index := index + 2;
            end loop jenny;
         Nodes.tags_insert_word (word => SU.To_Unbounded_String (name_string));
      else
         Background.total_nodes := Background.total_nodes + 1;
         declare
            Self_ID   : constant Nodes.TNodeIndex := Background.total_nodes;
            SF        : constant access Nodes.TNode :=
                        Background.Access_DOM.Document (Self_ID)'Access;
            Parent_ID : Nodes.TNodeIndex;
            Identity  : SU.Unbounded_String := SU.Null_Unbounded_String;
            id_attr   : constant String := "id";
            NP        : access Nodes.TNode;
            FC        : access Nodes.TNode;
            LB        : access Nodes.TNode;
         begin
            if Self_ID = 1 then
               Parent_ID := Nodes.NO_PARENT;
            else
               Parent_ID := Background.heritage.peek;
            end if;
            Background.heritage.push (Self_ID);
            index := 0;
            Martha :
               loop
                  exit Martha when atts (index) = Null_Ptr;
                  if Value (atts (index)) = id_attr then
                     Identity :=
                           SU.To_Unbounded_String (Value (atts (index + 1)));
                  end if;
                  index := index + 2;
               end loop Martha;

            SF.set_basic_data (
                  Self_ID    => Self_ID,
                  Parent_ID  => Parent_ID,
                  TagName    => SU.To_Unbounded_String (name_string),
                  Identifier => Identity);

            if Self_ID > 1 then
               NP := Background.Access_DOM.Document (Parent_ID)'Access;
               if NP.has_child_nodes then
                  FC := Background.Access_DOM.first_child (NP.all);
                  if NP.child_node_count = 1 then
                     SF.signal_prev_brother (Brother_ID => FC.index);
                     FC.signal_next_brother (Brother_ID => Self_ID);
                  else
                     LB := FC;
                     for x in Positive range 2 .. NP.child_node_count loop
                        LB := Background.Access_DOM.Document (
                              LB.next_sibling_node_index)'Access;
                     end loop;
                     SF.signal_prev_brother (Brother_ID => LB.index);
                     LB.signal_next_brother (Brother_ID => Self_ID);
                  end if;
               end if;
               NP.signal_parent (Child_ID => Self_ID);
            end if;

            index := 0;
            Jocelyn :
               loop
                  exit Jocelyn when atts (index) = Null_Ptr;
                  declare
                     skey : constant String := Value (atts (index));
                     sval : constant String := Value (atts (index + 1));
                  begin
                     if skey /= id_attr then
                        SF.attribute_add (
                              key   => SU.To_Unbounded_String (skey),
                              value => SU.To_Unbounded_String (sval));
                     end if;
                  end;
                  index := index + 2;
               end loop Jocelyn;
         end;
      end if;

      if userData = Null_Address then
         null;  --  to silence compiler
      end if;
   end call_tag_start;



   --------------------
   --  call_tag_end  --
   --------------------

   procedure call_tag_end (
         userData : in out expat.Access_Void;
         name     : in     expat.Access_XML_Char)
   is
      use ICS;
      use System;
      tag  : Nodes.TNodeIndex;
      Self : constant Natural := Background.total_nodes;
   begin
      if Background.scanning then
         Background.depth := Background.depth - 1;
      else
         tag := Background.heritage.peek;
         Background.Access_DOM.Document (tag).expand_scope (Node_ID => Self);
         Background.heritage.pop;
      end if;

      if name = ICS.Null_Ptr or else userData = Null_Address then
         null;  --  to silence compiler warning.
      end if;

   end call_tag_end;



   ----------------------------
   --  load_xml_from_string  --
   ----------------------------

   procedure load_xml_from_string (DOM          : in out TDOM;
                                   input_string : in     SU.Unbounded_String)
   is
      use expat;
      parser : constant XML_Parser :=
                        XML_ParserCreate (encoding => ICS.Null_Ptr);
      buffer : constant ICS.chars_ptr :=
                        ICS.New_String (SU.To_String (input_string));
      buflen : constant IC.int := IC.int (ICS.Strlen (buffer));
      status : XML_Status;
      airlock : aliased Natural := 0;
   begin
      if DOM.Document /= null then
         Free (DOM.Document);
      end if;

      Background.scanning    := True;
      Background.total_nodes := 0;
      Background.depth       := 0;
      Background.Access_DOM  := DOM'Unchecked_Access;

      XML_SetUserData (
            parser   => parser,
            userData => airlock'Address);
      XML_SetStartElementHandler (
            parser   => parser,
            handler  => call_tag_start'Access);
      XML_SetEndElementHandler (
            parser   => parser,
            handler  => call_tag_end'Access);

      status := XML_Parse (parser   => parser,
                           s        => buffer,
                           len      => buflen,
                           isFinal  => 1);

      Nodes.tags_set_active_state;
      Nodes.attr_set_active_state;

      if status = XML_STATUS_ERROR then
         Put_Line (expat.XML_ErrorString (parser => parser));
      else
         declare
            PR_Result : constant XML_Bool := XML_ParserReset (
                                                parser   => parser,
                                                encoding => ICS.Null_Ptr);
            use IC;
         begin
            Background.scanning := False;
            if PR_Result > 0 then
               DOM.Document := new TNodeSet (1 .. Background.total_nodes);
               DOM.DocLength := Background.total_nodes;
               Background.total_nodes := 0;

               XML_SetUserData (
                     parser   => parser,
                     userData => airlock'Address);
               XML_SetStartElementHandler (
                     parser   => parser,
                     handler  => call_tag_start'Access);
               XML_SetEndElementHandler (
                     parser   => parser,
                     handler  => call_tag_end'Access);

               status := XML_Parse (parser   => parser,
                                    s        => buffer,
                                    len      => buflen,
                                    isFinal  => 1);
            end if;
            if status = XML_STATUS_ERROR then
               null;
            end if;
         end;
      end if;

      XML_ParserFree (parser => parser);
   end load_xml_from_string;



   ------------------
   --  child_node  --
   ------------------

   function child_node_index (DOM     : TDOM;
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
      result := DOM.Document (Node_ID).first_child_index;
      if Child = 1 then
         return result;
      end if;

      for x in Positive range 2 .. Child loop
         result := DOM.Document (result).next_sibling_node_index;
      end loop;
      return result;

   end child_node_index;

end XML_Reader;
