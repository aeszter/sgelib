package body SGE.Advance_Reservations is

   -----------------
   -- Append_List --
   -----------------

   procedure Append_List (Node_Tree : SGE.Parser.Tree) is
      Nodes : Parser.Node_List := Parser.Get_Elements_By_Tag_Name (Doc => Node_Tree,
                                                                   A   => "ar_summary");
      AR_Node : Parser.Node;
   begin
      for Index in 1 .. Length (Nodes) loop
         AR_Node := Item (Nodes, Index - 1);
         if Name (AR_Node) /= "#text" then
            List.Append (New_Reservation (Child_Nodes (AR_Node)));
         end if;
      end loop;
   end Append_List;

end SGE.Advance_Reservations;
