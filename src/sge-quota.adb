with SGE.Parser; use SGE.Parser;

package body SGE.Quota is

   function "<" (Left, Right : Index_Card) return Boolean is
   begin
      if Left.User < Right.User then
         return True;
      elsif Left.User > Right.User then
         return False;
      elsif Left.Resource < Right.Resource then
         return True;
      elsif Left.Resource > Right.Resource then
            return False;
      elsif Left.PEs < Right.PEs then
            return True;
      else
         return False;
      end if;

   end "<";


   -----------------
   -- Append_List --
   -----------------

   procedure Append_List (Nodes : DOM.Core.Node_List) is
      N, Field_Node : Node;
      Subnodes      : Node_List;

   begin
      for Index in 1 .. Length (Nodes) loop
         N := Item (Nodes, Index - 1);
         Subnodes := Child_Nodes (N);
         if Name (N) /= "qquota_rule" then
            raise Parser_Error with "found """ & Name (N)
              & """ instead of qquota_rule";
         end if;
         declare
            RQS : Index_Card;
            Quota_Value : Quota;
         begin
            for Field_Index in 1 .. Length (Subnodes) loop
               Field_Node := Item (Subnodes, Field_Index - 1);
               if Name (Field_Node) = "users" then
                  RQS.User := To_User_Name (Value (Field_Node));
               elsif Name (Field_Node) = "pes" then
                  RQS.PEs := True;
               elsif Name (Field_Node) = "limit" then
                  RQS.Resource := To_Unbounded_String (Value (Get_Attr (Field_Node, "resource")));
                  Quota_Value.Limit := Integer'Value (Value (Get_Attr (Field_Node, "limit")));
                  Quota_Value.Value := Integer'Value (Value (Get_Attr (Field_Node, "value")));
               elsif Name (Field_Node) = "#text" then
                  null;
               else
                  raise Parser_Error with "unexpected node found: """
                    & Name (Field_Node) & """";
               end if;
            end loop;
            List.Insert (RQS, Quota_Value);
         end;
      end loop;
   end Append_List;

   ---------------
   -- Get_Limit --
   ---------------

   function Get_Limit
     (User     : SGE.Utils.User_Name;
      PEs      : Boolean;
      Resource : String)
      return Integer
   is
   begin
      return List.Element ((User => User,
                            PEs      => PEs,
                            Resource => To_Unbounded_String (Resource))).Limit;
   end Get_Limit;

   ---------------
   -- Get_Value --
   ---------------

   function Get_Value
     (User     : SGE.Utils.User_Name;
      PEs      : Boolean;
      Resource : String)
      return Integer
   is
   begin
      return List.Element ((User    => User,
                         PEs     => PEs,
                         Resource => To_Unbounded_String (Resource))).Value;
   end Get_Value;

   ------------------
   -- Get_Headroom --
   ------------------

   function Get_Headroom
     (User     : SGE.Utils.User_Name;
      PEs      : Boolean;
      Resource : String)
      return Integer
   is
   begin
      return Get_Limit (User, PEs, Resource)  - Get_Value (User, PEs, Resource);
   end Get_Headroom;

end SGE.Quota;
