with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Ordered_Maps;
with DOM.Core;
with SGE.Utils;

package SGE.Quota is
   use SGE.Utils;

   type Resource_Quota is private;

   procedure Append_List (Nodes : DOM.Core.Node_List);
   function Get_Limit (User : SGE.Utils.User_Name; PEs : Boolean) return Integer;
   function Get_Value (User : SGE.Utils.User_Name; PEs : Boolean) return Integer;
   function Get_Headroom (User : SGE.Utils.User_Name; PEs : Boolean) return Natural;

private
   type Resource_Quota is record
      User : User_Name;
      Resource : Unbounded_String;
      Limit : Integer;
      Value : Integer;
      PEs : Boolean;
   end record;

   package Lists is new Ada.Containers.Ordered_Maps (Key_Type => Unbounded_String,
                                         Element_Type => Resource_Quota);

   List : Lists.Map;

end SGE.Quota;
