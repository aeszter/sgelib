with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Ordered_Maps;
with DOM.Core;
with SGE.Utils;

package SGE.Quota is
   use SGE.Utils;

   procedure Append_List (Nodes : DOM.Core.Node_List);
   function Get_Limit (User     : SGE.Utils.User_Name;
      PEs      : Boolean;
      Resource : String)
                       return Integer;
   function Get_Value (User     : SGE.Utils.User_Name;
      PEs      : Boolean;
      Resource : String)
                       return Integer;
   function Get_Headroom (User     : SGE.Utils.User_Name;
      PEs      : Boolean;
      Resource : String)
                          return Integer;

private
   type Index_Card is record
      User : User_Name;
      Resource : Unbounded_String;
      PEs : Boolean := False;
   end record;

   type Quota is record
      Limit : Integer;
      Value : Integer;
   end record;

   function "<" (Left, Right : Index_Card) return Boolean;

   package Lists is new Ada.Containers.Ordered_Maps (Key_Type => Index_Card,
                                         Element_Type => Quota);

   List : Lists.Map;

end SGE.Quota;
