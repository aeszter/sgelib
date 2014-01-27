with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package body SGE.Context is
   function Get (From : List; Key : Key_Type) return String is
   begin
      if From.Contains (To_Literal (Key)) then
         return To_String (From.Element (To_Literal (Key)));
      else
         return "";
      end if;
   end Get;

end SGE.Context;
