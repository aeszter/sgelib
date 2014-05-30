with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package body SGE.Loggers is

   -----------------
   -- Has_Entries --
   -----------------

   function Has_Errors (Object : Logger) return Boolean is
   begin
      return not Object.Error_Log.Is_Empty;
   end Has_Errors;

   ------------------
   -- Record_Error --
   ------------------

   procedure Record_Error (Object : in out Logger; Message : String) is
   begin
      Object.Error_Log.Append (To_Unbounded_String (Message));
   end Record_Error;

   procedure Iterate_Errors (Object  : Logger;
                             Process : not null access procedure (Message : String)) is
      procedure Wrapper (Position : Utils.String_Lists.Cursor) is
      begin
         Process (To_String (Utils.String_Lists.Element (Position)));
      end Wrapper;

   begin
      Object.Error_Log.Iterate (Wrapper'Access);
   end Iterate_Errors;


end SGE.Loggers;
