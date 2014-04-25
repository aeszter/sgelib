with Ada.Calendar; use Ada.Calendar;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with SGE.Parser;
with SGE.Utils; use SGE.Utils;
with SGE.Jobs; use SGE.Jobs;

package SGE.Advance_Reservations is
   type Reservation is private;
   procedure Append_List (Node_Tree : SGE.Parser.Tree);

   function New_Reservation (List : SGE.Parser.Node_List) return Reservation;
   function Get_Name (R : Reservation) return String;
   function Get_ID (R : Reservation) return String;
   function Get_Owner (R : Reservation) return String;
   function Has_Error_Log_Entries (R: Reservation) return Boolean;
   procedure Iterate_Error_Log (R       : Reservation;
                                Process : not null access procedure (Message : String));
   procedure Iterate (Process : not null access procedure (R : Reservation));


private
   type Reservation is record
      Number               : Integer; -- Job ID
      Name                 : Unbounded_String; -- Job name
      Owner                : Utils.User_Name; -- User whom this job belongs to
      State_Array          : State;
      State_String         : String (1 .. 4);
      Start_Time, End_Time : Time;
   end record;
end;
