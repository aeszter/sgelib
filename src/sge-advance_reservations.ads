with Ada.Calendar; use Ada.Calendar;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with SGE.Parser;
with SGE.Utils; use SGE.Utils;
with SGE.Jobs; use SGE.Jobs;
with Ada.Containers.Doubly_Linked_Lists;

package SGE.Advance_Reservations is
   Other_Error : exception;
   type Reservation is private;

   procedure Append_List (Node_Tree : SGE.Parser.Tree);

   function New_Reservation (List : SGE.Parser.Node_List) return Reservation;
   function Get_Name (R : Reservation) return String;
   function Get_ID (R : Reservation) return String;
   function Get_Owner (R : Reservation) return User_Name;
   function Get_State (R : Reservation) return String;
   function Get_Start (R : Reservation) return Ada.Calendar.Time;
   function Get_Duration (R : Reservation) return Duration;

   function Has_Error_Log_Entries (R : Reservation) return Boolean;
   procedure Iterate_Error_Log (R       : Reservation;
                                Process : not null access procedure (Message : String));
   procedure Iterate (Process : not null access procedure (R : Reservation));
   function Same (Left, Right : Reservation) return Boolean;


private
   type Reservation is record
      Number               : Integer; -- Job ID
      Name                 : Unbounded_String; -- Job name
      Owner                : Utils.User_Name; -- User whom this job belongs to
      Group                : Unbounded_String;
      Account              : Unbounded_String;
      State_Array          : State;
      State_String         : String (1 .. 4);
      Start_Time, End_Time : Time;
      Time_Span            : Duration;
      Submission_Time      : Time;    -- when submitted
      Error_Log            : Utils.String_List;

   end record;

   package AR_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Element_Type => Reservation, "=" => Same);

   List : AR_Lists.List;

   procedure Record_Error (R : in out Reservation; Message : String);
   --  Purpose: store an error message for retrieval by the calling application
   --  without raising an exception (so we can resume Library oprations)

end SGE.Advance_Reservations;
