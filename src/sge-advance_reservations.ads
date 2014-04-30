with Ada.Calendar; use Ada.Calendar;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with SGE.Parser;
with SGE.Utils; use SGE.Utils;
with SGE.Jobs; use SGE.Jobs;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Strings.Bounded;
with Ada.Containers.Ordered_Maps;
with SGE.Resources;

package SGE.Advance_Reservations is
   Other_Error : exception;
   type Reservation is private;
   type Queue is private;

   function Get_Name (Q : Queue) return String;
   function Get_Slots (Q : Queue) return String;

   procedure Append_List (Node_Tree : SGE.Parser.Tree);

   function New_Reservation (List : SGE.Parser.Node_List) return Reservation;
   function Get_Name (R : Reservation) return String;
   function Get_ID (R : Reservation) return String;
   function Get_Owner (R : Reservation) return User_Name;
   function Get_Group (R : Reservation) return String;
   function Get_Account (R : Reservation) return String;
   function Get_State (R : Reservation) return String;
   function Get_Start_Time (R : Reservation) return Ada.Calendar.Time;
   function Get_End_Time (R : Reservation) return Ada.Calendar.Time;
   function Get_Submission_Time (R : Reservation) return Ada.Calendar.Time;
   function Get_Duration (R : Reservation) return Duration;
   function Get_Resources (R : Reservation) return Resources.Hashed_List;

   procedure Iterate_Queues (R       : Reservation;
                             Process : not null access procedure (Q : Queue));

   function Has_Error_Log_Entries (R : Reservation) return Boolean;
   procedure Iterate_Error_Log (R       : Reservation;
                                Process : not null access procedure (Message : String));
   procedure Iterate (Process : not null access procedure (R : Reservation));
   function Same (Left, Right : Reservation) return Boolean;

   procedure Append_Queue (R : in out Reservation; Q : String; Slots : Positive);
   procedure Add_Resource (R : in out Reservation; Key, Value : String);
private
   Key_Length : constant Positive := 16;
   package Queue_Keys is
     new Ada.Strings.Bounded.Generic_Bounded_Length (Max => Key_Length);
   type Queue_Key is new Queue_Keys.Bounded_String;
   type Queue is record
      Name : Unbounded_String;
      Slots : Positive;
   end record;

   function To_Key (S : String) return Queue_Key;

   procedure Record_Error (R : in out Reservation; Message : String);
   --  Purpose: store an error message for retrieval by the calling application
   --  without raising an exception (so we can resume Library oprations)

   package Queue_Lists is
      new Ada.Containers.Ordered_Maps (Key_Type => Queue_Key, Element_Type => Queue);

   type Reservation is tagged record
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
      Queues               : Queue_Lists.Map;
      Resource_List        : Resources.Hashed_List;
      Error_Log            : Utils.String_List;

   end record;

   package AR_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Element_Type => Reservation, "=" => Same);

   List : AR_Lists.List;


end SGE.Advance_Reservations;
