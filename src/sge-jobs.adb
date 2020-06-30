with Ada.Calendar;   use Ada.Calendar;
with Ada.Calendar.Conversions;
with SGE.Resources;      use SGE.Resources; use SGE.Resources.Resource_Lists;
with SGE.Ranges;          use SGE.Ranges; use SGE.Ranges.Range_Lists;
with SGE.Utils;          use SGE.Utils; use SGE.Utils.String_Lists; use SGE.Utils.String_Pairs;
with SGE.Parser;
with SGE.Quota;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Real_Time;
with Ada.Strings.Fixed;
with Interfaces.C;
with Ada.Containers; use Ada.Containers;
with Ada.Strings.Maps;
with Ada.Characters.Handling;
with SGE.Context;
use SGE.Context;
with SGE.Taint; use SGE.Taint;

package body SGE.Jobs is
   use Job_Lists;
   use Job_Maps;


   procedure Parse_JAT_Message_List (Message_List : Node; J : in out Job);
   procedure Determine_Balancer_Support (J : in out Job);
   procedure Update_State_Array (J : in out Job);


   procedure Parse_JAT_Task_List
     (J             : in out Job;
      Task_List             : Node);

   procedure Parse_PET_Destinations
     (J             : in out Job;
      PE_Task_Entry : Node);

   procedure Parse_Usage
     (Usage_Data : in out Usage;
      Usage_Tree : Node;
      Cumulative : Boolean);

   procedure Update_Job_From_Overlay (J : in out Job);
   procedure Extract_Generic_Range (Range_Nodes    : Node_List;
                                    Min, Step, Max : out Natural);

   procedure Clear (Collection : in out List) is
   begin
      Collection.Container.Clear;
   end Clear;

   function Count (Collection : List;
                   Predicate : not null access function (J : Job) return Boolean)
                   return Natural is

      Counter : Natural := 0;
      procedure Wrapper (Position : Job_Lists.Cursor) is
      begin
         if Predicate (Element (Position)) then
            Counter := Counter + 1;
         end if;
      end Wrapper;

   begin
      Collection.Container.Iterate (Wrapper'Access);
      return Counter;
   end Count;

   procedure Extract_Generic_Range (Range_Nodes    : Node_List;
                                    Min, Step, Max : out Natural) is
      R : Node;
   begin
      for J in 1 .. Length (Range_Nodes) loop
         R := Item (Range_Nodes, J - 1);
         if Name (R) = "RN_min" then
            Min := Integer'Value (Value (First_Child (R)));
         elsif Name (R) = "RN_max" then
            Max := Integer'Value (Value (First_Child (R)));
         elsif Name (R) = "RN_step" then
            Step := Integer'Value (Value (First_Child (R)));
         end if;
      end loop;
   end Extract_Generic_Range;

   function Get_Task_Count (J : Job) return Natural is
   begin
      return Count (J.Task_IDs);
   end Get_Task_Count;

   function Get_ID (J : Job) return String is
   begin
      return Ada.Strings.Fixed.Trim (Source => J.Number'Img,
                                     Side => Ada.Strings.Left);
   end Get_ID;

   function Get_ID (J : Job) return Positive is
   begin
      return J.Number;
   end Get_ID;

   function Get_PE (J : Job) return Unbounded_String is
   begin
      return J.PE;
   end Get_PE;

   function Get_Granted_PE (J : Job) return Unbounded_String is
   begin
      return J.Granted_PE;
   end Get_Granted_PE;

   function Get_Slot_List (J : Job) return Ranges.Step_Range_List is
   begin
      return J.Slot_List;
   end Get_Slot_List;

   function Get_Slot_Number (J : Job) return Unbounded_String is
   begin
      return J.Slot_Number;
   end Get_Slot_Number;

   function Get_Minimum_Slots (J : Job) return Positive is
   begin
      if Is_Empty (J.Slot_List) then
         return Positive'Value (To_String (J.Slot_Number));
      end if;

      return Min (J.Slot_List);
   end Get_Minimum_Slots;

   function Get_Maximum_Slots (J : Job) return Positive is
   begin
      if Is_Empty (J.Slot_List) then
         return Positive'Value (To_String (J.Slot_Number));
      end if;

      return Max (J.Slot_List);
   end Get_Maximum_Slots;

   function Get_Minimum_CPU_Slots (J : Job) return Positive is
      use Ada.Strings;
      use Ada.Strings.Fixed;
      use Ada.Strings.Maps;
   begin
      if not Supports_Balancer (J, CPU_GPU) then
         return Get_Minimum_Slots (J);
      else
         declare
            Raw_Range : String := Get_CPU_Range (J);
            Separator : Natural := Index (Source => Raw_Range,
                                          Set    => To_Set (" 0123456789"),
                                          --  note leading blank
                                          Test   => Outside);
         begin
            if Separator = 0 then -- Bug #1909
               return Natural'Value (Raw_Range);
            else
               return Natural'Value (Raw_Range (Raw_Range'First .. Separator - 1));
            end if;
         exception
            when Constraint_Error =>
               raise Constraint_Error with "Cannot extract slots from context """ & Raw_Range & """("
                 & Raw_Range'First'Img & ".." & Separator'Img & "-1)";
         end;
      end if;
   end Get_Minimum_CPU_Slots;

   function Get_Maximum_CPU_Slots (J : Job) return Positive is
      use Ada.Strings;
      use Ada.Strings.Fixed;
      use Ada.Strings.Maps;
   begin
      if not Supports_Balancer (J, CPU_GPU) then
         return Get_Maximum_Slots (J);
      else
         declare
            Raw_Range : String := Get_CPU_Range (J);
            Separator : Natural := Index (Source => Raw_Range,
                                          Set    => To_Set (" 0123456789"),
                                          --  note leading blank
                                          Test   => Outside,
                                          Going  => Backward);
         begin
            return Natural'Value (Raw_Range (Separator + 1 .. Raw_Range'Last));
         exception
            when Constraint_Error =>
               raise Constraint_Error with "Cannot extract slots from context """ & Raw_Range & """("
                 & Separator'Img & "+1)" & ".." & Raw_Range'Last'Img;
         end;
      end if;
   end Get_Maximum_CPU_Slots;

   function Get_Queue (J : Job) return Unbounded_String is
   begin
      return J.Queue;
   end Get_Queue;

   function Get_Hard_Resources (J : Job) return Resources.Hashed_List is
   begin
      return J.Hard;
   end Get_Hard_Resources;

   function Get_Hard_Resources (J : Job) return String is
   begin
      return To_String (J.Hard);
   end Get_Hard_Resources;

   function Get_Soft_Resources (J : Job) return String is
   begin
      return To_String (J.Soft);
   end Get_Soft_Resources;

   function Get_Soft_Resources (J : Job) return Resources.Hashed_List is
   begin
      return J.Soft;
   end Get_Soft_Resources;

   function Supports_Balancer (J : Job; What : Balancer_Capability := Any) return Boolean is
   begin
      return J.Balancer (What);
   end Supports_Balancer;

   procedure Determine_Balancer_Support (J : in out Job) is
   begin
      for Capability in Balancer_Capability'Range loop
         J.Balancer (Capability) := False;
      end loop;

      if J.Context.Contains (SGE.Context.Slots_CPU)
        and then J.Context.Contains (Slots_GPU)
      then
         J.Balancer (CPU_GPU) := True;
         J.Balancer (Any) := True;
      end if;
      if J.Context.Contains (Wait_Reduce)
        and then J.Context.Contains (Slots_Reduce)
      then
         J.Balancer (Low_Cores) := True;
         J.Balancer (Any) := True;
      end if;
      if J.Context.Contains (Slots_Extend) then
         J.Balancer (High_Cores) := True;
         J.Balancer (Any) := True;
      end if;
   end Determine_Balancer_Support;

   function Length (Collection : List) return Natural is
   begin
      return Natural (Collection.Container.Length);
   end Length;

   procedure Update_State_Array (J : in out Job) is
      Flag : State_Flag;
      Skip : Boolean;
   begin
      for Flag in J.State_Array'Range loop
         J.State_Array (Flag) := False;
      end loop;

      for Position in J.State_String'Range loop
         Skip := False;
         case J.State_String (Position) is
            when 'r' =>
               Flag := running;
            when 'd' =>
               Flag := deletion;
            when 'E' =>
               Flag := Error;
            when 'h' =>
               Flag := hold;
            when 'R' =>
               Flag := Restarted;
            when 's' =>
               Flag := suspended;
            when 'S' =>
               Flag := Q_Suspended;
            when 't' =>
               Flag := transfering;
            when 'T' =>
               Flag := Threshold;
            when 'q' =>
               Skip := True;
            when 'w' =>
               Flag := waiting;
            when ' ' =>
               Skip := True;
            when others =>
               raise Jobs.Other_Error with "Unknown state character " &
               J.State_String (Position) & " found";
         end case;
         if not Skip then
            J.State_Array (Flag) := True;
         end if;
      end loop;
   end Update_State_Array;

   function Get_Name (J : Job) return String is
   begin
      return Job_Names.To_String (J.Name);
   end Get_Name;

   function Get_Full_Name (J : Job) return String is
   begin
      return To_String (J.Full_Name);
   end Get_Full_Name;

   function Is_Name_Truncated (J : Job) return Boolean is
   begin
      return J.Name_Truncated;
   end Is_Name_Truncated;

   function Get_Owner (J : Job) return User_Name is
   begin
      return J.Owner;
   end Get_Owner;

   function Get_Group (J : Job) return String is
   begin
      return To_String (J.Group);
   end Get_Group;

   function Get_Account (J : Job) return String is
   begin
      return To_String (J.Account);
   end Get_Account;

   function Get_Submission_Time (J : Job) return Ada.Calendar.Time is
   begin
      return J.Submission_Time;
   end Get_Submission_Time;

   function Get_Advance_Reservation (J : Job) return String is
   begin
      return To_String (J.Job_Advance_Reservation);
   end Get_Advance_Reservation;

   function Has_Reserve (J : Job) return Tri_State is
   begin
      return J.Reserve;
   end Has_Reserve;

   function Get_State (J : Job) return String is
   begin
      return Ada.Strings.Fixed.Trim (J.State_String, Ada.Strings.Right);
   end Get_State;

   function Get_Directory (J : Job) return String is
   begin
      return To_String (J.Directory);
   end Get_Directory;

   function Get_Script_File (J : Job) return String is
   begin
      return To_String (J.Script_File);
   end Get_Script_File;

   function Get_Args (J : Job) return String_List is
   begin
      return J.Args;
   end Get_Args;

   function Get_Exec_File (J : Job) return String is
   begin
      return To_String (J.Exec_File);
   end Get_Exec_File;

   function Get_Std_Out_Paths (J : Job) return String_List is
   begin
      return J.Std_Out_Paths;
   end Get_Std_Out_Paths;

   function Get_Std_Err_Paths (J : Job) return String_List is
   begin
      return J.Std_Err_Paths;
   end Get_Std_Err_Paths;

   function Is_Merge_Std_Err (J : Job) return Tri_State is
   begin
      return J.Merge_Std_Err;
   end Is_Merge_Std_Err;

   function Has_Notify (J : Job) return Tri_State is
   begin
      return J.Notify;
   end Has_Notify;

   function Get_Task_List (J : Job) return String_Sets.Set is
   begin
      return J.Task_List;
   end Get_Task_List;

   function Get_Detected_Queues (J : Job) return String_Sets.Set is
   begin
      return J.Detected_Queues;
   end Get_Detected_Queues;

   function Get_Priority (J : Job) return Utils.Fixed is
   begin
      return J.Priority;
   end Get_Priority;

   function Get_Project (J : Job) return String is
   begin
      return To_String (J.Project);
   end Get_Project;

   function Get_Override_Tickets (J : Job) return Natural is
   begin
      return J.Override_Tickets;
   end Get_Override_Tickets;

   function Get_Share_Tickets (J : Job) return Natural is
   begin
      return J.Share_Tickets;
   end Get_Share_Tickets;

   function Get_Functional_Tickets (J : Job) return Natural is
   begin
      return J.Functional_Tickets;
   end Get_Functional_Tickets;

   function Get_Urgency (J : Job) return Fixed is
   begin
      return J.Urgency;
   end Get_Urgency;

   function Get_Resource_Contrib (J : Job) return Natural is
   begin
      return J.Resource_Contrib;
   end Get_Resource_Contrib;

   function Get_Waiting_Contrib (J : Job) return Natural is
   begin
      return J.Waiting_Contrib;
   end Get_Waiting_Contrib;

   function Get_Posix_Priority (J : Job) return Posix_Priority_Type is
   begin
      return J.Posix_Priority;
   end Get_Posix_Priority;

   function Get_JAT_Usage  (J : Job) return Usage is
   begin
      return J.JAT_Usage;
   end Get_JAT_Usage;

   function Get_PET_Usage  (J : Job) return Usage is
   begin
      return J.PET_Usage;
   end Get_PET_Usage;

   function Get_CPU (J : Job) return Float is
   begin
      return J.CPU;
   end Get_CPU;

   function Get_Mem (J : Job) return Float is
   begin
      return J.Mem;
   end Get_Mem;

   function Get_IO (J : Job) return Float is
   begin
      return J.IO;
   end Get_IO;

   function Get_Context (J : Job; Key : Context.Key_Type) return String is
   begin
      return J.Context.Get (Key);
   end Get_Context;

   function Has_Context (J : Job; Key : Context.Key_Type) return Boolean is
   begin
      return Context.Contains (J.Context, Key);
   end Has_Context;

   function Has_Context (J : Job) return Boolean is
   begin
      return not J.Context.Is_Empty;
   end Has_Context;


   function Get_Task_IDs (J : Job) return Ranges.Step_Range_List is
   begin
      return J.Task_IDs;
   end Get_Task_IDs;

   ----------------
   --  list-related
   ----------------

   function Find_Job (Collection : List; ID : Natural) return Job_Lists.Cursor is
      Pos : Job_Lists.Cursor := Collection.Container.First;
      The_Job : Job;
   begin
      while Pos /= Job_Lists.No_Element loop
         The_Job := Job_Lists.Element (Pos);
         if The_Job.Number = ID then
            return Pos;
         end if;
         Next (Pos);
      end loop;
      return Job_Lists.No_Element;
   end Find_Job;

   function Find_Job (Collection : List; ID : Natural) return Job is
   begin
      return Element (Find_Job (Collection, ID));
   end Find_Job;

   procedure Sort (Collection : in out List) is
   begin
      Sorting_By_Resources.Sort (Collection.Container);
   end Sort;

   function Empty (Collection : List) return Boolean is
   begin
      return Collection.Container.Is_Empty;
   end Empty;

   procedure Get_Summary (Collection   : List;
                          Tasks, Slots : out State_Count) is

      procedure Count (Position : Job_Lists.Cursor) is
         State_Array : State := Job_Lists.Element (Position).State_Array;
         Slot_Number : Natural := Integer'Value (To_String (Job_Lists.Element (Position).Slot_Number));
      begin
         for Flag in State_Array'Range loop
            if State_Array (Flag) then
               if Flag = waiting and then State_Array (hold) then
                  null; -- we see a job as either waiting or held
                        --  but not both, see Bug #2478
               else
                  Tasks (Flag) := Tasks (Flag) + 1;
                  Slots (Flag) := Slots (Flag) + Slot_Number;
               end if;
            end if;
         end loop;
      end Count;

   begin
      for S in Tasks'Range loop
         Tasks (S) := 0;
         Slots (S) := 0;
      end loop;
      Collection.Container.Iterate (Process => Count'Access);
   end Get_Summary;

   function Get_Last_Migration (J : Job) return Time is
   begin
      if J.Context.Contains (Last_Migration) then
         return Ada.Calendar.Conversions.To_Ada_Time
           (Interfaces.C.long'Value (J.Context.Get (Last_Migration)));
      else
         raise Constraint_Error;
      end if;
   end Get_Last_Migration;

   function Get_Last_Reduction (J : Job) return Time is
   begin
      if J.Context.Contains (Last_Reduction) then
         return Ada.Calendar.Conversions.To_Ada_Time
           (Interfaces.C.long'Value (J.Context.Get (Last_Reduction)));
      else
         raise Constraint_Error;
      end if;
   end Get_Last_Reduction;

   function Get_Last_Extension (J : Job) return Time is
   begin
      if J.Context.Contains (Last_Extension) then
         return Ada.Calendar.Conversions.To_Ada_Time
           (Interfaces.C.long'Value (J.Context.Get (Last_Extension)));
      else
         raise Constraint_Error;
      end if;
   end Get_Last_Extension;

   function Get_Reduce_Wait (J : Job) return Natural is
   begin
      if J.Context.Contains (Wait_Reduce) then
         return Integer'Value (J.Context.Get (Wait_Reduce));
      else
         raise Constraint_Error;
      end if;
   end Get_Reduce_Wait;

   function Get_Reduced_Runtime (J : Job) return String is
   begin
      return J.Context.Get (Reduced_Runtime);
   end Get_Reduced_Runtime;

   function Get_Reduced_Slots (J : Job) return String is
   begin
      if J.Context.Contains (Slots_Reduce) then
         return J.Context.Get (Slots_Reduce);
      else
         raise Constraint_Error;
      end if;
   end Get_Reduced_Slots;

   function Get_Extended_Slots (J : Job) return String is
   begin
      if J.Context.Contains (Slots_Extend) then
         return J.Context.Get (Slots_Extend);
      else
         raise Constraint_Error;
      end if;
   end Get_Extended_Slots;

   function Get_CPU_Range (J : Job) return String is
   begin
      if J.Context.Contains (Slots_CPU) then
         return Ranges.To_String (Ranges.To_Step_Range_List
                                  (J.Context.Get (Slots_CPU)),
                                 Short => True);
      else
         raise Unsupported_Error;
      end if;
   end Get_CPU_Range;

   function Get_GPU_Range (J : Job) return String is
   begin
      if J.Context.Contains (Slots_GPU) then
         return Ranges.To_String (Ranges.To_Step_Range_List (
                                  J.Context.Get (Slots_GPU)),
                                 Short => True);
      else
         raise Constraint_Error;
      end if;
   end Get_GPU_Range;

   function To_String (Capability : Balancer_Capability) return String is
   begin
      return Ada.Characters.Handling.To_Lower (Capability'Img);
   end To_String;

   --------------
   -- To_State --
   --------------

   function To_Memory (Amount : Usage_Integer) return String is
   begin
      if Amount > 2 ** 34 then
         return Usage_Integer'Image (Amount / 2 ** 30) & " GB";
      elsif Amount > 2 ** 24 then
         return Usage_Integer'Image (Amount / 2 ** 20) & " MB";
      else
         return Usage_Integer'Image (Amount / 2** 10) & " kB";
      end if;
   end To_Memory;


   -------------
   -- On_Hold --
   -------------

   function On_Hold (J : Job) return Boolean is
   begin
      return J.State_Array (hold);
   end On_Hold;

   ---------------
   -- Has_Error --
   ---------------

   function Has_Error (J : Job) return Boolean is
   begin
      return J.State_Array (Error);
   end Has_Error;

   function Is_Running (J : Job) return Boolean is
   begin
      return J.State_Array (running);
   end Is_Running;

   function Quota_Inhibited (J : Job) return Boolean is
   begin
      return J.RQS_Reached;
   end Quota_Inhibited;


   function Has_Error_Log_Entries (J : Job) return Boolean is
   begin
      return Has_Errors (J);
   end Has_Error_Log_Entries;

   --------------
   -- End_Time --
   --------------

   function End_Time (J : Job) return Time is
   begin
      return J.Submission_Time
        + Ada.Real_Time.To_Duration (Ada.Real_Time.Seconds (
        J.Hard.Numerical ("h_rt")));
   exception
      when Constraint_Error =>
         raise Resource_Error with "Unable to compute end time";
   end End_Time;

   --------------------
   -- Remaining_Time --
   --------------------

   function Remaining_Time (J : Job) return Duration is
   begin
      return End_Time (J) - Ada.Calendar.Clock;
   end Remaining_Time;

   function Walltime (J : Job) return Duration is
   begin
--      return Ada.Real_Time.To_Duration (Ada.Real_Time.Seconds (
--                                        J.Hard.Numerical ("h_rt")));
--  This does not work because qstat -urg does not return resource info
      if J.Is_Running then
         return Ada.Calendar.Clock - J.Submission_Time;
      else
         return Duration (0);
      end if;
   exception
      when Constraint_Error =>
         return Duration (0);
   end Walltime;

   -----------------
   -- Append_List --
   -----------------

   procedure Append (Collection     : in out List;
                     Nodes          : Node_List;
                     Fix_Posix_Prio : Boolean) is
      N : Node;
   begin
      for Index in 1 .. Length (Nodes) loop
         N := Item (Nodes, Index - 1);
         if Name (N) /= "#text" then
            Collection.Container.Append (New_Job (Child_Nodes (N),
                                                  Fix_Posix_Prio));
         end if;
      end loop;
   end Append;

   procedure Update_Messages (Collection : in out List; Nodes : Node_List) is
      MES_Part : Node;
      Number : Natural;
      Message : Unbounded_String;
      Message_Nodes : Node_List;

      procedure Store_Message (Item : in out Job) is
      begin
         Add_Message (Item, Number, Message);
      end Store_Message;
   begin
      if Length (Collection) = 1 then
         Elements :
         for Index in 1 .. Length (Nodes) loop
            if Name (Item (Nodes, Index - 1)) = "element" then
               Message_Nodes := Child_Nodes (Item (Nodes, Index - 1));
               MES_Parts :
               for Part_Index in 1 .. Length (Message_Nodes) loop
                  MES_Part := Item (Message_Nodes, Part_Index - 1);
                  if Name (MES_Part) = "MES_message_number" then
                     Number := Integer'Value (Value (First_Child (MES_Part)));
                  elsif Name (MES_Part) = "MES_message" then
                     Message := To_Unbounded_String (Value (First_Child (MES_Part)));
                  end if;
               end loop MES_Parts;
               Collection.Container.Update_Element (Position => Collection.Container.First,
                                                    Process  => Store_Message'Access);
            end if;
         end loop Elements;
      elsif not Is_Empty (Collection.Container) then
         raise Too_Many_Jobs_Error;
      end if;
   end Update_Messages;


   procedure Prune (Collection : in out List;
                    PE, Queue, Hard_Requests,
                    Soft_Requests,
                    Slot_Number, Slot_Ranges : Unbounded_String) is
      --  FIXME: implement in terms of the new kernel-based Prune_List below
      Position : Job_Lists.Cursor := Collection.Container.First;
      J : Job;
      Pruned_List : Job_Lists.List;
   begin
      while Has_Element (Position) loop
         J := Element (Position);
         if J.PE = PE and then
           J.Queue = Queue and then
           J.Hard.Hash = Hard_Requests and then
           J.Soft.Hash = Soft_Requests
         then
            Update_Job_From_Overlay (J);
            if Slot_Ranges = Null_Unbounded_String or else -- Bug #1610
              Hash_Type'Value (To_String (Slot_Ranges)) = 0
            then
               --  checking against a string (i.e. " 0") would be too brittle,
               --  since any change in leading blanks would break this code
               if J.Slot_Number = Slot_Number then
                  Pruned_List.Append (J);
               end if;
            else
               if Hash (J.Slot_List) = Slot_Ranges then
                  Pruned_List.Append (J);
               end if;
            end if;
         end if;
         Next (Position);
      end loop;
      Clear (Collection);
      Collection.Container.Splice (Source => Pruned_List,
                                   Before => Collection.Container.First);
   end Prune;

   procedure Prune (Collection : in out List;
                    Keep       : not null access function (J : Job) return Boolean) is
      Pruned_List : Job_Lists.List;
      procedure Selector (J : Job) is
      begin
         if Keep (J) then
            Pruned_List.Append (J);
         end if;
      end Selector;

   begin
      Iterate (Collection, Selector'Access);
      Clear (Collection);
      Collection.Container.Splice (Source => Pruned_List,
                                   Before => Collection.Container.First);
   end Prune;

   -------------------
   -- Update_Status --
   -------------------

   procedure Update_Status (Collection : in out List) is
      Pos : Job_Lists.Cursor;
   begin
      Pos := Collection.Container.First;
      while Pos /= Job_Lists.No_Element loop
         Collection.Container.Update_Element (Position => Pos,
                                              Process  => Update_Status'Access);
         Next (Pos);
      end loop;
   end Update_Status;

   procedure Update_Status (J : in out Job) is
      SGE_Out     : Tree;
      Nodes       : Node_List;
      Field_Nodes : Node_List;
      Field       : Node;
      Number      : Positive;
      State       : String (1 .. 4);
      Found       : Boolean := False;

      use Ada.Strings.Fixed;
   begin
      SGE_Out := Parser.Setup (Command => Cmd_Qstat,
                               Selector => Implicit_Trust ("-u ")
                               & Sanitise (Trim (To_String (J.Owner), Ada.Strings.Right)));

      --  Fetch Jobs
      Nodes := Parser.Get_Elements_By_Tag_Name (SGE_Out, "job_list");

      Jobs :
      for Index in 1 .. Length (Nodes) loop
         Field_Nodes := Child_Nodes (Item (Nodes, Index - 1));
         Fields :
         for Field_Index in 1 .. Length (Field_Nodes) loop
            Field := Item (Field_Nodes, Field_Index - 1);
            if Name (Field) = "JB_job_number" then
               Number := Integer'Value (Value (First_Child (Field)));
            elsif Name (Field) = "state" then
               State := Head (Value (First_Child (Field)), State'Length);
               Found := True;
            end if;
         end loop Fields;
         if not Found then
            raise Missing_Tag_Error with "No state found for Job" & Number'Img;
         end if;
         if Number = J.Number then
            J.State_String := State;
            Update_State_Array (J);
            exit Jobs;
         end if;
      end loop Jobs;

      Parser.Free;
   end Update_Status;

   -------------------
   -- Search_Queues --
   -------------------

   procedure Search_Queues (Collection : in out List) is
      SGE_Out                : Tree;
      Job_Nodes, Value_Nodes : Node_List;
      Job_Node, Value_Node   : Node;
      A                      : Attr;
      Pos                    : Job_Lists.Cursor;
      Number_Found           : Natural;
      The_Task               : Natural;
      The_Queue              : Unbounded_String;
      Found                  : Boolean := False;

      procedure Record_Queue (Element : in out Job) is
         Success : Boolean;
         Where : String_Sets.Cursor;
      begin
         if The_Task = 0 then
            Element.Detected_Queues.Insert (New_Item => The_Queue,
                                            Position => Where,
                                            Inserted => Success);
         else
            Element.Detected_Queues.Insert (New_Item => The_Task'Img & ": " &
                                              The_Queue,
                                            Position => Where,
                                            Inserted => Success);
         end if;
      end Record_Queue;

   begin
      SGE_Out := Parser.Setup (Command  => Cmd_Qhost,
                               Selector => Implicit_Trust ("-j"));

      --  Fetch Jobs
      Job_Nodes := Parser.Get_Elements_By_Tag_Name (SGE_Out, "job");
      for I in 0 .. Length (Job_Nodes) - 1 loop
         Job_Node := Item (List  => Job_Nodes,
                           Index => I);
         A := Get_Attr (Job_Node, "name");
         Number_Found := Integer'Value (Value (A));
         Pos := Find_Job (Collection => Collection, ID => Number_Found);
         --  in theory, this scaling is less than ideal:
         --  O(number of jobs in Job_List times number of jobs returned by qhost)
         --  however, in the present implementation, there is only one job in the list,
         --  so this should not be a problem
         --  if we ever change the code to produce more than one or two jobs at this
         --  point, we should change the job list to a map with the ID as a key
         if Pos /= Job_Lists.No_Element then
            Value_Nodes := Child_Nodes (Job_Node);
            The_Task := 0;
            for J in 0 .. Length (Value_Nodes) - 1 loop
               Value_Node := Item (List  => Value_Nodes,
                                   Index => J);
               if Name (Value_Node) = "jobvalue" then
                  A := Get_Attr (Value_Node, "name");
                  if Value (A) = "qinstance_name" then
                     The_Queue := To_Unbounded_String (Value (First_Child (Value_Node)));
                     Found := True;
                  elsif Value (A) = "taskid" then
                     The_Task := Integer'Value (Value (First_Child (Value_Node)));
                  end if;
               end if;
            end loop;
            if not Found then
               raise Missing_Tag_Error with "no qinstance_name";
            end if;
            Collection.Container.Update_Element (Position => Pos,
                                                 Process  => Record_Queue'Access);
         end if;
      end loop;
      Parser.Free;
   end Search_Queues;

   -------------
   -- New_Job --
   -------------

   function New_Job (List : Node_List; Fix_Posix_Prio : Boolean) return Job is
      J           : Job;
   begin
      J.Merge_Std_Err := Undecided;
      J.Reserve := Undecided;
      J.Mem := 0.0;
      J.IO := 0.0;
      J.CPU := 0.0;
      Update_Job (J => J, List => List, Fix_Posix_Prio => Fix_Posix_Prio);
      Determine_Balancer_Support (J);
      return J;
   end New_Job;

   ----------------
   -- Update_Job --
   ----------------

   procedure Update_Job (J              : in out Job;
                         List           : Node_List;
                         Fix_Posix_Prio : Boolean) is
      A           : Attr;
      Inserted    : Boolean;
      Inserted_At : Resource_Lists.Cursor;

      use Ada.Strings.Fixed;
   begin
      for Index in 0 .. Length (List) - 1 loop
         declare
            C : Node := Item (List, Index);
            Node_Name : String := Name (C);
            Raw_Priority : Integer;
         begin
            if Node_Name = "JB_job_number" then
               J.Number := Integer'Value (Value (First_Child (C)));
            elsif Node_Name = "JAT_prio" then
               J.Priority := Fixed'Value (Value (First_Child (C)));
            elsif Node_Name = "JB_name" or else
              Node_Name = "JB_job_name"
            then
               J.Full_Name := To_Unbounded_String (Value (First_Child (C)));
               J.Name := Job_Names.To_Bounded_String (Source => Value (First_Child (C)),
                                                      Drop   => Ada.Strings.Right);
               if Length (J.Full_Name) > Max_Name_Length then
                  J.Name_Truncated := True;
               else
                  J.Name_Truncated := False;
               end if;
            elsif Node_Name = "JB_owner" then
               J.Owner := To_User_Name (Value (First_Child (C)));
            elsif Node_Name = "state" then
               J.State_String := Head (Value (First_Child (C)), J.State_String'Length);
               Update_State_Array (J);
            elsif Node_Name = "JB_submission_time" then
               J.Submission_Time := To_Time (Value (First_Child (C)));

            elsif Node_Name = "JAT_start_time" then
               J.Submission_Time := To_Time (Value (First_Child (C)));
            elsif Node_Name = "queue_name" then
               null; -- ignore
            elsif Node_Name = "slots" then
               J.Slot_Number := To_Unbounded_String (Value (First_Child (C)));
            elsif Node_Name = "ftickets" then
               J.Functional_Tickets := Integer'Value (Value (First_Child (C)));
            elsif Node_Name  = "stickets" then
               J.Share_Tickets := Integer'Value (Value (First_Child (C)));
            elsif Node_Name  = "otickets" then
               J.Override_Tickets := Integer'Value (Value (First_Child (C)));
            elsif Node_Name  = "cpu_usage" then
               J.CPU := Float'Value (Value (First_Child (C)));
            elsif Node_Name  = "mem_usage" then
               J.Mem := Float'Value (Value (First_Child (C)));
            elsif Node_Name  = "io_usage" then
               J.IO := Float'Value (Value (First_Child (C)));
            elsif Node_Name  = "JB_wtcontr" then
               J.Waiting_Contrib := Integer'Value (Value (First_Child (C)));
            elsif Node_Name  = "JB_rrcontr" then
               J.Resource_Contrib := Integer (Float'Value (Value (First_Child (C))));
            elsif Node_Name  = "JB_nurg" then
               J.Urgency := Fixed'Value (Value (First_Child (C)));
            elsif Node_Name  = "JB_priority" then
               Raw_Priority := Integer'Value (Value (First_Child (C)));
               if Fix_Posix_Prio then
                  Raw_Priority := Raw_Priority - 1_024;
               end if;
               J.Posix_Priority := Posix_Priority_Type (Raw_Priority);
            elsif Node_Name  = "hard_req_queue" then
               J.Queue := To_Unbounded_String (Value (First_Child (C)));
            elsif Node_Name  = "full_job_name" then
               null; -- ignore
            elsif Node_Name  = "requested_pe" then
               A := Get_Attr (C, "name");
               J.PE := To_Unbounded_String (Value (A));
            elsif Node_Name  = "hard_request" then
               A := Get_Attr (C, "name");
               J.Hard.Insert (Key      => To_Unbounded_String (Value (A)),
                              New_Item => New_Resource (Name  => Value (A),
                                                        Value => Value (First_Child (C))),
                              Position => Inserted_At,
                              Inserted => Inserted);
            elsif Node_Name = "soft_request" then
               A := Get_Attr (C, "name");
               J.Soft.Insert (Key      => To_Unbounded_String (Value (A)),
                              New_Item => New_Resource (Name  => Value (A),
                                                        Value => Value (First_Child (C))),
                              Position => Inserted_At,
                              Inserted => Inserted);
            elsif Node_Name  = "predecessor_jobs" or else
              Node_Name  = "ad_predecessor_jobs"
            then
               J.Predecessors.Include (New_Item => Natural'Value (Value (First_Child (C))));
            elsif Node_Name = "predecessor_jobs_req" or else
              Node_Name = "ad_predecessor_jobs_req"
            then
               J.Predecessor_Request.Append (To_Unbounded_String (Value (First_Child (C))));
            elsif Node_Name = "JB_hard_resource_list" then
               Extract_Resource_List (J, Child_Nodes (C));
            elsif Node_Name  = "JB_soft_resource_list" then
               Extract_Resource_List (J, Child_Nodes (C), Soft => True);
            elsif Node_Name  = "JB_hard_queue_list" then
               Extract_Queue_List (J, Child_Nodes (C));
            elsif Node_Name  = "JB_ja_tasks" then
               Extract_Tasks (J, Child_Nodes (C));
            elsif Node_Name  = "JB_pe_range" then
               Extract_PE_Range (J, Child_Nodes (C));

            elsif Node_Name  = "JB_department" then
               J.Department := To_Unbounded_String (Value (First_Child (C)));
            elsif Node_Name  = "JB_project" then
               if Length (Child_Nodes (C)) > 0 then
                  J.Project := To_Unbounded_String (Value (First_Child (C)));
               else
                  J.Project := Null_Unbounded_String;
               end if;
            elsif Node_Name  = "JB_ar" then
               J.Job_Advance_Reservation := To_Unbounded_String (Value (First_Child (C)));
            elsif Node_Name = "JB_ja_structure" then
               Extract_Array (J, Child_Nodes (C));  -- to array
            elsif Node_Name  = "JB_exec_file" then
               J.Exec_File := To_Unbounded_String (Value (First_Child (C)));
            elsif Node_Name  = "JB_group" then
               J.Group := To_Unbounded_String (Value (First_Child (C)));
            elsif Node_Name  = "JB_merge_stderr" then
               J.Merge_Std_Err := To_Tri_State (Value (First_Child (C)));
            elsif Node_Name = "JB_stdout_path_list" then
               Extract_Paths (J.Std_Out_Paths, Child_Nodes (C));
            elsif Node_Name = "JB_stderr_path_list" then
               Extract_Paths (J.Std_Err_Paths, Child_Nodes (C));
            elsif Node_Name = "JB_script_file" then
               J.Script_File := To_Unbounded_String (Value (First_Child (C)));
            elsif Node_Name = "JB_cwd" then
               J.Directory := To_Unbounded_String (Value (First_Child (C)));
            elsif Node_Name = "JB_reserve" then
               J.Reserve := To_Tri_State (Value (First_Child (C)));
            elsif Node_Name = "JB_pe" then
               J.PE := To_Unbounded_String (Value (First_Child (C)));
            elsif Node_Name = "JB_notify" then
               J.Notify := To_Tri_State (Value (First_Child (C)));
            elsif Node_Name = "JB_account" then
               J.Account := To_Unbounded_String (Value (First_Child (C)));
            elsif Node_Name = "JB_job_args" then
               Extract_Args (J, Child_Nodes (C));
            elsif Node_Name = "tasks" then
               J.Task_IDs := To_Step_Range_List (Value (First_Child (C)));
            elsif Node_Name = "granted_pe" then
               J.Granted_PE := To_Unbounded_String (Value (First_Child (C)));
            elsif Node_Name = "JB_jid_predecessor_list" then
               Extract_Hold_ID_List (J.Predecessors, Child_Nodes (C));
            elsif Node_Name = "JB_jid_successor_list" then
               Extract_Hold_ID_List (J.Successors, Child_Nodes (C));
            elsif Node_Name = "JB_context" then
               Extract_Context (J.Context, Child_Nodes (C));
            elsif Node_Name = "JB_urg" or else
              Node_Name = "JB_dlcontr" or else
              Node_Name = "JAT_ntix" or else
              Node_Name = "JAT_share" or else
              Node_Name = "JB_jobshare" or else
              Node_Name = "JB_jid_request_list" or else
              Node_Name = "tickets" or else
              Node_Name = "JB_nppri" or else
              Node_Name = "JB_uid" or else
              Node_Name = "JB_gid" or else
              Node_Name = "JB_mail_list" or else
              Node_Name = "JB_mail_options" or else
              Node_Name = "JB_deadline" or else
              Node_Name = "JB_shell_list" or else
              Node_Name = "JB_env_list" or else
              Node_Name = "JB_checkpoint_attr" or else
              Node_Name = "JB_checkpoint_interval" or else
              Node_Name = "JB_verify" or else
              Node_Name = "JB_restart" or else
              Node_Name = "JB_soft_wallclock_gmt" or else
              Node_Name = "JB_hard_wallclock_gmt" or else
              Node_Name = "JB_execution_time" or else
              Node_Name = "JB_script_size" or else
              Node_Name = "JB_version" or else
              Node_Name = "JB_type" or else
              Node_Name = "JB_verify_suitable_queues" or else
              Node_Name = "JB_override_tickets"
            then
               null;

            elsif Node_Name /= "#text" then
               raise Other_Error with "Unknown Field: " & Node_Name;
            end if;
         exception
               when E : Parser_Error =>
                  Record_Error (J, "information incomplete: " & Exception_Message (E));
               when E : others =>
                  Record_Error (J, "While parsing job: " & Exception_Message (E)
                                    & "Node type: """ & Node_Name
                             & """ Value: """ & Value (First_Child (C)) & """");
         end;
      end loop;

      if J.Queue = "" then
         J.Queue := To_Unbounded_String ("*");
      end if;

   exception
      when E : others =>
         raise Other_Error with Exception_Message (E);
   end Update_Job;

   ---------------------------
   -- Extract_Resource_List --
   ---------------------------

   procedure Extract_Resource_List (J              : in out Job;
                                    Resource_Nodes : Node_List;
                                    Soft : Boolean := False) is
      Resource_Tags      : Node_List;
      Res_Value          : Unbounded_String;
      Res_Name           : Unbounded_String;
      Res_Bool           : Boolean;
      Res_State          : Tri_State;
      Inserted           : Boolean;
      Inserted_At        : Resource_Lists.Cursor;

   begin
      for I in 1 .. Length (Resource_Nodes) loop
         declare
            Found_Name, Found_Value : Boolean := False;
            N                       : Node := Item (Resource_Nodes, I - 1);
            Node_Name               : String := Name (N);
         begin
            if Node_Name = "qstat_l_requests"
              or else Node_Name = "element"
            then
               Res_Bool := False;
               Res_State := Undecided;
               Resource_Tags := Child_Nodes (N);
               for J in 1 .. Length (Resource_Tags) loop
                  declare
                     R : Node := Item (Resource_Tags, J - 1);
                     Subnode_Name : String := Name (R);
                  begin
                     if Subnode_Name = "CE_name" then
                        Res_Name := To_Unbounded_String (Value (First_Child (R)));
                        Found_Name := True;
                     elsif Subnode_Name = "CE_stringval" then
                        Res_Value := To_Unbounded_String (Value (First_Child (R)));
                        Found_Value := True;
                     elsif Subnode_Name = "CE_valtype" and then
                       Value (First_Child (R)) = "5"
                     then
                        Res_Bool := True;
                        --  maybe check for relop here?
                     end if;
                  end;
               end loop;
               if Res_Bool then
                  if Res_Value = "TRUE" or else
                    Res_Value = "true" or else
                    Res_Value = "1"
                  then
                     Res_State := True;
                  elsif Res_Value = "FALSE" or else
                    Res_Value = "false" or else
                    Res_Value = "0"
                  then
                     Res_State := False;
                  else
                     raise Constraint_Error
                       with  """" & To_String (Res_Value) & """ is not boolean";
                  end if;
               end if;
               if not Found_Name then
                  raise Missing_Tag_Error with "resource name not found";
               end if;
               if not Found_Value then
                  raise Missing_Tag_Error with "resource value not found";
               end if;
               if Soft then
                  J.Soft.Insert (Key      => Res_Name,
                              New_Item => New_Resource (Name  => To_String (Res_Name),
                                                        Value => Res_Value,
                                                        Boolean_Valued => Res_Bool,
                                                        State => Res_State),
                              Position => Inserted_At,
                              Inserted => Inserted);
               else
                  J.Hard.Insert (Key      => Res_Name,
                              New_Item => New_Resource (Name  => To_String (Res_Name),
                                                        Value => Res_Value,
                                                        Boolean_Valued => Res_Bool,
                                                        State => Res_State),
                              Position => Inserted_At,
                              Inserted => Inserted);
               end if;
            end if;
         end;
      end loop;
   end Extract_Resource_List;

   ------------------
   -- Extract_Args --
   ------------------

   procedure Extract_Args (J : in out Job;
                           Arg_Nodes : Node_List) is
      N, ST : Node;
      Sub_Elements : Node_List;
   begin
      for I in 1 .. Length (Arg_Nodes) loop
         N := Item (Arg_Nodes, I - 1);
         if Name (N) = "element" then
            Sub_Elements := Child_Nodes (N);
            if Length (Sub_Elements) < 2 then
               raise Assumption_Error with "too few sub-elements";
            end if;
            ST := Item (Sub_Elements, 1);
            if Name (ST) /= "ST_name" then
               raise Assumption_Error with "Expected ""ST_name"" but found """
                 & Name (ST) & """ instead";
            else
               J.Args.Append (New_Item => To_Unbounded_String (Value (First_Child (ST))));
            end if;
         end if;

      end loop;
   end Extract_Args;

   ------------------------------
   -- Extract_Predecessor_List --
   ------------------------------

   procedure Extract_Hold_ID_List (ID_List         : in out Utils.ID_List;
                                   Sub_Nodes       : Node_List) is
      N, ID_Node : Node;
   begin
      for I in 0 .. Length (Sub_Nodes) - 1 loop
         N := Item (Sub_Nodes, I);
         if Name (N) = "job_predecessors" or else
           Name (N) = "ulong_sublist"
         then
            ID_Node := Item (Child_Nodes (N), 1);
            if Name (ID_Node) /= "JRE_job_number" then
               raise Assumption_Error with
                 "Expected ""JRE_job_number"" but found """
                 & Name (ID_Node) & """ instead";
            end if;
            ID_List.Include (New_Item => Natural'Value (Value (First_Child (ID_Node))));
         end if;
      end loop;
   end Extract_Hold_ID_List;

   ------------------------
   -- Extract_Queue_List --
   ------------------------

   procedure Extract_Queue_List (J : in out Job; Destin_Nodes : Node_List) is
      QR_Nodes     : Node_List;
      N, Q         : Node;
   begin
      for I in 1 .. Length (Destin_Nodes) loop
         N := Item (Destin_Nodes, I - 1);
         if Name (N) = "destin_ident_list" or else
           Name (N) = "element"
         then
            QR_Nodes := Child_Nodes (N);
            for K in 1 .. Length (QR_Nodes) loop
               Q := Item (QR_Nodes, K - 1);
               if Name (Q) = "QR_name" then
                  J.Queue_List.Append (To_Unbounded_String (Value (First_Child (Q))));
                  if J.Queue = Null_Unbounded_String then
                     J.Queue := To_Unbounded_String (Value (First_Child (Q)));
                  end if;
               end if;
            end loop;
         end if;
      end loop;
   end Extract_Queue_List;

   procedure Extract_Array (J : in out Job; Task_Nodes : Node_List) is
      Task_Min, Task_Max, Task_Step  : Natural;
      N  : Node;
   begin
      for I in 1 .. Length (Task_Nodes) loop
         N := Item (Task_Nodes, I - 1);
         if Name (N) = "task_id_range" then
            Extract_Generic_Range (Range_Nodes => Child_Nodes (N),
                                   Min         => Task_Min,
                                   Max         => Task_Max,
                                   Step        => Task_Step);
            J.Array_Tasks.Append (Ranges.New_Range (Min => Task_Min,
                                                    Max => Task_Max,
                                                    Step => Task_Step));
         elsif Name (N) /= "#text" then
            raise Missing_Tag_Error with "found """ & Name (N) & """ instead of task_id_range";
         end if;
      end loop;
   end Extract_Array;

   ----------------------
   -- Extract_PE_Range --
   ----------------------

   procedure Extract_PE_Range (J : in out Job; Children : Node_List) is
      N                             : Node;
      Slots_Min, Slots_Step, Slots_Max : Natural;
   begin
      for I in 1 .. Length (Children) loop
         N := Item (Children, I - 1);
         if Name (N) = "ranges" or else
           Name (N) = "element"
         then
               Extract_Generic_Range (Range_Nodes => Child_Nodes (N),
                                      Min         => Slots_Min,
                                      Max         => Slots_Max,
                                      Step        => Slots_Step);
            J.Slot_List.Append (Ranges.New_Range (Min  => Slots_Min,
                                                       Max  => Slots_Max,
                                                       Step => Slots_Step));
         end if;
      end loop;
   end Extract_PE_Range;

   procedure Parse_Usage
     (Usage_Data : in out Usage;
      Usage_Tree : Node;
      Cumulative : Boolean)
   is
      Usage_Entries, Quantity_Nodes : Node_List;
      Usage_Entry, Quantity_Field   : Node;
      Usage_Kind                    : Usage_Type;
      Usage_Value                   : Usage_Number;
   begin
      Usage_Entries := Child_Nodes (Usage_Tree);
      Over_Usage_Entries :
      for I in 0 .. Length (Usage_Entries) - 1 loop
         Usage_Entry := Item (Usage_Entries, I);
         if Name (Usage_Entry) = "element" or else
           Name (Usage_Entry) = "scaled"
         then
            Quantity_Nodes := Child_Nodes (Usage_Entry);
            begin
               Over_Quantity_Nodes :
               for K in 0 .. Length (Quantity_Nodes) - 1 loop
                  Quantity_Field := Item (Quantity_Nodes, K);
                  if Name (Quantity_Field) = "UA_name" then
                     Usage_Kind := Usage_Type'Value (Value (First_Child (Quantity_Field)));
                  elsif Name (Quantity_Field) = "UA_value" then
                     Usage_Value := Usage_Number'Value (Value (First_Child (Quantity_Field)));
                  end if;
               end loop Over_Quantity_Nodes;
               if Cumulative then
                  Usage_Data (Usage_Kind) := Usage_Data (Usage_Kind) + Usage_Value;
               else
                  Usage_Data (Usage_Kind) := Usage_Value;
               end if;
               exception
               when E : Constraint_Error =>
                  declare
                     Quantity : String := Value (First_Child (Quantity_Field));
                  begin
                     if Quantity'Length >= 13
                       and then Quantity (Quantity'First .. Quantity'First + 12) = "binding_inuse"
                     then
                        null; -- ignore; binding has nothing to do with usage, and
                              --  the format is utterly insane;
                              --  there is no way we can handle this crappy xml without
                              --  writing equally crappy code, and it is not that important
                     elsif Quantity'Length >= 5
                       and then (Quantity (Quantity'First .. Quantity'First + 2) = "ru_"
                                 or else Quantity (Quantity'First .. Quantity'First + 4) = "acct_")
                     then
                        null; -- Bug #1752
                     else
                        raise Constraint_Error with "Unable to parse usage (QF => """
                        & Quantity & """): "
                                      & Exception_Message (E);
                     end if;
                  end;
            end;
         end if;
      end loop Over_Usage_Entries;
   end Parse_Usage;

   ----------------------------
   -- Parse_PET_Destinations --
   ----------------------------

   procedure Parse_PET_Destinations
     (J             : in out Job;
      PE_Task_Entry : Node)
   is
      JG_Nodes : Node_List;
      JG_Entry : Node;
      Element_Node : Node;
   begin
      Element_Node := Item (Child_Nodes (PE_Task_Entry), 1);
      if Name (Element_Node) /= "element" then
         raise Assumption_Error;
      end if;
      JG_Nodes := Child_Nodes (Element_Node);
      Over_JG_Nodes :
      for M in 1 .. Length (JG_Nodes) loop
         JG_Entry := Item (JG_Nodes, M - 1);
         if Name (JG_Entry) = "JG_qname" then
            J.Task_List.Include (To_Unbounded_String (Value (First_Child (JG_Entry))));
         end if;
      end loop Over_JG_Nodes;
   end Parse_PET_Destinations;

   -------------------------
   -- Parse_JAT_Task_List --
   -------------------------

   procedure Parse_JAT_Task_List
     (J             : in out Job;
      Task_List             : Node)
   is
      Task_List_Nodes : Node_List;
      PE_Task_Nodes   : Node_List;
      PE_Task_Entry   : Node;
   begin
      Task_List_Nodes := Child_Nodes (Task_List);
      Over_Task_List_Nodes :
      for K in 1 .. Length (Task_List_Nodes) loop
         if Name (Item (Task_List_Nodes, K - 1)) = "pe_tasks" or else
           Name (Item (Task_List_Nodes, K - 1)) = "element"
         then
            PE_Task_Nodes := Child_Nodes (Item (Task_List_Nodes, K - 1));
            Over_PE_Task_Nodes :
            for L in 1 .. Length (PE_Task_Nodes) loop
               PE_Task_Entry := Item (PE_Task_Nodes, L - 1);
               if Name (PE_Task_Entry) = "PET_granted_destin_identifier_list" then
                  Parse_PET_Destinations (J, PE_Task_Entry);
               elsif Name (PE_Task_Entry) = "PET_usage" then
                  Parse_Usage (Usage_Data => J.PET_Usage, Usage_Tree => PE_Task_Entry, Cumulative => True);
               end if;
            end loop Over_PE_Task_Nodes;
         end if;
      end loop Over_Task_List_Nodes;
   end Parse_JAT_Task_List;

   ----------------------------
   -- Parse_JAT_Message_List --
   ----------------------------

   procedure Parse_JAT_Message_List (Message_List : Node; J : in out Job) is
      Messages  : Node_List;
      Sublist, M : Node;
   begin
      Sublist := Item (Child_Nodes (Message_List), 1);
      if Name (Sublist) /= "ulong_sublist" then
         raise Assumption_Error;
      end if;
      Messages := Child_Nodes (Sublist);
      Over_Messages :
      for K in 1 .. Length (Messages) loop
         M := Item (Messages, K - 1);
         if Name (M) = "QIM_message" then
            J.Message_List.Append (To_Unbounded_String (Value (First_Child (M))));
         end if;
      end loop Over_Messages;
   end Parse_JAT_Message_List;

   -------------------
   -- Extract_Tasks --
   -------------------

   procedure Extract_Tasks (J : in out Job; Task_Nodes : Node_List) is
      Children      : Node_List;
      N             : Node;
      JA_Tasks      : Node;
   begin
      Over_Task_Nodes :
      for H in 1 .. Length (Task_Nodes) loop
         JA_Tasks := Item (Task_Nodes, H - 1);
         if Name (JA_Tasks) = "ja_tasks"
           or else Name (JA_Tasks) = "ulong_sublist"
         then
            Children := Child_Nodes (JA_Tasks);
            Task_Entries :
            for I in 1 .. Length (Children) loop
               N := Item (Children, I - 1);
               if Name (N) = "JAT_message_list" then
                  Parse_JAT_Message_List (Message_List => N, J => J);
               elsif Name (N) = "JAT_task_list" then
                  Parse_JAT_Task_List (J, N);
               elsif Name (N) = "JAT_scaled_usage_list" then
                  Parse_Usage (Usage_Data => J.JAT_Usage, Usage_Tree => N, Cumulative => False);
               end if;
            end loop Task_Entries;
         end if;
      end loop Over_Task_Nodes;
   end Extract_Tasks;

   -------------------
   -- Extract_Paths --
   -------------------

   procedure Extract_Paths (Path_List  : in out String_Lists.List;
                            List_Nodes  : Node_List) is
      List_Node, N : Node;
      Path_Nodes : Node_List;
   begin
      for I in 1 .. Length (List_Nodes) loop
         List_Node := Item (List_Nodes, I - 1);
         if Name (List_Node) = "path_list" then
            Path_Nodes := Child_Nodes (List_Node);
            for I in 1 .. Length (Path_Nodes) loop
               N := Item (Path_Nodes, I - 1);
               if Name (N) = "PN_path" then
                  Path_List.Append (To_Unbounded_String (Value (First_Child (N))));
               end if;
            end loop;
         end if;
      end loop;
   end Extract_Paths;


   procedure Extract_Context (Context       : in out SGE.Context.List;
                              Context_Nodes : Node_List) is
      N, Var_Node : Node;
      Context_Entries : Node_List;
      Variable_Name, Variable_Value : Unbounded_String;
   begin
      for I in 0 .. Length (Context_Nodes) - 1 loop
         N := Item (Context_Nodes, I);
         if Name (N) = "context_list" then
            Context_Entries := Child_Nodes (N);
            for J in 0 .. Length (Context_Entries) - 1 loop
               Var_Node := Item (Context_Entries, J);
               if Name (Var_Node) = "VA_variable" then
                  Variable_Name := To_Unbounded_String (Value (First_Child (Var_Node)));
               elsif Name (Var_Node) = "VA_value" then
                  Variable_Value := To_Unbounded_String (Value (First_Child (Var_Node)));
               elsif Name (Var_Node) /= "#text" then
                  raise Assumption_Error with
                    "Unexpected  """
                    & Name (Var_Node) & """ found inside <context_list>";
               end if;
            end loop;
            Context.Include (Key => To_String (Variable_Name),
                             New_Item => To_String (Variable_Value));
         end if;
      end loop;
   end Extract_Context;

   ----------------
   -- Prune_List --
   ----------------


   procedure Prune_By_Slots (Collection : in out List; Slots : String) is
      Temp : Job_Lists.List;
      Pos  : Job_Lists.Cursor := Collection.Container.First;
      J    : Job;

   begin
      loop
         exit when Pos = Job_Lists.No_Element;
         J := Job_Lists.Element (Pos);
         if Hash (J.Slot_List) = Slots then
            Temp.Append (J);
         end if;
         Next (Pos);
      end loop;
      Collection.Container := Temp;
   end Prune_By_Slots;

   ------------------
   -- Sort_By      --
   --  Purpose:  Sort the job list by any column/field
   --  Parameter Field: Title of the column to sort by
   ------------------

   procedure Sort_By (Collection : in out List;
                      Field      : String; Direction : String) is
   begin
      if Field = "Number" or else Field = "ID" then
         Sorting_By_Number.Sort (Collection.Container);
      elsif Field = "Name" then
         Sorting_By_Name.Sort (Collection.Container);
      elsif Field = "Owner" then
         Sorting_By_Owner.Sort (Collection.Container);
      elsif Field = "Priority" then
         Sorting_By_Priority.Sort (Collection.Container);
      elsif Field = "Submitted" then
         Sorting_By_Submission_Time.Sort (Collection.Container);
      elsif  Field = "Walltime" then
         Sorting_By_Walltime.Sort (Collection.Container);
      elsif Field = "Slots" then
         Sorting_By_Slots.Sort (Collection.Container);
      elsif Field = "State" then
         Sorting_By_State.Sort (Collection.Container);
      elsif Field = "CPU" then
         Sorting_By_CPU_Used.Sort (Collection.Container);
      elsif Field = "Memory" then
         Sorting_By_Memory_Used.Sort (Collection.Container);
      elsif Field = "IO" then
         Sorting_By_IO_Used.Sort (Collection.Container);
      elsif Field = "Priority" then
         Sorting_By_Priority.Sort (Collection.Container);
      elsif Field = "O" then
         Sorting_By_Override.Sort (Collection.Container);
      elsif Field = "S" then
         Sorting_By_Share.Sort (Collection.Container);
      elsif Field = "F" then
         Sorting_By_Functional.Sort (Collection.Container);
      elsif Field = "Urgency" then
         Sorting_By_Urgency.Sort (Collection.Container);
      elsif Field = "Resource" then
         Sorting_By_Resource_Contrib.Sort (Collection.Container);
      elsif Field = "Waiting" then
         Sorting_By_Waiting_Contrib.Sort (Collection.Container);
      elsif Field = "Custom" then
         Sorting_By_Posix_Priority.Sort (Collection.Container);
      elsif Field = "Ends In" or else
        Field = "Ends At"
      then
         Sorting_By_End.Sort (Collection.Container);
      else
         raise Constraint_Error with "Sorting by " & Field & " unimplemented";
      end if;
      if Direction = "dec" then
         Collection.Container.Reverse_Elements;
      end if;
   end Sort_By;

   ------------------------
   -- Precedes_By_Resources   --
   --  Purpose: Check whether one job should precede another when sorted by
   --           various resources
   --  Parameter Left: First Job
   --  Parameter Right: Second Job
   --  Returns: Whether Left precedes Right
   --  Description: This implements the "<" operator for package Generic_Sorting.
   --  This does a multi-column sort by slots, runtime, PE and so on.
   --  If neither a < b nor a > b, then a and b belong to the same Bunch.
   ------------------------

   function Precedes_By_Resources (Left, Right : Job) return Boolean is
   begin
      if Supports_Balancer (Left) and then not Supports_Balancer (Right) then
         return True;
      elsif not Supports_Balancer (Left) and then Supports_Balancer (Right) then
         return False;
      elsif Left.Queue < Right.Queue then
         return True;
      elsif Left.Queue > Right.Queue then
         return False;
      elsif Left.PE < Right.PE then
         return True;
      elsif Left.PE > Right.PE then
         return False;
      elsif Left.Slot_Number < Right.Slot_Number then
         return True;
      elsif Left.Slot_Number > Right.Slot_Number then
         return False;
      elsif Resources.Precedes (Left.Hard, Right.Hard) then
         return True;
      elsif Resources.Precedes (Right.Hard, Left.Hard) then
         return False;
      elsif Resources.Precedes (Left.Soft, Right.Soft) then
         return True;
      elsif Resources.Precedes (Right.Soft, Left.Soft) then
         return False;
      elsif Ranges.Precedes (Left.Slot_List, Right.Slot_List) then
         return True;
      elsif Ranges.Precedes (Left.Slot_List, Right.Slot_List) then
         return False;
      else
         return False;
      end if;
   end Precedes_By_Resources;

   ------------------------
   -- Precedes_By_Name   --
   --  Purpose: Check whether one job should precede another when sorted by name
   --  Parameter Left: First Job
   --  Parameter Right: Second Job
   --  Returns: Whether Left precedes Right
   --  Description: This implements the "<" operator for package Generic_Sorting
   ------------------------

   function Precedes_By_Name (Left, Right : Job) return Boolean is
   begin
      return Left.Full_Name < Right.Full_Name;
   end Precedes_By_Name;

   --------------------------
   -- Precedes_By_Number   --
   --  Purpose: Check whether one job should precede another when sorted by number
   --  Parameter Left: First Job
   --  Parameter Right: Second Job
   --  Returns: Whether Left precedes Right
   --  Description: This implements the "<" operator for package Generic_Sorting
   --------------------------

   function Precedes_By_Number (Left, Right : Job) return Boolean is
   begin
      if Left.Number = Right.Number then
         return  Min (Left.Task_IDs) <  Min (Right.Task_IDs);
      else
         return Left.Number < Right.Number;
      end if;
   end Precedes_By_Number;

   --------------------------
   -- Precedes_By_Owner   --
   --  Purpose: Check whether one job should precede another when sorted by owner
   --  Parameter Left: First Job
   --  Parameter Right: Second Job
   --  Returns: Whether Left precedes Right
   --  Description: This implements the "<" operator for package Generic_Sorting
   --------------------------

   function Precedes_By_Owner (Left, Right : Job) return Boolean is
   begin
      return Left.Owner < Right.Owner;
   end Precedes_By_Owner;

   --------------------------
   -- Precedes_By_Priority   --
   --  Purpose: Check whether one job should precede another when sorted by priority
   --  Parameter Left: First Job
   --  Parameter Right: Second Job
   --  Returns: Whether Left precedes Right
   --  Description: This implements the "<" operator for package Generic_Sorting
   --------------------------

   function Precedes_By_Priority (Left, Right : Job) return Boolean is
   begin
      return Left.Priority < Right.Priority;
   end Precedes_By_Priority;

   --------------------------
   -- Precedes_By_Submission_time   --
   --  Purpose: Check whether one job should precede another when sorted by submission time
   --  Parameter Left: First Job
   --  Parameter Right: Second Job
   --  Returns: Whether Left precedes Right
   --  Description: This implements the "<" operator for package Generic_Sorting
   --------------------------

   function Precedes_By_Submission_Time (Left, Right : Job) return Boolean is
   begin
      return Left.Submission_Time < Right.Submission_Time;
   end Precedes_By_Submission_Time;

   function Precedes_By_Walltime (Left, Right : Job) return Boolean is
   begin
      return Left.Walltime < Right.Walltime;
   end Precedes_By_Walltime;

   --------------------------
   -- Precedes_By_Slots   --
   --  Purpose: Check whether one job should precede another when sorted by
   --           number of slots
   --  Parameter Left: First Job
   --  Parameter Right: Second Job
   --  Returns: Whether Left precedes Right
   --  Description: This implements the "<" operator for package Generic_Sorting
   --------------------------

   function Precedes_By_Slots (Left, Right : Job) return Boolean is
   begin
      return Integer'Value (To_String (Left.Slot_Number)) < Integer'Value (To_String (Right.Slot_Number));
   end Precedes_By_Slots;

   --------------------------
   -- Precedes_By_State   --
   --  Purpose: Check whether one job should precede another when sorted by state
   --  Parameter Left: First Job
   --  Parameter Right: Second Job
   --  Returns: Whether Left precedes Right
   --  Description: This implements the "<" operator for package Generic_Sorting
   --------------------------

   function Precedes_By_State (Left, Right : Job) return Boolean is
   begin
      return Left.State_Array < Right.State_Array;
   end Precedes_By_State;

   --------------------------
   -- Precedes_By_CPU_Used   --
   --  Purpose: Check whether one job should precede another when sorted by CPU time
   --  Parameter Left: First Job
   --  Parameter Right: Second Job
   --  Returns: Whether Left precedes Right
   --  Description: This implements the "<" operator for package Generic_Sorting
   --------------------------

   function Precedes_By_CPU_Used (Left, Right : Job) return Boolean is
   begin
      return Left.CPU < Right.CPU;
   end Precedes_By_CPU_Used;

   --------------------------
   -- Precedes_By_Memory_Used   --
   --  Purpose: Check whether one job should precede another when sorted by memory usage
   --  Parameter Left: First Job
   --  Parameter Right: Second Job
   --  Returns: Whether Left precedes Right
   --  Description: This implements the "<" operator for package Generic_Sorting
   --------------------------

   function Precedes_By_Memory_Used (Left, Right : Job) return Boolean is
   begin
      return Left.Mem < Right.Mem;
   end Precedes_By_Memory_Used;

   --------------------------
   -- Precedes_By_IO_Used   --
   --  Purpose: Check whether one job should precede another when sorted by IO usage
   --  Parameter Left: First Job
   --  Parameter Right: Second Job
   --  Returns: Whether Left precedes Right
   --  Description: This implements the "<" operator for package Generic_Sorting
   --------------------------

   function Precedes_By_IO_Used (Left, Right : Job) return Boolean is
   begin
      return Left.IO < Right.IO;
   end Precedes_By_IO_Used;

   --------------------------
   -- Precedes_By_Override   --
   --  Purpose: Check whether one job should precede another when sorted by override tickets
   --  Parameter Left: First Job
   --  Parameter Right: Second Job
   --  Returns: Whether Left precedes Right
   --  Description: This implements the "<" operator for package Generic_Sorting
   --------------------------

   function Precedes_By_Override (Left, Right : Job) return Boolean is
   begin
      return Left.Override_Tickets < Right.Override_Tickets;
   end Precedes_By_Override;

   --------------------------
   -- Precedes_By_Share   --
   --  Purpose: Check whether one job should precede another when sorted by share tickets
   --  Parameter Left: First Job
   --  Parameter Right: Second Job
   --  Returns: Whether Left precedes Right
   --  Description: This implements the "<" operator for package Generic_Sorting
   --------------------------

   function Precedes_By_Share (Left, Right : Job) return Boolean is
   begin
      return Left.Share_Tickets < Right.Share_Tickets;
   end Precedes_By_Share;

   --------------------------
   -- Precedes_By_Functional   --
   --  Purpose: Check whether one job should precede another when sorted by functional tickets
   --  Parameter Left: First Job
   --  Parameter Right: Second Job
   --  Returns: Whether Left precedes Right
   --  Description: This implements the "<" operator for package Generic_Sorting
   --------------------------

   function Precedes_By_Functional (Left, Right : Job) return Boolean is
   begin
      return Left.Functional_Tickets < Right.Functional_Tickets;
   end Precedes_By_Functional;

   --------------------------
   -- Precedes_By_Urgency   --
   --  Purpose: Check whether one job should precede another when sorted by urgency tickets
   --  Parameter Left: First Job
   --  Parameter Right: Second Job
   --  Returns: Whether Left precedes Right
   --  Description: This implements the "<" operator for package Generic_Sorting
   --------------------------

   function Precedes_By_Urgency (Left, Right : Job) return Boolean is
   begin
      return Left.Urgency < Right.Urgency;
   end Precedes_By_Urgency;

   --------------------------
   -- Precedes_By_Waiting_Contrib   --
   --  Purpose: Check whether one job should precede another when sorted by waiting time contribution to priority
   --  Parameter Left: First Job
   --  Parameter Right: Second Job
   --  Returns: Whether Left precedes Right
   --  Description: This implements the "<" operator for package Generic_Sorting
   --------------------------

   function Precedes_By_Waiting_Contrib (Left, Right : Job) return Boolean is
   begin
      return Left.Waiting_Contrib < Right.Waiting_Contrib;
   end Precedes_By_Waiting_Contrib;

   --------------------------
   -- Precedes_By_Resource_Contrib   --
   --  Purpose: Check whether one job should precede another when sorted by resource contribution to priority
   --  Parameter Left: First Job
   --  Parameter Right: Second Job
   --  Returns: Whether Left precedes Right
   --  Description: This implements the "<" operator for package Generic_Sorting
   --------------------------

   function Precedes_By_Resource_Contrib (Left, Right : Job) return Boolean is
   begin
      return Left.Resource_Contrib < Right.Resource_Contrib;
   end Precedes_By_Resource_Contrib;

   --------------------------
   -- Precedes_By_Ppsix_Priority   --
   --  Purpose: Check whether one job should precede another when sorted by POSIX priority
   --  Parameter Left: First Job
   --  Parameter Right: Second Job
   --  Returns: Whether Left precedes Right
   --  Description: This implements the "<" operator for package Generic_Sorting
   --------------------------

   function Precedes_By_Posix_Priority (Left, Right : Job) return Boolean is
   begin
      return Left.Posix_Priority < Right.Posix_Priority;
   end Precedes_By_Posix_Priority;

   ---------------------
   -- Precedes_By_End --
   ---------------------

   function Precedes_By_End (Left, Right : Job) return Boolean is
   begin
      return End_Time (Left) < End_Time (Right);
   exception
      when Resource_Error =>
         return True;
   end Precedes_By_End;


   ------------------------
   -- Same               --
   --  Purpose: Check whether two jobs are identical
   --  Parameter Left: First Job
   --  Parameter Right: Second Job
   --  Returns: Whether Left and Right are identical
   --  Description: This implements the "=" operator for package Doubly_Linked_Lists;
   --    we compare job numbers (since they are unique)
   ------------------------


   function Same (Left, Right : Job) return Boolean is
   begin
      if Left.Number = 0 then
         return False;
      elsif Left.Number = Right.Number then
         return True;
      else
         return False;
      end if;
   end Same;



   --------------
   -- Overlays --
   --------------

   procedure Create_Overlay (Nodes : Node_List) is
      N : Node;
      J : Job;
   begin
      for Index in 1 .. Length (Nodes) loop
         N := Item (Nodes, Index - 1);
         if Name (N) /= "#text" then
            J := New_Job (Child_Nodes (N), True);
            Overlay.Insert (New_Item => J,
                            Key      => J.Number);
         end if;
      end loop;
   end Create_Overlay;

   procedure Update_Job_From_Overlay (J : in out Job) is
      Update : Job := Overlay.Element (J.Number);
   begin
      J.Reserve := Update.Reserve;
      J.Slot_List := Update.Slot_List;
      J.Context := Update.Context;
      J.PE := Update.PE;
      Determine_Balancer_Support (J);
   end Update_Job_From_Overlay;

   procedure Apply_Overlay (Collection : in out List) is

      procedure Apply_Overlay_Entry (Position : Job_Lists.Cursor) is
      begin
         Collection.Container.Update_Element (Position => Position,
                                              Process  => Update_Job_From_Overlay'Access);
      end Apply_Overlay_Entry;

   begin
      Collection.Container.Iterate (Apply_Overlay_Entry'Access);
   end Apply_Overlay;

   procedure Update_Quota (Collection : in out List) is
      procedure Update_Quota_For_Job (J : in out Job) is
      begin
         J.RQS_Reached := SGE.Quota.Get_Headroom (User => J.Owner,
                                                  Resource => "slots",
                                                  PEs  => J.PE /= Null_Unbounded_String) < Get_Maximum_Slots (J);
      end Update_Quota_For_Job;

      procedure Quota_For_Job (Position : Job_Lists.Cursor) is
      begin
         Collection.Container.Update_Element (Position => Position,
                                              Process  => Update_Quota_For_Job'Access);
      end Quota_For_Job;

   begin
      Collection.Container.Iterate (Quota_For_Job'Access);
   end Update_Quota;

   procedure Iterate (Collection : List;
                      Process    : not null access procedure (J : Job)) is
      procedure Wrapper (Position : Job_Lists.Cursor) is
      begin
         Process (Element (Position));
      end Wrapper;

   begin
      Collection.Container.Iterate (Wrapper'Access);
   end Iterate;

   procedure Iterate_Predecessors (J       : Job;
                                   Process : not null access procedure (ID : Natural)) is
      procedure Wrapper (Position : Utils.ID_Lists.Cursor) is
      begin
         Process (Utils.ID_Lists.Element (Position));
      end Wrapper;

   begin
      J.Predecessors.Iterate (Wrapper'Access);
   end Iterate_Predecessors;

   procedure Iterate_Predecessor_Requests (J       : Job;
                                           Process : not null access procedure (S : String)) is
      procedure Wrapper (Position : Utils.String_Lists.Cursor) is
      begin
         Process (To_String (Element (Position)));
      end Wrapper;

   begin
      J.Predecessor_Request.Iterate (Wrapper'Access);
   end Iterate_Predecessor_Requests;

   procedure Iterate_Successors (J       : Job;
                                 Process : not null access procedure (ID : Natural)) is
      procedure Wrapper (Position : Utils.ID_Lists.Cursor) is
      begin
         Process (Utils.ID_Lists.Element (Position));
      end Wrapper;

   begin
      J.Successors.Iterate (Wrapper'Access);
   end Iterate_Successors;

   procedure Iterate_Messages (J : Job;
                               Process : not null access procedure (Message : String))
   is
      procedure Wrapper (Position : Utils.String_Lists.Cursor) is
      begin
         Process (To_String (Element (Position)));
      end Wrapper;

   begin
      J.Message_List.Iterate (Wrapper'Access);
   end Iterate_Messages;

   procedure Iterate_Queues (J : Job;
                             Process : not null access procedure (Queue : String)) is
      procedure Wrapper (Position : Utils.String_Lists.Cursor) is
      begin
         Process (To_String (Element (Position)));
      end Wrapper;

   begin
      J.Queue_List.Iterate (Wrapper'Access);
   end Iterate_Queues;

   procedure Iterate_Slots (J : Job;
                            Process : not null access procedure (R : Step_Range)) is
      procedure Wrapper (Position : Range_Lists.Cursor) is
      begin
         Process (Element (Position));
      end Wrapper;

   begin
      J.Slot_List.Iterate (Wrapper'Access);
   end Iterate_Slots;

   procedure Iterate_Tasks (J : Job;
                            Process : not null access procedure (R : Step_Range)) is
      procedure Wrapper (Position : Range_Lists.Cursor) is
      begin
         Process (Element (Position));
      end Wrapper;

   begin
      J.Array_Tasks.Iterate (Wrapper'Access);
   end Iterate_Tasks;

   procedure Iterate_Context (J : Job;
                              Process : not null access procedure (Key, Element : String))
   is
   begin
      J.Context.Iterate (Process);
   end Iterate_Context;

   function To_Abbrev (Flag : State_Flag) return String is
   begin
      case Flag is
         when deletion =>
            return "d";
         when Error =>
            return "E";
         when waiting =>
            return "qw";
         when hold =>
            return "h";
         when Threshold =>
            return "T";
         when running =>
            return "r";
         when transfering =>
            return "t";
         when Restarted =>
            return "R";
         when suspended =>
            return "s";
         when Q_Suspended =>
            return "S";
      end case;
   end To_Abbrev;

   function To_String (Flag : State_Flag) return String is
   begin
      return Ada.Characters.Handling.To_Lower (Flag'Img);
   end To_String;

   procedure Add_Message (J : in out Job; Number : Natural; Message : Unbounded_String) is
   begin
      J.Message_List.Append (Number'Img & ": " & Message);
      --  FIXME: can we use and store the message number seperately?
   end Add_Message;

   function Is_Sorted (Collection : List) return Boolean is
   begin
      return Sorting_By_Resources.Is_Sorted (Collection.Container);
   end Is_Sorted;

   function First (Collection : List) return Cursor is
   begin
      return Jobs.Cursor (Collection.Container.First);
   end First;

   overriding procedure Next (Position : in out Cursor) is
   begin
      Next (Job_Lists.Cursor (Position));
   end Next;

   overriding function Has_Element (Position : Cursor) return Boolean is
   begin
      return Has_Element (Job_Lists.Cursor (Position));
   end Has_Element;

   overriding function Element (Position : Cursor) return Job is
   begin
      return Job_Lists.Element (Job_Lists.Cursor (Position));
   end Element;

end SGE.Jobs;
