with Ada.Text_IO;
with Ada.Calendar;   use Ada.Calendar;
with GNAT.Calendar.Time_IO;
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

   -------------
   --  accessors
   -------------
   function Count return Natural is
   begin
      return Natural (List.Length);
   end Count;

   function Count (Predicate : not null access function (J : Job) return Boolean)
                   return Natural is

      Counter : Natural := 0;
      procedure Wrapper (Position : Job_Lists.Cursor) is
      begin
         if Predicate (Element (Position)) then
            Counter := Counter + 1;
         end if;
      end Wrapper;

   begin
      List.Iterate (Wrapper'Access);
      return Counter;
   end Count;

   function Get_Task_Count (J : Job) return Natural is
   begin
      return Count (J.Task_IDs);
   end Get_Task_Count;

   function Get_ID (J : Job) return String is
   begin
      return Ada.Strings.Fixed.Trim (Source => J.Number'Img,
                                     Side => Ada.Strings.Left);
   end Get_ID;

   function Get_PE (J : Job) return Unbounded_String is
   begin
      return J.PE;
   end Get_PE;

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
            return Natural'Value (Raw_Range (Raw_Range'First .. Separator - 1));
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

      if J.Context.Contains (To_Unbounded_String ("SLOTSCPU"))
        and then J.Context.Contains (To_Unbounded_String ("SLOTSGPU")) then
         J.Balancer (CPU_GPU) := True;
         J.Balancer (Any) := True;
      end if;
      if J.Context.Contains (To_Unbounded_String ("WAITREDUCE"))
        and then J.Context.Contains (To_Unbounded_String ("SLOTSREDUCE")) then
         J.Balancer (Low_Cores) := True;
         J.Balancer (Any) := True;
      end if;
      if J.Context.Contains (To_Unbounded_String ("SLOTSEXTEND")) then
         J.Balancer (High_Cores) := True;
         J.Balancer (Any) := True;
      end if;
   end Determine_Balancer_Support;

   procedure Update_State_Array (J : in out Job) is
      Flag : State_Flag;
   begin
      for Flag in J.State_Array'Range loop
         J.State_Array (Flag) := False;
      end loop;

      for Position in J.State_String'Range loop
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
            when 'T' => Flag := Threshold;
            when 'q' =>
               null;
            when 'w' =>
               Flag := waiting;
            when ' ' =>
               null;
            when others =>
               raise Jobs.Other_Error with "Unknown state character " &
               J.State_String (Position) & " found";
         end case;
         J.State_Array (Flag) := True;
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

   function Get_Context (J : Job) return Utils.String_Pairs.Map is
   begin
      return J.Context;
   end Get_Context;

   function Get_Priority (J : Job) return Utils.Fixed is
   begin
      return J.Priority;
   end Get_Priority;

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

   function Get_Context (J : Job; Key : String) return String is
   begin
      if J.Context.Contains (To_Unbounded_String (Key)) then
         return To_String (J.Context.Element (To_Unbounded_String (Key)));
      else
         return "";
      end if;
   end Get_Context;

   function Has_Context (J : Job; Key : String) return Boolean is
   begin
      return J.Context.Contains (To_Unbounded_String (Key));
   end Has_Context;

   function Get_Task_IDs (J : Job) return Ranges.Step_Range_List is
   begin
      return J.Task_IDs;
   end Get_Task_IDs;

   ----------------
   --  list-related
   ----------------

   function Find_Job (ID : Natural) return Job_Lists.Cursor is
      Pos : Job_Lists.Cursor := List.First;
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

   function Find_Job (ID : Natural) return Job is
   begin
      return Element (Find_Job (ID));
   end Find_Job;

   procedure Sort is
   begin
      Sorting_By_Resources.Sort (List);
   end Sort;

   procedure Rewind is
   begin
      List_Cursor := List.First;
   end Rewind;

   function Empty return Boolean is
   begin
      return List.Is_Empty;
   end Empty;

   function Next return Job is
   begin
      Next (List_Cursor);
      return Job_Lists.Element (List_Cursor);
   end Next;

   function At_End return Boolean is
   begin
      if List_Cursor = Job_Lists.No_Element or else
        List_Cursor = List.Last then
         return True;
      end if;
      return False;
   end At_End;

   function Current return Job is
   begin
      return Job_Lists.Element (List_Cursor);
   end Current;


   -----------------
   -- Get_Summary --
   -----------------

   procedure Get_Summary (Tasks, Slots : out State_Count) is

      procedure Count (Position : Job_Lists.Cursor) is
         State_Array : State := Job_Lists.Element (Position).State_Array;
         Slot_Number : Natural := Integer'Value (To_String (Job_Lists.Element (Position).Slot_Number));
      begin
         for Flag in State_Array'Range loop
            if State_Array (Flag) then
               Tasks (Flag) := Tasks (Flag) + 1;
               Slots (Flag) := Slots (Flag) + Slot_Number;
            end if;
         end loop;
      end Count;

   begin
      for S in Tasks'Range loop
         Tasks (S) := 0;
         Slots (S) := 0;
      end loop;
      List.Iterate (Process => Count'Access);
   end Get_Summary;

   function Get_Last_Migration (J : Job) return Time is
      Last_Mig : constant Unbounded_String := To_Unbounded_String ("LASTMIG");
   begin
      if J.Context.Contains (Last_Mig) then
         return Ada.Calendar.Conversions.To_Ada_Time
           (Interfaces.C.long'Value (To_String (J.Context.Element (Last_Mig))));
      else
         raise Constraint_Error;
      end if;
   end Get_Last_Migration;

   function Get_Last_Reduction (J : Job) return Time is
      Last_Red : constant Unbounded_String := To_Unbounded_String ("LASTRED");
   begin
      if J.Context.Contains (Last_Red) then
         return Ada.Calendar.Conversions.To_Ada_Time
           (Interfaces.C.long'Value (To_String (J.Context.Element (Last_Red))));
      else
         raise Constraint_Error;
      end if;
   end Get_Last_Reduction;

   function Get_Last_Extension (J : Job) return Time is
      Last_Ext : constant Unbounded_String := To_Unbounded_String ("LASTEXT");
   begin
      if J.Context.Contains (Last_Ext) then
         return Ada.Calendar.Conversions.To_Ada_Time
           (Interfaces.C.long'Value (To_String (J.Context.Element (Last_Ext))));
      else
         raise Constraint_Error;
      end if;
   end Get_Last_Extension;

   function Get_Reduce_Wait (J : Job) return Natural is
      Key : constant Unbounded_String := To_Unbounded_String ("WAITREDUCE");
   begin
      if J.Context.Contains (Key) then
         return Integer'Value (To_String (J.Context.Element (Key)));
      else
         raise Constraint_Error;
      end if;
   end Get_Reduce_Wait;

   function Get_Reduced_Runtime (J : Job) return String is
      Key : constant Unbounded_String := To_Unbounded_String ("RTREDUCE");
   begin
      if J.Context.Contains (Key) then
         return To_String (J.Context.Element (Key));
      else
         return "";
      end if;
   end Get_Reduced_Runtime;

   function Get_Reduced_Slots (J : Job) return String is
      Key : constant Unbounded_String := To_Unbounded_String ("SLOTSREDUCE");
   begin
      if J.Context.Contains (Key) then
         return To_String (J.Context.Element (Key));
      else
         raise Constraint_Error;
      end if;
   end Get_Reduced_Slots;

   function Get_Extended_Slots (J : Job) return String is
      Key : constant Unbounded_String := To_Unbounded_String ("SLOTSEXTEND");
   begin
      if J.Context.Contains (Key) then
         return To_String (J.Context.Element (Key));
      else
         raise Constraint_Error;
      end if;
   end Get_Extended_Slots;

   function Get_CPU_Range (J : Job) return String is
      CPU_Range : constant Unbounded_String := To_Unbounded_String ("SLOTSCPU");
   begin
      if J.Context.Contains (CPU_Range) then
         return Ranges.To_String (Ranges.To_Step_Range_List
                                  (To_String (J.Context.Element (CPU_Range))),
                                 Short => True);
      else
         raise Constraint_Error;
      end if;
   end Get_CPU_Range;

   function Get_GPU_Range (J : Job) return String is
      GPU_Range : constant Unbounded_String := To_Unbounded_String ("SLOTSGPU");
   begin
      if J.Context.Contains (GPU_Range) then
         return Ranges.To_String (Ranges.To_Step_Range_List (To_String
                                  (J.Context.Element (GPU_Range))),
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
      return not J.Error_Log.Is_Empty;
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


   -----------------
   -- Append_List --
   -----------------

   procedure Append_List (Nodes : Node_List) is
      N : Node;
   begin
      for Index in 1 .. Length (Nodes) loop
         N := Item (Nodes, Index - 1);
         if Name (N) /= "#text" then
            List.Append (New_Job (Child_Nodes (N)));
         end if;
      end loop;
   end Append_List;


   procedure Prune_List (PE, Queue, Hard_Requests,
                         Soft_Requests,
                         Slot_Number, Slot_Ranges : Unbounded_String) is
      --  FIXME: implement in terms of the new kernel-based Prune_List below
      J : Job;
      Pruned_List : Job_Lists.List;
   begin
      Rewind;
      J := Current;
      loop
         if J.PE = PE and then
 J.Queue = Queue and then
         J.Hard.Hash = Hard_Requests and then
         J.Soft.Hash = Soft_Requests then
            Update_Job_From_Overlay (J);
            if Slot_Ranges = Null_Unbounded_String or else -- Bug #1610
              Hash_Type'Value (To_String (Slot_Ranges)) = 0 then
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
         exit when At_End;
         J := Next;
      end loop;
      List.Clear;
      List.Splice (Source => Pruned_List, Before => List.First);
   end Prune_List;

   procedure Prune_List (Keep : not null access function (J : Job) return Boolean) is
      J : Job;
      Pruned_List : Job_Lists.List;
   begin
      Rewind;
      J := Current;
      loop
         if Keep (J) then
            Pruned_List.Append (J);
         end if;
         exit when At_End;
         J := Next;
      end loop;
      List.Clear;
      List.Splice (Source => Pruned_List, Before => List.First);
   end Prune_List;

   -------------------
   -- Update_Status --
   -------------------

   procedure Update_Status is
      Pos : Job_Lists.Cursor;
   begin
      Pos := List.First;
      while Pos /= Job_Lists.No_Element loop
         List.Update_Element (Position => Pos,
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

      use Ada.Strings.Fixed;
   begin
      SGE_Out := Parser.Setup (Selector => "-u " & To_String (J.Owner));

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
            end if;
         end loop Fields;
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

   procedure Search_Queues is
      SGE_Out                : Tree;
      Job_Nodes, Value_Nodes : Node_List;
      Job_Node, Value_Node   : Node;
      A                      : Attr;
      Pos                    : Job_Lists.Cursor;
      Number_Found           : Natural;
      The_Queue              : Unbounded_String;

      procedure Record_Queue (Element : in out Job) is
         Success : Boolean;
         Where : String_Sets.Cursor;
      begin
         Element.Detected_Queues.Insert (New_Item => The_Queue,
                                         Position => Where,
                                         Inserted => Success);
      end Record_Queue;

   begin
      SGE_Out := Parser.Setup (Command  => "qhost",
                               Selector => "-j");

      --  Fetch Jobs
      Job_Nodes := Parser.Get_Elements_By_Tag_Name (SGE_Out, "job");
      for I in 0 .. Length (Job_Nodes) - 1 loop
         Job_Node := Item (List  => Job_Nodes,
                           Index => I);
         A := Get_Attr (Job_Node, "name");
         Number_Found := Integer'Value (Value (A));
         Pos := Find_Job (ID => Number_Found);
         --  in theory, this scaling is less than ideal:
         --  O(number of jobs in Job_List times number of jobs returned by qhost)
         --  however, in the present implementation, there is only one job in the list,
         --  so this should not be a problem
         --  if we ever change the code to produce more than one or two jobs at this
         --  point, we should change the job list to a map with the ID as a key
         if Pos /= Job_Lists.No_Element then
            Value_Nodes := Child_Nodes (Job_Node);
            for J in 0 .. Length (Value_Nodes) - 1 loop
               Value_Node := Item (List  => Value_Nodes,
                                   Index => J);
               if Name (Value_Node) = "jobvalue" then
                  A := Get_Attr (Value_Node, "name");
                  if Value (A) = "qinstance_name" then
                     The_Queue := To_Unbounded_String (Value (First_Child (Value_Node)));
                  end if;
               end if;
            end loop;
            List.Update_Element (Position => Pos,
                                 Process  => Record_Queue'Access);
         end if;
      end loop;
      Parser.Free;
   end Search_Queues;

   -------------
   -- New_Job --
   -------------

   function New_Job (List : Node_List) return Job is
      J           : Job;
   begin
      J.Merge_Std_Err := Undecided;
      J.Reserve := Undecided;
      J.Mem := 0.0;
      J.IO := 0.0;
      J.CPU := 0.0;
      Update_Job (J => J, List => List);
      Determine_Balancer_Support (J);
      return J;
   end New_Job;

   ----------------
   -- Update_Job --
   ----------------

   procedure Update_Job (J : in out Job; List : Node_List) is
      C           : Node;
      A           : Attr;
      Time_Buffer : String (1 .. 19);
      Inserted    : Boolean;
      Inserted_At : Resource_Lists.Cursor;

      use Ada.Strings.Fixed;
   begin
      for Index in 0 .. Length (List) - 1 loop
         begin
            C := Item (List, Index);
            if Name (C) = "JB_job_number" then
               J.Number := Integer'Value (Value (First_Child (C)));
            elsif Name (C) = "JAT_prio" then
               J.Priority := Fixed'Value (Value (First_Child (C)));
            elsif Name (C) = "JB_name" or else
            Name (C) = "JB_job_name" then
               J.Full_Name := To_Unbounded_String (Value (First_Child (C)));
               J.Name := Job_Names.To_Bounded_String (Source => Value (First_Child (C)),
                                                      Drop   => Ada.Strings.Right);
               if Length (J.Full_Name) > Max_Name_Length then
                  J.Name_Truncated := True;
               else
                  J.Name_Truncated := False;
               end if;
            elsif Name (C) = "JB_owner" then
               J.Owner := To_User_Name (Value (First_Child (C)));
            elsif Name (C) = "state" then
               J.State_String := Head (Value (First_Child (C)), J.State_String'Length);
               Update_State_Array (J);
            elsif Name (C) = "JB_submission_time" then
               if Value (First_Child (C))'Length > 11 and then
                 Value (First_Child (C)) (11) = 'T' then
                  Time_Buffer := Value (First_Child (C));
                  Time_Buffer (11) := ' ';
                  J.Submission_Time := GNAT.Calendar.Time_IO.Value (Time_Buffer);
               else
                  J.Submission_Time := Ada.Calendar.Conversions.To_Ada_Time
                    (Interfaces.C.long'Value (Value (First_Child (C))));
               end if;

            elsif Name (C) = "JAT_start_time" then
               Time_Buffer := Value (First_Child (C));
               if Time_Buffer (11) /= 'T' then
                  raise Time_Error;
               end if;
               Time_Buffer (11) := ' ';
               J.Submission_Time := GNAT.Calendar.Time_IO.Value (Time_Buffer);
            elsif Name (C) = "queue_name" then
               null; -- ignore
            elsif Name (C) = "slots" then
               J.Slot_Number := To_Unbounded_String (Value (First_Child (C)));
            elsif Name (C) = "ftickets" then
               J.Functional_Tickets := Integer'Value (Value (First_Child (C)));
            elsif Name (C) = "stickets" then
               J.Share_Tickets := Integer'Value (Value (First_Child (C)));
            elsif Name (C) = "otickets" then
               J.Override_Tickets := Integer'Value (Value (First_Child (C)));
            elsif Name (C) = "cpu_usage" then
               J.CPU := Float'Value (Value (First_Child (C)));
            elsif Name (C) = "mem_usage" then
               J.Mem := Float'Value (Value (First_Child (C)));
            elsif Name (C) = "io_usage" then
               J.IO := Float'Value (Value (First_Child (C)));
            elsif Name (C) = "JB_wtcontr" then
               J.Waiting_Contrib := Integer'Value (Value (First_Child (C)));
            elsif Name (C) = "JB_rrcontr" then
               J.Resource_Contrib := Integer'Value (Value (First_Child (C)));
            elsif Name (C) = "JB_nurg" then
               J.Urgency := Fixed'Value (Value (First_Child (C)));
            elsif Name (C) = "JB_priority" then
               J.Posix_Priority := Posix_Priority_Type'Value (Value (First_Child (C)));
            elsif Name (C) = "hard_req_queue" then
               J.Queue := To_Unbounded_String (Value (First_Child (C)));
            elsif Name (C) = "full_job_name" then
               null; -- ignore
            elsif Name (C) = "requested_pe" then
               A := Get_Attr (C, "name");
               J.PE := To_Unbounded_String (Value (A));
            elsif Name (C) = "hard_request" then
               A := Get_Attr (C, "name");
               J.Hard.Insert (Key      => To_Unbounded_String (Value (A)),
                              New_Item => New_Resource (Name  => Value (A),
                                                        Value => Value (First_Child (C))),
                              Position => Inserted_At,
                              Inserted => Inserted);
            elsif Name (C) = "soft_request" then
               A := Get_Attr (C, "name");
               J.Soft.Insert (Key      => To_Unbounded_String (Value (A)),
                              New_Item => New_Resource (Name  => Value (A),
                                                        Value => Value (First_Child (C))),
                              Position => Inserted_At,
                              Inserted => Inserted);
            elsif Name (C) = "predecessor_jobs" or else
               Name (C) = "ad_predecessor_jobs" then
               J.Predecessors.Include (New_Item => Natural'Value (Value (First_Child (C))));
            elsif Name (C) = "predecessor_jobs_req" or else
              Name (C) = "ad_predecessor_jobs_req" then
               J.Predecessor_Request.Append (To_Unbounded_String (Value (First_Child (C))));
            elsif Name (C) = "JB_hard_resource_list" then
               Extract_Resource_List (J, Child_Nodes (C));
            elsif Name (C) = "JB_soft_resource_list" then
               Extract_Resource_List (J, Child_Nodes (C), Soft => True);
            elsif Name (C) = "JB_hard_queue_list" then
               Extract_Queue_List (J, Child_Nodes (C));
            elsif Name (C) = "JB_ja_tasks" then
               Extract_Tasks (J, Child_Nodes (C));
            elsif Name (C) = "JB_pe_range" then
               Extract_PE_Range (J, Child_Nodes (C));

            elsif Name (C) = "JB_department" then
               J.Department := To_Unbounded_String (Value (First_Child (C)));
            elsif Name (C) = "JB_project" then
               if Length (Child_Nodes (C)) > 0 then
                  J.Project := To_Unbounded_String (Value (First_Child (C)));
               else
                  J.Project := To_Unbounded_String ("none");
               end if;
            elsif Name (C) = "JB_ar" then
               J.Job_Advance_Reservation := To_Unbounded_String (Value (First_Child (C)));
            elsif            Name (C) = "JB_ja_structure" then
               null;  -- to array
            elsif Name (C) = "JB_exec_file" then
               J.Exec_File := To_Unbounded_String (Value (First_Child (C)));
            elsif Name (C) = "JB_group" then
               J.Group := To_Unbounded_String (Value (First_Child (C)));
            elsif Name (C) = "JB_merge_stderr" then
               J.Merge_Std_Err := To_Tri_State (Value (First_Child (C)));
            elsif Name (C) = "JB_stdout_path_list" then
               Extract_Paths (J.Std_Out_Paths, Child_Nodes (C));
            elsif Name (C) = "JB_stderr_path_list" then
               Extract_Paths (J.Std_Err_Paths, Child_Nodes (C));
            elsif Name (C) = "JB_script_file" then
               J.Script_File := To_Unbounded_String (Value (First_Child (C)));
            elsif Name (C) = "JB_cwd" then
               J.Directory := To_Unbounded_String (Value (First_Child (C)));
            elsif Name (C) = "JB_reserve" then
               J.Reserve := To_Tri_State (Value (First_Child (C)));
            elsif Name (C) = "JB_pe" then
               J.PE := To_Unbounded_String (Value (First_Child (C)));
            elsif Name (C) = "JB_notify" then
               J.Notify := To_Tri_State (Value (First_Child (C)));
            elsif Name (C) = "JB_account" then
               J.Account := To_Unbounded_String (Value (First_Child (C)));
            elsif Name (C) = "JB_job_args" then
               Extract_Args (J, Child_Nodes (C));
            elsif Name (C) = "tasks" then
               J.Task_IDs := To_Step_Range_List (Value (First_Child (C)));
            elsif Name (C) = "granted_pe" then
               null;
            elsif Name (C) = "JB_jid_predecessor_list" then
               Extract_Hold_ID_List (J.Predecessors, Child_Nodes (C));
            elsif Name (C) = "JB_jid_successor_list" then
               Extract_Hold_ID_List (J.Successors, Child_Nodes (C));
            elsif Name (C) = "JB_context" then
               Extract_Context (J.Context, Child_Nodes (C));
            elsif Name (C) = "JB_urg" or else
              Name (C) = "JB_dlcontr" or else
              Name (C) = "JAT_ntix" or else
              Name (C) = "JAT_share" or else
              Name (C) = "JB_jobshare" or else
              Name (C) = "JB_jid_request_list" or else
              Name (C) = "tickets" or else
              Name (C) = "JB_nppri" or else
              Name (C) = "JB_uid" or else
              Name (C) = "JB_gid" or else
              Name (C) = "JB_mail_list" or else
              Name (C) = "JB_mail_options" or else
              Name (C) = "JB_deadline" or else
              Name (C) = "JB_shell_list" or else
              Name (C) = "JB_env_list" or else
              Name (C) = "JB_checkpoint_attr" or else
              Name (C) = "JB_checkpoint_interval" or else
              Name (C) = "JB_verify" or else
              Name (C) = "JB_restart" or else
              Name (C) = "JB_soft_wallclock_gmt" or else
              Name (C) = "JB_hard_wallclock_gmt" or else
              Name (C) = "JB_execution_time" or else
              Name (C) = "JB_script_size" or else
              Name (C) = "JB_version" or else
              Name (C) = "JB_type" or else
              Name (C) = "JB_verify_suitable_queues" or else
              Name (C) = "JB_override_tickets" then
               null;

            elsif Name (C) /= "#text" then
               Ada.Text_IO.Put_Line ("Unknown Field: " & Name (C));
            end if;
         exception
               when E : Parser_Error =>
                  Record_Error (J, "information incomplete: " & Exception_Message (E));
               when E : others =>
                  Record_Error (J, "While parsing job: " & Exception_Message (E)
                                    & "Node type: """ & Name (C)
                             & """ Value: """ & Value (First_Child (C)) & """");
         end;
      end loop;

      if J.Queue = "" then
         J.Queue := To_Unbounded_String ("*");
      end if;

   exception
      when E : others =>
         raise Other_Error with Exception_Message (E)
          & "Node type: """ & Name (C)
                     & """ Value: """ & Value (First_Child (C)) & """";

   end Update_Job;

   ---------------------------
   -- Extract_Resource_List --
   ---------------------------

   procedure Extract_Resource_List (J              : in out Job;
                                    Resource_Nodes : Node_List;
                                    Soft : Boolean := False) is
      Resource_Tags      : Node_List;
      N, R               : Node;
      Res_Value          : Unbounded_String;
      Res_Name           : Unbounded_String;
      Res_Bool           : Boolean;
      Res_State          : Tri_State;
      Inserted           : Boolean;
      Inserted_At        : Resource_Lists.Cursor;

   begin
      for I in 1 .. Length (Resource_Nodes) loop
         N := Item (Resource_Nodes, I - 1);
         if Name (N) = "qstat_l_requests"
         or else Name (N) = "element" then
            Res_Bool := False;
            Res_State := Undecided;
            Resource_Tags := Child_Nodes (N);
            for J in 1 .. Length (Resource_Tags) loop
               R := Item (Resource_Tags, J - 1);
               if Name (R) = "CE_name" then
                  Res_Name := To_Unbounded_String (Value (First_Child (R)));
               elsif Name (R) = "CE_stringval" then
                  Res_Value := To_Unbounded_String (Value (First_Child (R)));
               elsif Name (R) = "CE_valtype" and then
                  Value (First_Child (R)) = "5" then
                  Res_Bool := True;
                  --  maybe check for relop here?
               end if;
            end loop;
            if Res_Bool then
               if Res_Value = "TRUE" or else
                 Res_Value = "true" or else
                 Res_Value = "1" then
                  Res_State := True;
               elsif Res_Value = "FALSE" or else
                 Res_Value = "false" or else
                  Res_Value = "0" then
                  Res_State := False;
               else
                  raise Constraint_Error
                    with  """" & To_String (Res_Value) & """ is not boolean";
               end if;
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
            Name (N) = "ulong_sublist" then
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
         if Name (N) = "destin_ident_list" then
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

   ----------------------
   -- Extract_PE_Range --
   ----------------------

   procedure Extract_PE_Range (J : in out Job; Children : Node_List) is
      Range_Nodes                      : Node_List;
      N, R                             : Node;
      Slots_Min, Slots_Step, Slots_Max : Natural;
   begin
      for I in 1 .. Length (Children) loop
         N := Item (Children, I - 1);
         if Name (N) = "ranges" then
            Range_Nodes := Child_Nodes (N);
            for J in 1 .. Length (Range_Nodes) loop
               R := Item (Range_Nodes, J - 1);
               if Name (R) = "RN_min" then
                  Slots_Min := Integer'Value (Value (First_Child (R)));
               elsif Name (R) = "RN_max" then
                  Slots_Max := Integer'Value (Value (First_Child (R)));
               elsif Name (R) = "RN_step" then
                  Slots_Step := Integer'Value (Value (First_Child (R)));
               end if;
            end loop;
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
         Name (Usage_Entry) = "scaled" then
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
                       and then Quantity (Quantity'First .. Quantity'First + 12) = "binding_inuse" then
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
           Name (Item (Task_List_Nodes, K - 1)) = "element" then
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
           or else Name (JA_Tasks) = "ulong_sublist" then
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


   procedure Extract_Context (Context       : in out Utils.String_Pairs.Map;
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
            Context.Include (Key => Variable_Name,
                             New_Item => Variable_Value);
         end if;
      end loop;
   end Extract_Context;

   ----------------
   -- Prune_List --
   ----------------


   procedure Prune_List_By_Slots (Slots : String) is
      Temp : Job_Lists.List;
      Pos  : Job_Lists.Cursor := List.First;
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
      List := Temp;
   end Prune_List_By_Slots;

   ------------------
   -- Sort_By      --
   --  Purpose:  Sort the job list by any column/field
   --  Parameter Field: Title of the column to sort by
   ------------------

   procedure Sort_By (Field : String; Direction : String) is
   begin
      if Field = "Number" or else Field = "ID" then
         Sorting_By_Number.Sort (List);
      elsif Field = "Name" then
         Sorting_By_Name.Sort (List);
      elsif Field = "Owner" then
         Sorting_By_Owner.Sort (List);
      elsif Field = "Priority" then
         Sorting_By_Priority.Sort (List);
      elsif Field = "Submitted" then
         Sorting_By_Submission_Time.Sort (List);
      elsif Field = "Slots" then
         Sorting_By_Slots.Sort (List);
      elsif Field = "State" then
         Sorting_By_State.Sort (List);
      elsif Field = "CPU" then
         Sorting_By_CPU_Used.Sort (List);
      elsif Field = "Memory" then
         Sorting_By_Memory_Used.Sort (List);
      elsif Field = "IO" then
         Sorting_By_IO_Used.Sort (List);
      elsif Field = "Priority" then
         Sorting_By_Priority.Sort (List);
      elsif Field = "O" then
         Sorting_By_Override.Sort (List);
      elsif Field = "S" then
         Sorting_By_Share.Sort (List);
      elsif Field = "F" then
         Sorting_By_Functional.Sort (List);
      elsif Field = "Urgency" then
         Sorting_By_Urgency.Sort (List);
      elsif Field = "Resource" then
         Sorting_By_Resource_Contrib.Sort (List);
      elsif Field = "Waiting" then
         Sorting_By_Waiting_Contrib.Sort (List);
      elsif Field = "Custom" then
         Sorting_By_Posix_Priority.Sort (List);
      elsif Field = "Ends In" or else
        Field = "Ends At" then
         Sorting_By_End.Sort (List);
      else
         raise Constraint_Error with "Sorting by " & Field & " unimplemented";
      end if;
      if Direction = "dec" then
         List.Reverse_Elements;
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
      elsif Hash (Left.Slot_List) < Hash (Right.Slot_List) then
         return True;
      elsif Hash (Left.Slot_List) > Hash (Right.Slot_List) then
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
      return Left.Number < Right.Number;
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
            J := New_Job (Child_Nodes (N));
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

   procedure Apply_Overlay_Entry (Position : Job_Lists.Cursor) is
   begin
      List.Update_Element (Position => Position,
                           Process  => Update_Job_From_Overlay'Access);
   end Apply_Overlay_Entry;

   procedure Apply_Overlay is
   begin
      List.Iterate (Apply_Overlay_Entry'Access);
   end Apply_Overlay;

   procedure Update_Quota_For_Job (J : in out Job) is
   begin
      J.RQS_Reached := SGE.Quota.Get_Headroom (User => J.Owner,
                                               Resource => "slots",
                                               PEs  => J.PE /= Null_Unbounded_String) < Get_Maximum_Slots (J);
   end Update_Quota_For_Job;

   procedure Quota_For_Job (Position : Job_Lists.Cursor) is
   begin
      List.Update_Element (Position => Position,
                           Process  => Update_Quota_For_Job'Access);
   end Quota_For_Job;

   procedure Update_Quota is
   begin
      List.Iterate (Quota_For_Job'Access);
   end Update_Quota;

   procedure Record_Error (J : in out Job; Message : String) is
   begin
      J.Error_Log.Append (To_Unbounded_String (Message));
   end Record_Error;

   procedure Iterate (Process : not null access procedure (J : Job)) is
      procedure Wrapper (Position : Job_Lists.Cursor) is
      begin
         Process (Element (Position));
      end Wrapper;

   begin
      List.Iterate (Wrapper'Access);
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

   procedure Iterate_Error_Log (J : Job;
                               Process : not null access procedure (Message : String))
   is
      procedure Wrapper (Position : Utils.String_Lists.Cursor) is
      begin
         Process (To_String (Element (Position)));
      end Wrapper;

   begin
      J.Error_Log.Iterate (Wrapper'Access);
   end Iterate_Error_Log;

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



end SGE.Jobs;
