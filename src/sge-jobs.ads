with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Ordered_Maps;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Calendar; use Ada.Calendar;
with SGE.Resources;
with SGE.Parser; use SGE.Parser;
with SGE.Ranges; use SGE.Ranges;
with SGE.Utils; use SGE.Utils;
with SGE.Context;
with Ada.Strings.Bounded;
with SGE.Loggers; use SGE.Loggers;

package SGE.Jobs is
   Other_Error : exception; -- generic error
   Too_Many_Jobs_Error : exception; -- Operation usupported if Length (List) > 1
   Missing_Tag_Error : exception; -- an expected XML tag was not found


   type State_Flag is (deletion, Error, hold, running, Restarted, suspended,
                       Q_Suspended, transfering, Threshold, waiting);
   type State_Count is array (State_Flag) of Natural;
   type State is array (State_Flag) of Boolean;
   type Usage_Type is (cpu, mem, io, iow, vmem, maxvmem,
                       ru_wallclock, ru_utime, ru_stime, ru_maxrss, ru_ixrss,
                       submission_time, start_time, end_time,
                       priority, exit_status, signal);
   type Usage is array (Usage_Type) of Usage_Number;
   type Posix_Priority_Type is range -1_023 .. 1_024;
   type Balancer_Capability is (CPU_GPU, Low_Cores, High_Cores, Any);
   type Balancer_Support is array (Balancer_Capability) of Boolean;


   type Job is new Logger with private;
   type List is private;
   type Cursor is private;
   procedure Clear (Collection : in out List);
   function Length (Collection : List) return Natural;
   function Is_Sorted (Collection : List) return Boolean;

   function First (Collection : List) return Cursor;
   procedure Next (Position : in out Cursor);
   function Has_Element (Position : Cursor) return Boolean;
   function Element (Position : Cursor) return Job;

   function To_Abbrev (Flag : State_Flag) return String;
   function To_String (Flag : State_Flag) return String;

--   function Count (Collection : List) return Natural;
   function Count (Collection : List;
                   Predicate : not null access function (J : Job) return Boolean)
                   return Natural;

   function To_String (Capability : Balancer_Capability) return String;
   function To_Memory (Amount : Usage_Integer) return String;
   --  Purpose: Compose a memory quantity consisting of a number and a unit
   --  Returns: A string of the form xx MB, where xx is a number not exceeding
   --           five digits, and MB is a unit (actually kB, MB, or GB)

   function On_Hold (J : Job) return Boolean;
   function Has_Error (J : Job) return Boolean;
   function Is_Running (J : Job) return Boolean;
   function Quota_Inhibited (J : Job) return Boolean;

   function End_Time (J : Job) return Time;
   function Remaining_Time (J : Job) return Duration;
   function Get_Task_Count (J : Job) return Natural;
   function Get_Task_IDs (J : Job) return Ranges.Step_Range_List;
   function Get_ID (J : Job) return String;
   function Get_ID (J : Job) return Positive;
   function Get_PE (J : Job) return Unbounded_String;
   function Get_Granted_PE (J : Job) return Unbounded_String;
   function Get_Slot_List (J : Job) return Ranges.Step_Range_List;
   function Get_Slot_Number (J : Job) return Unbounded_String;

   --  maximum lower bound of the slot list
   function Get_Minimum_Slots (J : Job) return Positive;

   --  minimum upper bound of the slot list
   function Get_Maximum_Slots (J : Job) return Positive;

   --  maximum lower bound on the slot list assuming the job is migrated
   --  to a (pure) cpu queue by the balancer
   function Get_Minimum_CPU_Slots (J : Job) return Positive;

   --  minimum upper bound on the slot list assuming the job is migrated
   --  to a (pure) cpu queue by the balancer
   function Get_Maximum_CPU_Slots (J : Job) return Positive;

   function Get_Queue (J : Job) return Unbounded_String;
   function Get_Hard_Resources (J : Job) return Resources.Hashed_List;
   function Get_Soft_Resources (J : Job) return Resources.Hashed_List;
   function Get_Hard_Resources (J : Job) return String;
   function Get_Soft_Resources (J : Job) return String;
   function Supports_Balancer (J : Job; What : Balancer_Capability := Any) return Boolean;
   function Get_Name (J : Job) return String;
   function Get_Full_Name (J : Job) return String;
   function Is_Name_Truncated (J : Job) return Boolean;
   function Get_Owner (J : Job) return User_Name;
   function Get_Group (J : Job) return String;
   function Get_Account (J : Job) return String;
   function Get_Submission_Time (J : Job) return Ada.Calendar.Time;
   function Get_Advance_Reservation (J : Job) return String;
   function Has_Reserve (J : Job) return Tri_State;
   function Get_State (J : Job) return String;
   function Get_Directory (J : Job) return String;
   function Get_Script_File (J : Job) return String;
   function Get_Args (J : Job) return String_List;
   function Get_Exec_File (J : Job) return String;
   function Get_Std_Out_Paths (J : Job) return String_List;
   function Get_Std_Err_Paths (J : Job) return String_List;
   function Is_Merge_Std_Err (J : Job) return Tri_State;
   function Has_Notify (J : Job) return Tri_State;
   function Get_Task_List (J : Job) return String_Sets.Set;
   function Get_Detected_Queues (J : Job) return String_Sets.Set;
   function Get_Context (J : Job; Key : Context.Key_Type) return String;
   function Has_Context (J : Job; Key : Context.Key_Type) return Boolean;
   function Has_Context (J : Job) return Boolean;
   function Get_Last_Extension (J : Job) return Time;
   function Get_Last_Migration (J : Job) return Time;
   function Get_Last_Reduction (J : Job) return Time;
   function Get_CPU_Range (J : Job) return String;
   function Get_GPU_Range (J : Job) return String;
   function Get_Reduce_Wait (J : Job) return Natural;
   function Get_Reduced_Slots (J : Job) return String;
   function Get_Extended_Slots (J : Job) return String;
   function Get_Reduced_Runtime (J : Job) return String;
   function Get_Priority (J : Job) return Utils.Fixed;
   function Get_Project (J : Job) return String;
   function Get_Override_Tickets (J : Job) return Natural;
   function Get_Share_Tickets (J : Job) return Natural;
   function Get_Functional_Tickets (J : Job) return Natural;
   function Get_Urgency (J : Job) return Fixed;
   function Get_Resource_Contrib (J : Job) return Natural;
   function Get_Waiting_Contrib (J : Job) return Natural;
   function Get_Posix_Priority (J : Job) return Posix_Priority_Type;
   function Get_JAT_Usage  (J : Job) return Usage;
   function Get_PET_Usage  (J : Job) return Usage;
   function Get_CPU (J : Job) return Float;
   function Get_Mem (J : Job) return Float;
   function Get_IO (J : Job) return Float;

   function Has_Error_Log_Entries (J : Job) return Boolean;
   -----------------
   -- Get_Summary --
   --  Purpose: Count the number of jobs per state from the List
   -----------------
   procedure Get_Summary (Collection   : List;
                          Tasks, Slots : out State_Count);


   -------------
   -- New_Job --
   --  Purpose: Create a new job and populate its fields from a given list
   --  of XML nodes
   --  Parameter List: A list of XML nodes containing information about the job
   --  Returns: The newly created job
   -------------

   function New_Job (List : Node_List) return Job;

   ----------------
   -- Update_Job --
   --  Purpose: Populate the fields of a given job from a list of XML nodes
   --  Parameter J: The job to update
   --  Parameter List: A list of XML nodes containing information about the job
   ----------------
   procedure Update_Job (J : in out Job; List : Node_List);
   procedure Extract_Resource_List (J              : in out Job;
                                    Resource_Nodes : Node_List;
                                    Soft           : Boolean := False);
   procedure Extract_Queue_List (J : in out Job; Destin_Nodes : Node_List);
   procedure Extract_Hold_ID_List (ID_List         : in out Utils.ID_List;
                                       Sub_Nodes : Node_List);
   procedure Extract_Tasks (J : in out Job; Task_Nodes : Node_List);
   procedure Extract_PE_Range (J : in out Job; Children : Node_List);
   procedure Extract_Array (J : in out Job; Task_Nodes : Node_List);
   procedure Extract_Paths (Path_List  : in out String_Lists.List;
                            List_Nodes  : Node_List);
   procedure Extract_Args (J : in out Job;
                           Arg_Nodes : Node_List);
   procedure Extract_Context (Context       : in out SGE.Context.List;
                              Context_Nodes : Node_List);
   procedure Add_Message (J : in out Job; Number : Natural; Message : Unbounded_String);

   -----------------
   -- Append_List --
   --  Purpose: Read Jobs from a given list of (DOM) Nodes and populate the List
   --  accordingly
   -----------------

   procedure Append (Collection : in out List; Nodes : Node_List) with
     Post => not Empty (Collection);

   -----------------
   -- Update_Messages --
   --  Purpose: Read Scheduler Messages from a given list of (DOM) Nodes and
   --  update the List accordingly
   --  Note: Since there is no direct association between messages and jobs,
   --  this will not work properly unless there is only a single job in the List.
   --  Apparently, qstat -j -xml will only output a single job, too, so this should not pose a problem.
   --  Raise: Too_Many_Jobs_Error if there is more than one entry in the List.
   -----------------

   procedure Update_Messages (Collection : in out List; Nodes : Node_List);

   --------------------
   -- Create_Overlay --
   --  Purpose: Create a Set with values read from qstat -j
   --------------------

   procedure Create_Overlay (Nodes : Node_List);

   -------------------
   -- Apply_Overlay --
   --  Purpose: For each job in the global List, update its field from the
   --  Set created with Create_Overlay
   -------------------

   procedure Apply_Overlay (Collection : in out List);
   procedure Update_Quota (Collection : in out List);


   -----------------
   -- Prune_List --
   --  Purpose: Remove Jobs not matching certain criteria
   -----------------

   procedure Prune (Collection : in out List;
                    Keep       : not null access function (J : Job) return Boolean);
   procedure Prune (Collection : in out List;
                    PE, Queue, Hard_Requests,
                    Soft_Requests,
                    Slot_Number, Slot_Ranges : Unbounded_String);

   procedure Prune_By_Slots (Collection : in out List; Slots : String);
   --  outdated. move functionality to Append_List. Does GPS notice this?

   procedure Sort_By (Collection : in out List;
                      Field      : String; Direction : String);
   function Precedes_By_Name (Left, Right : Job) return Boolean;
   function Precedes_By_Number (Left, Right : Job) return Boolean;
   function Precedes_By_Owner (Left, Right : Job) return Boolean;
   function Precedes_By_Priority (Left, Right : Job) return Boolean;
   function Precedes_By_Submission_Time (Left, Right : Job) return Boolean;
   function Precedes_By_Slots (Left, Right : Job) return Boolean;
   function Precedes_By_State (Left, Right : Job) return Boolean;
   function Precedes_By_CPU_Used (Left, Right : Job) return Boolean;
   function Precedes_By_Memory_Used (Left, Right : Job) return Boolean;
   function Precedes_By_IO_Used (Left, Right : Job) return Boolean;
   function Precedes_By_Override (Left, Right : Job) return Boolean;
   function Precedes_By_Share (Left, Right : Job) return Boolean;
   function Precedes_By_Functional (Left, Right : Job) return Boolean;
   function Precedes_By_Urgency (Left, Right : Job) return Boolean;
   function Precedes_By_Waiting_Contrib (Left, Right : Job) return Boolean;
   function Precedes_By_Resource_Contrib (Left, Right : Job) return Boolean;
   function Precedes_By_Posix_Priority (Left, Right : Job) return Boolean;
   function Precedes_By_End (Left, Right : Job) return Boolean;

   function Precedes_By_Resources (Left, Right : Job) return Boolean;

   function Same (Left, Right : Job) return Boolean;

   procedure Update_Status (Collection : in out List);
   --  Purpose: Update all jobs' status
   procedure Search_Queues (Collection : in out List);
   --  Look for slots occupied by a job

   procedure Sort (Collection : in out List);
   --  Sort the job list by resources
   function Empty (Collection : List) return Boolean;
   --  is the job list empty?

   function Find_Job (Collection : List; ID : Natural) return Job;


   -------------
   -- Iterate --
   -------------

   procedure Iterate (Collection : List;
                      Process    : not null access procedure (J : Job));
   procedure Iterate_Predecessors (J       : Job;
                                   Process : not null access procedure (ID : Natural));
   procedure Iterate_Predecessor_Requests (J : Job; Process : not null access procedure (S : String));
   procedure Iterate_Successors (J       : Job;
                                 Process : not null access procedure (ID : Natural));
   procedure Iterate_Messages (J : Job;
                               Process : not null access procedure (Message : String));
   procedure Iterate_Queues (J : Job;
                             Process : not null access procedure (Queue : String));
   procedure Iterate_Slots (J : Job;
                            Process : not null access procedure (R : Step_Range));
   procedure Iterate_Tasks (J : Job;
                            Process : not null access procedure (R : Step_Range));
   procedure Iterate_Context (J : Job;
                              Process : not null access procedure (Key, Element : String));


   Max_Name_Length : constant Positive := 25;

private
   package Job_Names is new Ada.Strings.Bounded.Generic_Bounded_Length (Max => Max_Name_Length);
   subtype Job_Name is Job_Names.Bounded_String;

   procedure Update_Status (J : in out Job);
   --  Purpose: Read the job's status from an appropriate source
   --  (such as a qstat -u call)

   type Job is new Logger with record
      --  basic attributes
      Number               : Integer; -- Job ID
      Task_IDs             : Ranges.Step_Range_List;
      Full_Name            : Unbounded_String; -- Job name
      Name                 : Job_Name; -- Job name, truncated to Max_J_Name_Length
      Name_Truncated       : Boolean;          -- Whether Full_Name and Name differ
      Owner                : Utils.User_Name; -- User whom this job belongs to
      Group                : Unbounded_String;
      Account              : Unbounded_String;

      Priority             : Fixed; -- Numerical priority
      State_Array          : State;
      State_String         : String (1 .. 4);
      Slot_Number          : Unbounded_String; -- how many slots/CPUs to use
      PE, Granted_PE       : Unbounded_String; -- Parallel environment
      Submission_Time      : Time;    -- when submitted
      Project              : Unbounded_String;
      Department           : Unbounded_String;
      Job_Advance_Reservation            : Unbounded_String;
      Notify               : Tri_State;
      JAT_Usage, PET_Usage : Usage := (others => 0.0);
      Predecessors         : Utils.ID_List;
      Successors           : Utils.ID_List;
      Predecessor_Request  : Utils.String_List;
      Context              : SGE.Context.List;
      Array_Tasks          : Ranges.Step_Range_List;


      --  File related stuff
      Exec_File          : Unbounded_String;
      Script_File        : Unbounded_String;
      Directory          : Unbounded_String;
      Reserve            : Tri_State;
      Merge_Std_Err      : Tri_State;
      Args               : Utils.String_List;


      --  qstat -ext
      CPU, Mem, IO       : Float;
      Override_Tickets   : Natural;
      Share_Tickets      : Natural;
      Functional_Tickets : Natural;

      --  qstat -urg
      Urgency          : Fixed;
      Resource_Contrib : Natural;
      Waiting_Contrib  : Natural;

      --  qstat -pri
      Posix_Priority   : Posix_Priority_Type;

      --  qhost -j
      Detected_Queues  : String_Sets.Set;

      --  resources used for Bunching jobs
      Queue            : Unbounded_String;
      Hard, Soft       : Resources.Hashed_List;

      Slot_List        : Ranges.Step_Range_List;
      Queue_List       : String_Lists.List;
      Message_List     : String_Lists.List;
      Task_List        : String_Sets.Set;

      Std_Out_Paths    : String_Lists.List;
      Std_Err_Paths    : String_Lists.List;

      RQS_Reached      : Boolean;
      Balancer         : Balancer_Support;
   end record;

   package Job_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Element_Type => Job, "=" => Same);

   package Job_Maps is
     new Ada.Containers.Ordered_Maps (Key_Type     => Integer,
                                      Element_Type => Job,
                                      "<"          => "<",
                                      "="          => Same);

   function Find_Job (Collection : List; ID : Natural) return Job_Lists.Cursor;
   --  if the job list contains a job with the given ID, return a cursor
   --  pointing there;
   --  otherwise, return No_Element



   package Sorting_By_Name is
     new Job_Lists.Generic_Sorting
       ("<" => Precedes_By_Name);


   package Sorting_By_Number is
     new Job_Lists.Generic_Sorting ("<" => Precedes_By_Number);
   package Sorting_By_Owner is
     new Job_Lists.Generic_Sorting ("<" => Precedes_By_Owner);
   package Sorting_By_Priority is
     new Job_Lists.Generic_Sorting ("<" => Precedes_By_Priority);
   package Sorting_By_Submission_Time is
     new Job_Lists.Generic_Sorting ("<" => Precedes_By_Submission_Time);
   package Sorting_By_Slots is
     new Job_Lists.Generic_Sorting ("<" => Precedes_By_Slots);
   package Sorting_By_State is
     new Job_Lists.Generic_Sorting ("<" => Precedes_By_State);
   package Sorting_By_CPU_Used is
      new Job_Lists.Generic_Sorting ("<" => Precedes_By_CPU_Used);
   package Sorting_By_Memory_Used is
      new Job_Lists.Generic_Sorting ("<" => Precedes_By_Memory_Used);
   package Sorting_By_IO_Used is
      new Job_Lists.Generic_Sorting ("<" => Precedes_By_IO_Used);
   package Sorting_By_Override is
      new Job_Lists.Generic_Sorting ("<" => Precedes_By_Override);
   package Sorting_By_Share is
      new Job_Lists.Generic_Sorting ("<" => Precedes_By_Share);
   package Sorting_By_Functional is
      new Job_Lists.Generic_Sorting ("<" => Precedes_By_Functional);
   package Sorting_By_Urgency is
      new Job_Lists.Generic_Sorting ("<" => Precedes_By_Urgency);
   package Sorting_By_Resource_Contrib is
      new Job_Lists.Generic_Sorting ("<" => Precedes_By_Resource_Contrib);
   package Sorting_By_Waiting_Contrib is
      new Job_Lists.Generic_Sorting ("<" => Precedes_By_Waiting_Contrib);
   package Sorting_By_Posix_Priority is
     new Job_Lists.Generic_Sorting ("<" => Precedes_By_Posix_Priority);
   package Sorting_By_End is
      new Job_Lists.Generic_Sorting ("<" => Precedes_By_End);

   package Sorting_By_Resources is
      new Job_Lists.Generic_Sorting ("<" => Precedes_By_Resources);


   type List is  record
      Container : Job_Lists.List;
   end record;
   type Cursor is new Job_Lists.Cursor;
   Overlay : Job_Maps.Map;

end SGE.Jobs;
