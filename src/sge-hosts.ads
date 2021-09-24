with Ada.Containers.Doubly_Linked_Lists;
with SGE.Host_Properties; use SGE.Host_Properties;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Ordered_Maps;
with Ada.Strings.Bounded; use Ada.Strings.Bounded;
with SGE.Resources;
with SGE.Parser; use SGE.Parser;
with Ada.Calendar; use Ada.Calendar;

package SGE.Hosts is

   type Job is private;
   type Host is private;
   type Queue is private;
   type Queue_Name is private;
   type Queue_Pointer is private;


   procedure Append_List (Host_Nodes : Node_List);
   procedure Prune_List (Requirements : Set_Of_Properties; Slots : Positive);

   procedure Iterate (Process : access procedure (H : Host));
   procedure Iterate (Process : access procedure (H : Host);
                      Selector : access function (H : Host) return Boolean);
   procedure Iterate_Queues (H : Host; Process : not null access procedure (Q : Queue_Pointer));
   procedure Iterate_Jobs (H : Host; Process : not null access procedure (J : Job));

   procedure Update_Used_Slots (H : in out Host);

   ------------------
   -- Sort_By      --
   --  Purpose:  Sort the host list by any column/field
   --  Parameter Field: Title of the column to sort by
   ------------------
   procedure Sort_By (Field : String; Direction : String);

   type Percent is range 0 .. 100;

   -----------------------
   --
   --  Getters
   --
   -----------------------
   function Load_Per_Core (H : Host) return Load;
   function Get_Load (H : Host) return Load;
   function Get_Load_One (H : Host) return Load;
   function Mem_Ratio (H : Host) return Fixed;
   function Mem_Percentage (H : Host) return Percent;
   function Swap_Ratio (H : Host) return Fixed;
   function Swap_Percentage (H : Host) return Percent;
   function Color_Class (P : Percent) return String;
   function Color_Class (Load : Host_Properties.Load) return String;
   function Get_Free_Slots (H : Host) return Natural;
   function Get_Reserved_Slots (H : Host) return Natural;
   function Get_Used_Slots (H : Host) return Natural;
   function Get_Name (H : Host) return Host_Name;
   function Has_Queue (H : Host; Slots : Positive) return Boolean;
   function Has_Unreachable_Queue (H : Host) return Boolean;
   function Get_Network (H : Host) return String;
   function Get_Model (H : Host) return SGE.Resources.CPU_Model;
   function Get_GPU (H : Host) return SGE.Resources.GPU_Model;
   function Get_GPU_Count (H : Host) return Integer;
   function Get_Cores (H : Host) return Positive;
   function Get_Kernel_Release (H : Host) return String;
   function Get_Memory (H : Host) return String;
   function Queue_Count (H : Host) return Natural;

   function Is_Master (J : Job) return Boolean;
   function Has_Slaves (J : Job) return Boolean;
   function Get_Slaves (J : Job) return Natural;
   function Get_ID (J  : Job) return Positive;
   function Get_Full_ID (J : Job) return String;
   function Get_Start_Time (J  : Job) return Ada.Calendar.Time;

   function Get_State (Q : Queue_Pointer) return String;
   function Get_Slots (Q : Queue_Pointer) return Natural;
   function Get_Name (Q : Queue_Pointer) return String;
   function Get_Reserved_Slots (Q : Queue_Pointer) return Natural;

private

   type Job is record
      Master  : Boolean;
      ID      : Positive;
      Task_ID : Natural := 0;
      Slaves  : Natural := 0;
      Start_Time : Time;
   end record;

   -----------------------
   --
   --  Jobs
   --
   -----------------------

   function Equal (Left, Right : Job) return Boolean;
   procedure Add_Slave_Process (J : in out Job);
   procedure Set_Master (J : in out Job; PE_Master : String);

   package Job_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Element_Type => Job,
                                             "="          => Equal);
   subtype Job_List is Job_Lists.List;


   -----------------------
   --
   --  Queues
   --
   -----------------------
   package Queue_States is
     new Generic_Bounded_Length (Max => 5);

   type Queue is record
      State : Queue_States.Bounded_String;
      Slots : Natural;
      Reserved : Natural;
   end record;

   package Queue_Maps is
     new Ada.Containers.Ordered_Maps (Key_Type => Unbounded_String,
                                      Element_Type => Queue);
   subtype Queue_Map is Queue_Maps.Map;
   type Queue_Name is new Unbounded_String;
   type Queue_Pointer is new Queue_Maps.Cursor;

   ------------------
   -- Append_Queue --
   --  Purpose: Store information about one queue associated with this host
   --  Parameter H: The Host object to store the information
   --  Parameter Name: The name of the queue
   --  Parameter State: The state of the queue
   ------------------
   procedure Append_Queue (H    : out Host;
                           Name  : String;
                           State : String := "";
                           Slots : Natural := 0;
                           Reserved : Natural := 0);

   procedure Update_Or_Append_Queue (H    : out Host;
                           Name  : String;
                           State : String := "";
                                     Slots : Natural := 0;
                                    Reserved : Natural := 0);


   type Host is record
      Name       : Host_Name;
      Jobs       : Job_List;
      Properties : Set_Of_Properties;
      Load       : SGE.Host_Properties.Load;
      Slots_Used : Natural := 0;
      Mem_Used   : Resources.Gigs;
      Swap_Total : Resources.Gigs;
      Swap_Used  : Resources.Gigs;
      Queues     : Queue_Map;
   end record;


   package Host_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Element_Type => Host);

   Host_List : Host_Lists.List;

   procedure Compactify (List : in out Job_List);

   ------------------------------------
   --
   --  Sorting
   --
   ------------------------------------
   function Precedes_By_Free (Left, Right : Host) return Boolean;
   function Precedes_By_Net (Left, Right : Host) return Boolean;
   function Precedes_By_Cores (Left, Right : Host) return Boolean;
   function Precedes_By_RAM (Left, Right : Host) return Boolean;
   function Precedes_By_Load (Left, Right : Host) return Boolean;
   function Precedes_By_Mem (Left, Right : Host) return Boolean;
   function Precedes_By_Swap (Left, Right : Host) return Boolean;
   function Precedes_By_Name (Left, Right : Host) return Boolean;


   package By_Free is
     new Host_Lists.Generic_Sorting ("<" => Precedes_By_Free);
   package By_Net is
     new Host_Lists.Generic_Sorting ("<" => Precedes_By_Net);
   package By_Cores is
     new Host_Lists.Generic_Sorting ("<" => Precedes_By_Cores);
   package By_RAM is
     new Host_Lists.Generic_Sorting ("<" => Precedes_By_RAM);
   package By_Load is
     new Host_Lists.Generic_Sorting ("<" => Precedes_By_Load);
   package By_Mem is
     new Host_Lists.Generic_Sorting ("<" => Precedes_By_Mem);
   package By_Swap is
     new Host_Lists.Generic_Sorting ("<" => Precedes_By_Swap);
   package By_Name is
     new Host_Lists.Generic_Sorting ("<" => Precedes_By_Name);



   -----------------------
   --
   --  Parsing
   --
   -----------------------
   procedure Parse_Queue (H : in out Host; N : Node);
   ---------------------
   -- Parse_Queue --
   --  Purpose: Given a Node of an XML DOM tree,
   --  read queue attributes
   --  Parameter H : The Host record to update
   --  Parameter V : The Node to read from
   ---------------------
   procedure Parse_Hostvalue (H : in out Host; N : Node);
   procedure Parse_Job (H : in out Host; N : Node);

end SGE.Hosts;
