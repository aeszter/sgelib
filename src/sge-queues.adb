with SGE.Resources; use SGE.Resources;
with SGE.Parser; use SGE.Parser;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;

package body SGE.Queues is
   use Queue_Lists;

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

   function Next return Queue is
   begin
      Next (List_Cursor);
      return Queue_Lists.Element (List_Cursor);
   end Next;

   function At_End return Boolean is
   begin
      if List_Cursor = Queue_Lists.No_Element or else
        List_Cursor = List.Last then
         return True;
      end if;
      return False;
   end At_End;

   function Current return Queue is
   begin
      return Queue_Lists.Element (List_Cursor);
   end Current;

   procedure Iterate (Process : not null access procedure (Q : Queue)) is
      procedure Wrapper (Position : Queue_Lists.Cursor) is
      begin
         Process (Element (Position));
      end Wrapper;

   begin
      List.Iterate (Wrapper'Access);
   end Iterate;

   procedure Iterate (Process : not null access procedure (Q : Queue);
                      Selector : not null access function (Q : Queue) return Boolean) is
      procedure Wrapper (Position : Queue_Lists.Cursor) is
         Q : Queue := Element (Position);
      begin
         if Selector (Q) then
            Process (Q);
         end if;
      end Wrapper;

   begin
      List.Iterate (Wrapper'Access);
   end Iterate;

   procedure Append_List (Input_Nodes : Node_List) is
   begin
      for Index in 1 .. Length (Input_Nodes) loop
         declare
            Queue_Nodes : Node_List := Child_Nodes (Item (Input_Nodes, Index - 1));
            N                     : Node;
            A                     : Attr;
            Used, Reserved, Total : Natural := 0;
            Slots                 : Natural := 0;
            State, Q_Type         : Unbounded_String;
            Mem, Runtime          : Unbounded_String;
            Cores                 : Natural := 0;
            SSD                   : Boolean := False;
            Network               : Resources.Network := none;
            Model, GPU, Queue_Name : Unbounded_String := Null_Unbounded_String;
            Long_Queue_Name       : Unbounded_String := Null_Unbounded_String;
            type small is digits 4 range 0.0 .. 1.0;
            type large is digits 4 range 0.0 .. 100.0;
         begin
            for Index in 1 .. Length (Queue_Nodes) loop
               N := Item (Queue_Nodes, Index - 1);
               if Name (N) = "slots_used" then
                  Used := Integer'Value (Value (First_Child (N)));
               elsif Name (N) = "slots_resv" then
                  Reserved := Integer'Value (Value (First_Child (N)));
               elsif Name (N) = "slots_total" then
                  Total := Integer'Value (Value (First_Child (N)));
               elsif Name (N) = "state" then
                  State := To_Unbounded_String (Value (First_Child (N)));
               elsif Name (N) = "qtype" then
                  Q_Type := To_Unbounded_String (Value (First_Child (N)));
               elsif Name (N) = "resource" then
                  A := Get_Attr (N, "name");
                  if Value (A) = "mem_total" then
                     Mem := To_Unbounded_String (Value (First_Child (N)));
                  elsif Value (A) = "num_proc" then
                     Cores := Integer'Value (Value (First_Child (N)));
                  elsif Value (A) = "infiniband" and then
                    small'Value (Value (First_Child (N))) = 1.0 and then
                    Network = none then
                     Network := ib;
                  elsif Value (A) = "ib-switch" and then
                    small'Value (Value (First_Child (N))) = 1.0 then
                     Network := ibswitch;
                  elsif Value (A) = "ethernet" and then
                    small'Value (Value (First_Child (N))) = 1.0 then
                     Network := eth;
                  elsif Value (A) = "h_rt" then
                     Runtime := To_Unbounded_String (Value (First_Child (N)));
                  elsif Value (A) = "slots" then
                     Slots := Integer (large'Value (Value (First_Child (N))));
                  elsif Value (A) = "cpu_model" then
                     Model := To_Unbounded_String (Value (First_Child (N)));
                  elsif Value (A) = "qname" then
                     Queue_Name := To_Unbounded_String (Value (First_Child (N)));
                  elsif Value (A) = "ssd"  then
                     SSD := True; -- consumable, so do not check numerical value
                  elsif Value (A) = "gpu_model"  then
                     GPU := To_Unbounded_String (Value (First_Child (N)));
                  end if;
               elsif Name (N) = "name" then
                  Long_Queue_Name := To_Unbounded_String (Value (First_Child (N)));
               end if;
            end loop;

            List.Append (New_Queue (Used     => Used,
                                    Reserved => Reserved,
                                    Total    => Total,
                                    Memory   => To_String (Mem),
                                    Cores    => Cores,
                                    Slots    => Slots,
                                    Network  => Network,
                                    Model    => To_Model (Model),
                                    SSD      => SSD,
                                    GPU      => To_GPU (GPU),
                                    Runtime  => Runtime,
                                    Name     => Queue_Name,
                                    Long_Name => Long_Queue_Name,
                                    State     => To_String (State),
                                    Q_Type => To_String (Q_Type)
                                   ));
         end;
      end loop;
   end Append_List;

   ---------------
   -- New_Queue --
   --  Purpose: Create a new queue with the given resources and slots
   --  Parameter Used: Number of slots in use
   --  Parameter Reserved: Number of slots used for advance reservations
   --  Parameter Total: Number of total slots in queue
   --  Parameter Memory: RAM in queue
   --  Parameter Cores: number of cores in queue
   --  Parameter Network: type of network in queue
   --  Parameter Runtime: runtime limit of queue
   --  Returns:  the newly created queue
   ---------------

   function New_Queue
     (Used, Reserved, Total : Natural;
      State, Q_Type         : String;
      Memory                : String;
      Cores, Slots          : Natural;
      Network               : Resources.Network;
      SSD                   : Boolean;
      GPU                   : Resources.GPU_Model;
      Model                 : Resources.CPU_Model;
      Runtime               : Unbounded_String;
      Name                  : Unbounded_String;
      Long_Name             : Unbounded_String
     )
      return Queue
   is
      Q : Queue;
   begin
      Q.Used     := Used;
      Q.Reserved := Reserved;
      Q.Total    := Total;
      Q.Long_Name := Long_Name;
      for Pos in State'Range loop
         case State (Pos) is
            when 'a' => Q.State (alarm) := True;
            when 'E' => Q.State (error) := True;
            when 'd' => Q.State (disabled) := True;
            when 'u' => Q.State (unreachable) := True;
            when 'o' => Q.State (old) := True;
            when 'S' => Q.State (suspended) := True;
            when others => raise Constraint_Error
                 with "Queue State has an unknown character: " & State (Pos);
         end case;
      end loop;
      for Pos in Q_Type'Range loop
         case Q_Type (Pos) is
            when 'B' => Q.Q_Type (B) := True;
            when 'I' => Q.Q_Type (I) := True;
            when 'P' => Q.Q_Type (P) := True;
            when others => raise Constraint_Error
               with "Queue Type has an unknown character: " & Q_Type (Pos);
         end case;
      end loop;

      Set_Memory (Q.Properties, Memory);
      Set_Network (Q.Properties, Network);
      Set_Model (Q.Properties, Model);
      Set_Runtime (Q.Properties, Runtime);
      Q.Name     := Name;
      if Cores = 0 then
         Set_Cores (Q.Properties, Q.Total);
      else
         Set_Cores (Q.Properties, Cores);
      end if;
      Set_Slots (Q.Properties, Slots);
      if SSD then
         Set_SSD (Q.Properties);
      end if;
      Set_GPU (Q.Properties, GPU);

      return Q;
   end New_Queue;

   ---------------------------
   -- Precedes_By_Resources --
   ---------------------------

   function Precedes_By_Resources (Left, Right : Queue) return Boolean is
   begin
      return Left.Properties < Right.Properties;
   end Precedes_By_Resources;


   function Get_Slot_Count (Q : Queue) return Natural is
   begin
      return Q.Total;
   end Get_Slot_Count;

   function Get_Used_Slots (Q : Queue) return Natural is
   begin
      return Q.Used;
   end Get_Used_Slots;

   function Get_Reserved_Slots (Q : Queue) return Natural is
   begin
      return Q.Reserved;
   end Get_Reserved_Slots;

   function Get_Free_Slots (Q : Queue) return Natural is
   begin
      --  return Q.Total - Q.Used; -- see Bug #2000
      return Get_Slots (Q.Properties);
   end Get_Free_Slots;

   function Is_Offline (Q : Queue) return Boolean is
   begin
      return Has_Unreachable (Q);
   end Is_Offline;

   function Is_Disabled (Q : Queue) return Boolean is
   begin
      return Has_Disabled (Q) and then not Has_Unreachable (Q);
   end Is_Disabled;

   function Is_Suspended (Q : Queue) return Boolean is
   begin
      return Has_Suspended (Q)
        and then not Has_Disabled (Q)
        and then not Has_Unreachable (Q);
   end Is_Suspended;

   function Get_Properties (Q : Queue) return Set_Of_Properties is
   begin
      return Q.Properties;
   end Get_Properties;

   function Get_Name (Q : Queue) return Unbounded_String is
   begin
      return Q.Name;
   end Get_Name;

   function Get_Name (Q : Queue) return String is
   begin
      return To_String (Q.Name);
   end Get_Name;

   function Get_Long_Name (Q : Queue) return String is
   begin
      return To_String (Q.Long_Name);
   end Get_Long_Name;

   function Get_Host_Name (Q : Queue) return String is
      Src : String := To_String (Q.Long_Name);
      Start : Positive := Index (Source  => Src,
                                 Pattern => "@");
      Stop  : Positive := Index (Source  => Src,
                                    From => Start,
                                 Pattern => ".");
   begin
      return Src (Start .. Stop);
   end Get_Host_Name;

   function Has_Error (Q : Queue) return Boolean is
   begin
      return Q.State (error);
   end Has_Error;

   function Has_Disabled (Q : Queue) return Boolean is
   begin
      return Q.State (disabled);
   end Has_Disabled;

   function Has_Unreachable (Q : Queue) return Boolean is
   begin
      return Q.State (unreachable);
   end Has_Unreachable;

   function Has_Suspended (Q : Queue) return Boolean is
   begin
      return Q.State (suspended);
   end Has_Suspended;

   function Has_Old_Config (Q : Queue) return Boolean is
   begin
      return Q.State (old);
   end Has_Old_Config;

   function Is_Batch (Q : Queue) return Boolean is
   begin
      return Q.Q_Type (B);
   end Is_Batch;

   function Is_Interactive (Q : Queue) return Boolean is
   begin
      return Q.Q_Type (I);
   end Is_Interactive;

   function Is_Parallel (Q : Queue) return Boolean is
   begin
      return Q.Q_Type (P);
   end Is_Parallel;

   function Get_Type (Q : Queue) return String is
      Type_String : String := "   ";
   begin
      if Q.Q_Type (B) then
         Type_String (1) := 'B';
      end if;
      if Q.Q_Type (I) then
         Type_String (2) := 'I';
      end if;
      if Q.Q_Type (P) then
         Type_String (3) := 'P';
      end if;
      return Type_String;
   end Get_Type;



end SGE.Queues;
