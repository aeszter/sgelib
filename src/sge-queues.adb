with SGE.Resources; use SGE.Resources;
with SGE.Parser; use SGE.Parser;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with SGE.Loggers;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package body SGE.Queues is
   use Queue_Lists;

   procedure Clear is
   begin
   end;

   function Length return Natural is
   begin
   end;

   function Is_Empty return Boolean is
   begin
      end;

   procedure Occupy_Slots (Q : in out Queue; How_Many : Natural) is
   begin
      if How_Many > Q.Total - Q.Used - Q.Reserved then
         raise Constraint_Error with "Not enough free slots";
      end if;
      Q.Used := Q.Used + How_Many;
   end Occupy_Slots;

   procedure Append_List (Storage : in out List; Input_Nodes : Node_List) is
   begin
      for Index in 1 .. Length (Input_Nodes) loop
         declare
            Queue_Nodes : Node_List := Child_Nodes (Item (Input_Nodes, Index - 1));
         begin
            Storage.Data.Append (New_Queue (Queue_Nodes));
         exception
            when E : others =>
               Loggers.Record_Error ("Queue suppressed: " & Exception_Message (E));
         end;
      end loop;
   end Append_List;

   function New_Queue (List : Node_List) return Queue
   is
      Q : Queue;
            N                     : Node;
            A                     : Attr;
            Network               : Resources.Network := none;

            type small is digits 4 range 0.0 .. 1.0;
            type large is digits 4 range 0.0 .. 100.0;

   begin
      for Index in 1 .. Length (List) loop
         N := Item (List, Index - 1);
         if Name (N) = "slots_used" then
            Q.Used := Integer'Value (Value (First_Child (N)));
         elsif Name (N) = "slots_resv" then
            Q.Reserved := Integer'Value (Value (First_Child (N)));
         elsif Name (N) = "slots_total" then
            Q.Total := Integer'Value (Value (First_Child (N)));
         elsif Name (N) = "state" then
            declare
               State : String := Value (First_Child (N));
            begin
               for Pos in State'Range loop
                  case State (Pos) is
                     when 'a' => Q.State (alarm) := True;
                     when 'E' => Q.State (error) := True;
                     when 'd' => Q.State (disabled) := True;
                     when 'u' => Q.State (unreachable) := True;
                     when 'o' => Q.State (old) := True;
                     when 'S' => Q.State (suspended) := True;
                     when 'D' => Q.State (calendar_disabled) := True;
                     when others => raise Constraint_Error
                          with "Queue State has an unknown character: " & State (Pos);
                  end case;
               end loop;
            end;
         elsif Name (N) = "qtype" then
            declare
               Q_Type : String := Value (First_Child (N));
            begin
               for Pos in Q_Type'Range loop
                  case Q_Type (Pos) is
                     when 'B' => Q.Q_Type (B) := True;
                     when 'I' => Q.Q_Type (I) := True;
                     when 'P' => Q.Q_Type (P) := True;
                     when others => raise Constraint_Error
                        with "Queue Type has an unknown character: " & Q_Type (Pos);
                  end case;
               end loop;
            end;
         elsif Name (N) = "resource" then
            A := Get_Attr (N, "name");
            if Value (A) = "mem_total" then
               Set_Memory (Q.Properties, Value (First_Child (N)));
            elsif Value (A) = "num_proc" then
               Set_Cores (Q.Properties, Integer'Value (Value (First_Child (N))));
            elsif Value (A) = "infiniband" and then
              small'Value (Value (First_Child (N))) = 1.0 and then
              Network = none
            then
               Network := ib;
            elsif Value (A) = "ib-switch" and then
              small'Value (Value (First_Child (N))) = 1.0
            then
               Network := ibswitch;
            elsif Value (A) = "ethernet" and then
              small'Value (Value (First_Child (N))) = 1.0
            then
               Network := eth;
            elsif Value (A) = "h_rt" then
               Set_Runtime (Q.Properties, To_Unbounded_String (Value (First_Child (N))));
            elsif Value (A) = "slots" then
               Set_Slots (Q.Properties, Integer (large'Value (Value (First_Child (N)))));
            elsif Value (A) = "cpu_model" then
               Set_Model (Q.Properties, To_Model (Value (First_Child (N))));
            elsif Value (A) = "qname" then
               Q.Name := To_Unbounded_String (Value (First_Child (N)));
            elsif Value (A) = "ssd"  then
               Set_SSD (Q.Properties); -- consumable, so do not check numerical value
            elsif Value (A) = "gpu_model"  then
               Set_GPU (Q.Properties, Value (First_Child (N)));
            elsif Value (A) = "gpu" then
               Set_GPU (Q.Properties);
            elsif Value (A) = "exclusive" then
               Set_Exclusive (Q.Properties);
            elsif Value (A) = "seq_no" then
               Q.Sequence := Integer (large'Value (Value (First_Child (N))));
            elsif Value (A) = "pe_name" then
               Set_PE (Q.Properties, To_Unbounded_String (Value (First_Child (N))));
            end if;
         elsif Name (N) = "name" then
            Set_Host_Name (Q, Value (First_Child (N)));
         end if;
      end loop;



      Set_Network (Q.Properties, Network);
      if Get_Cores (Q.Properties) = 0 then
         Set_Cores (Q.Properties, Q.Total);
      end if;

      return Q;
   end New_Queue;

   ---------------------------
   -- Precedes_By_Resources --
   ---------------------------

   function Precedes_By_Resources (Left, Right : Queue'Class) return Boolean is
   begin
      return Left.Properties < Right.Properties;
   end Precedes_By_Resources;

   function Precedes_By_Sequence (Left, Right : Queue'Class) return Boolean is
   begin
      return Left.Sequence < Right.Sequence;
   end Precedes_By_Sequence;


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
      return (Has_Disabled (Q) or else Has_Calendar_Disabled (Q))
        and then not Has_Unreachable (Q);
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

   function Get_Host_Name (Q : Queue) return Host_Name is
   begin
      return Q.Host;
   end Get_Host_Name;

   procedure Set_Host_Name (Q : in out Queue; Long_Name : String) is
   begin
      Decompose_Long_Name (Long_Name => Long_Name,
                           Queue     => Q.Name,
                           Host      => Q.Host);
   end Set_Host_Name;

   function Has_Error (Q : Queue) return Boolean is
   begin
      return Q.State (error);
   end Has_Error;

   function Has_Disabled (Q : Queue) return Boolean is
   begin
      return Q.State (disabled);
   end Has_Disabled;

   function Has_Calendar_Disabled (Q : Queue) return Boolean is
   begin
      return Q.State (calendar_disabled);
   end Has_Calendar_Disabled;

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

   procedure Decompose_Long_Name (Long_Name : String; Queue : out Unbounded_String; Host : out Host_Name) is
      Start : Positive := Index (Source  => Long_Name,
                                 Pattern => "@");
      Stop  : Positive := Index (Source  => Long_Name,
                                    From => Start,
                                 Pattern => ".");
   begin
      Host := To_Host_Name (Long_Name (Start + 1 .. Stop - 1));
      Queue := To_Unbounded_String (Long_Name (Long_Name'First .. Start - 1));
   end Decompose_Long_Name;

   function Has_Element (Position : Cursor) return Boolean is
   begin
      return Has_Element (Position);
   end Has_Element;

   function Iterate (Container : List) return
     List_Iterator_Interfaces.Reversible_Iterator'Class is
   begin
      return Container.Data.Iterate;
   end Iterate;

   procedure Sort (What : in out List) is
   begin
      Sorting_By_Resources.Sort (What.Data);
   end Sort;

   procedure Sort_By_Sequence (What : in out List) is
   begin
      Sorting_By_Sequence.Sort (What.Data);
   end Sort_By_Sequence;

end SGE.Queues;
