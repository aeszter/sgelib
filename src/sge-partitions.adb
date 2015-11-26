with Ada.Containers.Doubly_Linked_Lists;
with Ada.Exceptions; use Ada.Exceptions;
with SGE.Queues;
with SGE.Resources; use SGE.Resources;

package body SGE.Partitions is

   use Queues;
   ---------
   -- "=" --
   ---------

   function "=" (Left : Partition; Right : Queue) return Boolean is
   begin
      return Left.Properties = Get_Properties (Right);
   end "=";

   function "=" (Left : Queue; Right : Partition) return Boolean is
   begin
      return Right = Left;
   end "=";

   overriding function Copy (Source : Summarized_List) return Summarized_List is
   begin
      return (Partition_Lists.Copy (Partition_Lists.List (Source)) with
                Summary => Source.Summary);
   end Copy;

   function Sum (Over : Countable_Map) return Natural is
      Total : Natural := 0;

      procedure Count (Position : Countable_Maps.Cursor) is
      begin
         Total := Total + Countable_Maps.Element (Position);
      end Count;

   begin
      Over.Iterate (Count'Access);
      return Total;
   end Sum;

   overriding procedure Include (Container : in out Countable_Map;
                                 Key       : Host_Name;
                                 New_Item  : Natural) is
      use Countable_Maps;
      Previous : Countable_Maps.Cursor := Find (Container => Container,
                                                Key       => Key);
      procedure Take_Maximum (Key : Host_Name; Element : in out Natural) is
         pragma Unreferenced (Key);
      begin
         if New_Item > Element then
            Element := New_Item;
         end if;
      end Take_Maximum;

   begin
      if Previous = No_Element then
         Insert (Container => Container,
                 Key       => Key,
                 New_Item  => New_Item);
      else
         Update_Element (Container => Container,
                         Position  => Previous,
                         Process   => Take_Maximum'Access);
      end if;
   end Include;

   -------------------
   -- New_Partition --
   --  Purpose: Build a partition list from a queue list.
   --           This totals all slots (available, used, reserved, ...) for the
   --           for all matching queues.
   --  Side Effect: Q_List is sorted by resources.
   -------------------

   procedure Initialize (Queue_List : Queues.List;
                         Partition_List : out Summarized_List) is
      P : Partition;
      Q : Queue;
      Position : Queues.Cursor;
   begin
      Partition_List.Clear;
      Position :=  First (Queue_List);
      --  Create Partition according to first Queue
      P := New_Partition (Element (Position));
      Next (Position);
      while Has_Element (Position) loop
         Q := Element (Position);
         --  New Partition?
         if P /= Q then
            --  Yes. Store previous one.
            Partition_List.Append (P);
            P := New_Partition (Q);
         end if;

         begin
            --  Update totals
            P.Total_Slots.Include (Key      => Get_Host_Name (Q),
                                   New_Item => Get_Slot_Count (Q));
            P.Total_Hosts.Include (Get_Host_Name (Q));
            Partition_List.Summary (total).Include (Key      => Get_Host_Name (Q),
                                                    New_Item => Get_Slot_Count (Q));
            if Is_Offline (Q) then
               P.Offline_Slots.Include (Key => Get_Host_Name (Q),
                                   New_Item => Get_Slot_Count (Q));
               P.Offline_Hosts.Include (Get_Host_Name (Q));
               Partition_List.Summary (offline).Include (Key      => Get_Host_Name (Q),
                                                         New_Item => Get_Slot_Count (Q));
            elsif Is_Disabled (Q) then
               P.Disabled_Slots.Include (Key      => Get_Host_Name (Q),
                                               New_Item => Get_Slot_Count (Q));
               P.Disabled_Hosts.Include (Get_Host_Name (Q));
               Partition_List.Summary (disabled).Include (Key      => Get_Host_Name (Q),
                                                New_Item => Get_Slot_Count (Q));
            elsif Is_Suspended (Q) then
               P.Suspended_Slots := P.Suspended_Slots + Get_Slot_Count (Q);
            else
               if Get_Used_Slots (Q) > 0 then
                  P.Used_Hosts.Include (Get_Host_Name (Q));
                  P.Used_Slots := P.Used_Slots + Get_Used_Slots (Q);
                  Partition_List.Summary (used).Include (Key      => Get_Host_Name (Q),
                                               New_Item => Get_Used_Slots (Q));
               end if;
               if Get_Reserved_Slots (Q) > 0 then
                  P.Reserved_Hosts.Include (Get_Host_Name (Q));
                  P.Reserved_Slots := P.Reserved_Slots + Get_Reserved_Slots (Q);
                  Partition_List.Summary (reserved).Include (Key      => Get_Host_Name (Q),
                                                   New_Item => Get_Reserved_Slots (Q));
               end if;
               P.Available_Slots.Include (Key      => Get_Host_Name (Q),
                                               New_Item => Get_Free_Slots (Q));
               if Get_Reserved_Slots (Q) = 0 and then
                 Get_Used_Slots (Q) = 0 and then
                 Get_Free_Slots (Q) = Get_Slot_Count (Q)
               then
                  P.Available_Hosts.Include (Get_Host_Name (Q));
               end if;
               Partition_List.Summary (available).Include (Key      => Get_Host_Name (Q),
                                                 New_Item => Get_Free_Slots (Q));
            end if;
         exception
            when E : Constraint_Error =>
               Record_Error (P, "Warning" & Exception_Message (E));
         end;
         --  Advance
         Next (Position);
      end loop;
      --  That's it. Store final partition.
      Partition_List.Append (P);
   end Initialize;

   -------------------
   -- New_Partition --
   -------------------

   function New_Partition (Q : Queue) return Partition is
      P : Partition;
   begin
      P.Properties := Get_Properties (Q);
      P.Name      := Get_Name (Q);
      return P;
   end New_Partition;


   function To_String (Source : State) return String is
   begin
      case Source is
         when total =>
            return "Total";
         when reserved =>
            return "Reserved";
         when used =>
            return "Used";
         when offline =>
            return "Offline";
         when available =>
            return "Available";
         when disabled =>
            return "Disabled";
      end case;
   end To_String;

   procedure Iterate (Collection : Summarized_List; Process : not null access procedure (P : Partition)) is
      procedure Wrapper (Position : Partition_Lists.Cursor) is
      begin
         Process (Partition_Lists.Element (Position));
      end Wrapper;

   begin
      Iterate (Collection, Wrapper'Access);
   end Iterate;

   function Get_Available_Hosts (P : Partition) return Natural is
   begin
      return Natural (Countable_Sets.Length (P.Available_Hosts));
   end Get_Available_Hosts;

   function Get_Available_Slots (P : Partition) return Natural is
   begin
      return Sum (P.Available_Slots);
   end Get_Available_Slots;

   function Get_Offline_Slots (P : Partition) return Natural is
   begin
      return Sum (P.Offline_Slots);
   end Get_Offline_Slots;

   function Get_Offline_Hosts (P : Partition) return Natural is
   begin
      return Natural (P.Offline_Hosts.Length);
   end Get_Offline_Hosts;

   function Get_Suspended_Slots (P : Partition) return Natural is
   begin
      return P.Suspended_Slots;
   end Get_Suspended_Slots;

   function Get_Total_Slots (P : Partition) return Natural is
   begin
      return Sum (P.Total_Slots);
   end Get_Total_Slots;

   function Get_Total_Hosts (P : Partition) return Natural is
   begin
      return Natural (P.Total_Hosts.Length);
   end Get_Total_Hosts;

   function Get_Used_Slots (P : Partition) return Natural is
   begin
      return P.Used_Slots;
   end Get_Used_Slots;

   function Get_Used_Hosts (P : Partition) return Natural is
   begin
      return Natural (P.Used_Hosts.Length);
   end Get_Used_Hosts;

   function Get_Reserved_Slots (P : Partition) return Natural is
   begin
      return P.Reserved_Slots;
   end Get_Reserved_Slots;

   function Get_Reserved_Hosts (P : Partition) return Natural is
   begin
      return Natural (P.Reserved_Hosts.Length);
   end Get_Reserved_Hosts;

   function Get_Disabled_Slots (P : Partition) return Natural is
   begin
      return Sum (P.Disabled_Slots);
   end Get_Disabled_Slots;

   function Get_Disabled_Hosts (P : Partition) return Natural is
   begin
      return Natural (P.Disabled_Hosts.Length);
   end Get_Disabled_Hosts;

   function Get_Network (P : Partition) return String is
   begin
      return Get_Network (P.Properties)'Img;
   end Get_Network;

   function Get_Model (P : Partition) return String is
   begin
      return To_String (Get_Model (P.Properties));
   end Get_Model;

   function Get_GPU (P : Partition) return String is
   begin
      return Get_GPU (P.Properties)'Img;
   end Get_GPU;

   function Get_Memory (P : Partition) return String is
   begin
      return SGE.Resources.To_String (Get_Memory (P.Properties));
   end Get_Memory;

   function Has_GPU (P : Partition) return Boolean is
   begin
      return Has_GPU (P.Properties);
   end Has_GPU;

   function Has_SSD (P : Partition) return Boolean is
   begin
      return Has_SSD (P.Properties);
   end Has_SSD;

   function Get_Name (P : Partition) return String is
   begin
      return To_String (P.Name);
   end Get_Name;

   function Get_Runtime (P : Partition) return String is
   begin
      return Get_Runtime (P.Properties);
   end Get_Runtime;

   function Get_Cores (P : Partition) return Natural is
   begin
      return Get_Cores (P.Properties);
   end Get_Cores;

   procedure Iterate_Summary (Process : not null access procedure (Item : State)) is
   begin
      null;
   end Iterate_Summary;

   function Get_Summary (List : Summarized_List; From : State) return Natural is
   begin
      return Sum (List.Summary (From));
   end Get_Summary;

   procedure Iterate_Available_Slots (P       : Partition;
                                      Process : not null access procedure (Position : Countable_Maps.Cursor)) is
   begin
      P.Available_Slots.Iterate (Process);
   end Iterate_Available_Slots;

   function Get_Properties (P : Partition) return Set_Of_Properties is
   begin
      return P.Properties;
   end Get_Properties;

end SGE.Partitions;
