with Ada.Containers.Doubly_Linked_Lists;
with SGE.Queues;
with Ada.Exceptions; use Ada.Exceptions;

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
      Previous : Cursor := Find (Container => Container,
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
   --  Parameter Q_List: List of queues to work on.
   --  Parameter Part_List: The new partition list.
   --  Side Effect: Q_List is sorted by resources.
   -------------------

   procedure Build_List is
      P : Partition;
      Q : Queue;
   begin
      Queues.Sort;
      Queues.Rewind;

      Q := Queues.Current;
      --  Create Partition according to first Queue
      P := New_Partition (Q);
      loop
         --  New Partition?
         if P /= Q then
            --  Yes. Store previous one.
            List.Append (P);
            P := New_Partition (Q);
         end if;

         begin
            --  Update totals
            P.Total_Slots.Include (Key      => Host_Names.To_Bounded_String (Get_Host_Name (Q)),
                                   New_Item => Get_Slot_Count (Q));
            P.Total_Hosts.Include (Host_Names.To_Bounded_String (Get_Host_Name (Q)));
            List.Summary (total).Include (Key      => Host_Names.To_Bounded_String (Get_Host_Name (Q)),
                                   New_Item => Get_Slot_Count (Q));
            if Is_Offline (Q) then
               P.Offline_Slots.Include (Key => Host_Names.To_Bounded_String (Get_Host_Name (Q)),
                                   New_Item => Get_Slot_Count (Q));
               P.Offline_Hosts.Include (Host_Names.To_Bounded_String (Get_Host_Name (Q)));
               List.Summary (offline).Include (Key      => Host_Names.To_Bounded_String (Get_Host_Name (Q)),
                                               New_Item => Get_Slot_Count (Q));
            elsif Is_Disabled (Q) then
               P.Disabled_Slots.Include (Key      => Host_Names.To_Bounded_String (Get_Host_Name (Q)),
                                               New_Item => Get_Slot_Count (Q));
               P.Disabled_Hosts.Include (Host_Names.To_Bounded_String (Get_Host_Name (Q)));
               List.Summary (disabled).Include (Key      => Host_Names.To_Bounded_String (Get_Host_Name (Q)),
                                                New_Item => Get_Slot_Count (Q));
            elsif Is_Suspended (Q) then
               P.Suspended_Slots := P.Suspended_Slots + Get_Slot_Count (Q);
            else
               if Get_Used_Slots (Q) > 0 then
                  P.Used_Hosts.Include (Host_Names.To_Bounded_String (Get_Host_Name (Q)));
                  P.Used_Slots := P.Used_Slots + Get_Used_Slots (Q);
                  List.Summary (used).Include (Key      => Host_Names.To_Bounded_String (Get_Host_Name (Q)),
                                               New_Item => Get_Used_Slots (Q));
               end if;
               if Get_Reserved_Slots (Q) > 0 then
                  P.Reserved_Hosts.Include (Host_Names.To_Bounded_String (Get_Host_Name (Q)));
                  P.Reserved_Slots := P.Reserved_Slots + Get_Reserved_Slots (Q);
                  List.Summary (reserved).Include (Key      => Host_Names.To_Bounded_String (Get_Host_Name (Q)),
                                                   New_Item => Get_Reserved_Slots (Q));
               end if;
               P.Available_Slots.Include (Key      => Host_Names.To_Bounded_String (Get_Host_Name (Q)),
                                               New_Item => Get_Free_Slots (Q));
               if Get_Reserved_Slots (Q) = 0 and then Get_Used_Slots (Q) = 0 then
                  P.Available_Hosts.Include (Host_Names.To_Bounded_String (Get_Host_Name (Q)));
               end if;
               List.Summary (available).Include (Key      => Host_Names.To_Bounded_String (Get_Host_Name (Q)),
                                                 New_Item => Get_Free_Slots (Q));
            end if;
         exception
            when E : Constraint_Error =>
               Record_Error (P, "Warning" & Exception_Message (E));
         end;
         exit when Queues.At_End;
         --  Advance
         Q := Queues.Next;
      end loop;
      --  That's it. Store final partition.
      List.Append (P);
   end Build_List;

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

   procedure Record_Error (P : in out Partition; Message : String) is
   begin
      P.Error_Log.Append (To_Unbounded_String (Message));
   end Record_Error;

end SGE.Partitions;
