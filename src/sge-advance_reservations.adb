with SGE.Parser; use SGE.Parser;
with Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Real_Time;
with SGE.Utils; use SGE.Utils.String_Lists;

package body SGE.Advance_Reservations is
   use AR_Lists;
   -----------------
   -- Append_List --
   -----------------
   procedure Update_State_Array (R : in out Reservation);
   procedure Extract_Resource_List (R              : in out Reservation;
                                    Resource_Nodes : Node_List);
   procedure Extract_Slots_List (R : in out Reservation; Children : Node_List);

   procedure Extract_Resource_List (R              : in out Reservation;
                                    Resource_Nodes : Node_List) is
      Entry_Node : Node;
      A          : Attr;
      Res_Value : Unbounded_String;
   begin
      for I in 0 .. Length (Resource_Nodes) - 1 loop
         Entry_Node := Item (Resource_Nodes, I);
         if Name (Entry_Node) = "resource" then
            A := Get_Attr (Entry_Node, "type");
            Res_Value := To_Unbounded_String (Value (A));
            A := Get_Attr (Entry_Node, "name");
            R.Add_Resource (Key => Value (A), Value => To_String (Res_Value));
         end if;
      end loop;
   end Extract_Resource_List;

   procedure Append_List (Node_Tree : SGE.Parser.Tree) is
      Nodes : Parser.Node_List := Parser.Get_Elements_By_Tag_Name (Doc => Node_Tree,
                                                                   Tag_Name   => "ar_summary");
      AR_Node : Parser.Node;
   begin
      for Index in 1 .. Length (Nodes) loop
         AR_Node := Item (Nodes, Index - 1);
         if Name (AR_Node) /= "#text" then
            List.Append (New_Reservation (Child_Nodes (AR_Node)));
         end if;
      end loop;
   end Append_List;

   function New_Reservation (List : SGE.Parser.Node_List) return Reservation is
      function HMS_To_Seconds (S : String) return Natural is
         Separator, Start : Positive;
         Seconds : Positive;
      begin
         Separator := Index (Source => S,
                             Pattern => ":");
         Seconds := 3_600 * Integer'Value (S (S'First .. Separator - 1));
         Start := Separator + 1;
         Separator := Index (Source => S (Start .. S'Last),
                             Pattern => ":");
         Seconds := Seconds + 60 * Integer'Value (S (Start .. Separator - 1));
         Seconds := Seconds + Integer'Value (S (Separator + 1 .. S'Last));
         return Seconds;
      end HMS_To_Seconds;

      C : Node;
      R : Reservation;
   begin
      for Index in 0 .. Length (List) - 1 loop
         begin
            C := Item (List, Index);
            if Name (C) = "id" then
               R.Number := Integer'Value (Value (First_Child (C)));
            elsif Name (C) = "name" then
               if Has_Child_Nodes (C) then
                  R.Name := To_Unbounded_String (Value (First_Child (C)));
               end if;
            elsif Name (C) = "owner" then
               R.Owner := To_User_Name (Value (First_Child (C)));
            elsif Name (C) = "state" then
               R.State_String := Head (Value (First_Child (C)), R.State_String'Length);
               Update_State_Array (R);
            elsif Name (C) = "start_time" then
               R.Start_Time := To_Time (Value (First_Child (C)));
            elsif Name (C) = "end_time" then
               R.End_Time := To_Time (Value (First_Child (C)));
            elsif Name (C) = "duration" then
               R.Time_Span := Ada.Real_Time.To_Duration
                 (Ada.Real_Time.Seconds
                    (HMS_To_Seconds (Value (First_Child (C)))));
            elsif Name (C) = "submission_time" then
               R.Submission_Time := To_Time (Value (First_Child (C)));
            elsif Name (C) = "group" then
               R.Group := To_Unbounded_String (Value (First_Child (C)));
            elsif Name (C) = "account" then
               R.Account := To_Unbounded_String (Value (First_Child (C)));
            elsif Name (C) = "granted_parallel_environment" then
               null;  -- Bug #2017
            elsif Name (C) = "granted_slots_list" then
               Extract_Slots_List (R, Child_Nodes (C));
            elsif Name (C) = "resource_list" then
               Extract_Resource_List (R, Child_Nodes (C));
            elsif Name (C) = "mail_options" then
               null; -- ignore
            elsif Name (C) = "message" then
               R.Message_List.Append (To_Unbounded_String (Value (First_Child (C))));
            elsif Name (C) /= "#text" then
               Ada.Text_IO.Put_Line ("Unknown Field: " & Name (C));
            end if;
         exception
               when E : Parser_Error =>
                  Record_Error (R, "information incomplete: " & Exception_Message (E));
               when E : others =>
                  Record_Error (R, "While parsing reservaton: " & Exception_Message (E)
                                    & "Node type: """ & Name (C)
                             & """ Value: """ & Value (First_Child (C)) & """");
         end;
      end loop;
      return R;
   end New_Reservation;

   function Get_Name (R : Reservation) return String is
   begin
      return To_String (R.Name);
   end Get_Name;

   function Get_ID (R : Reservation) return String is
   begin
      return R.Number'Img;
   end Get_ID;

   function Get_Owner (R : Reservation) return User_Name is
   begin
      return R.Owner;
   end Get_Owner;

   function Get_State (R : Reservation) return String is
   begin
      return Ada.Strings.Fixed.Trim (R.State_String, Ada.Strings.Right);
   end Get_State;

   function Get_Duration (R : Reservation) return Duration is
   begin
      return R.Time_Span;
   end Get_Duration;

   procedure Iterate_Messages (R : Reservation;
                               Process : not null access procedure (Message : String))
   is
      procedure Wrapper (Position : Utils.String_Lists.Cursor) is
      begin
         Process (To_String (Element (Position)));
      end Wrapper;

   begin
      R.Message_List.Iterate (Wrapper'Access);
   end Iterate_Messages;

   procedure Iterate_Queues (R       : Reservation;
                             Process : not null access procedure (Q : Queue)) is
      procedure Wrapper (Position : Queue_Lists.Cursor) is
      begin
         Process (Queue_Lists.Element (Position));
      end Wrapper;

   begin
      R.Queues.Iterate (Wrapper'Access);
   end Iterate_Queues;

   procedure Iterate (Process : not null access procedure (R : Reservation)) is
      procedure Wrapper (Position : AR_Lists.Cursor) is
      begin
         Process (Element (Position));
      end Wrapper;

   begin
      List.Iterate (Wrapper'Access);
   end Iterate;

   function Same (Left, Right : Reservation) return Boolean is
   begin
      return Left.Number = Right.Number;
   end Same;

   procedure Update_State_Array (R : in out Reservation) is
      Flag : State_Flag;
      Skip : Boolean;
   begin
      for Flag in R.State_Array'Range loop
         R.State_Array (Flag) := False;
      end loop;

      for Position in R.State_String'Range loop
         Skip := False;
         case R.State_String (Position) is
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
               raise Advance_Reservations.Other_Error with "Unknown state character " &
               R.State_String (Position) & " found";
         end case;
         if not Skip then
            R.State_Array (Flag) := True;
         end if;
      end loop;
   end Update_State_Array;

   procedure Extract_Slots_List (R : in out Reservation; Children : Node_List) is
      Slot_Node : Node;
      A         : Attr;
      Slots : Positive;
   begin
      for I in 0 .. Length (Children) - 1 loop
         Slot_Node := Item (Children, I);
         if Name (Slot_Node) = "granted_slots" then
            A := Get_Attr (Slot_Node, "slots");
            Slots := Integer'Value (Value (A));
            A := Get_Attr (Slot_Node, "queue_instance");
            R.Append_Queue (Q => Value (A), Slots => Slots);
         end if;
      end loop;
   end Extract_Slots_List;

   procedure Append_Queue (R : in out Reservation; Q : String; Slots : Positive) is
      New_Item : Queue := (Name => To_Unbounded_String (Q), Slots => Slots);
   begin
      R.Queues.Include (Key      => To_Key (Q),
                        New_Item => New_Item);
   end Append_Queue;

   procedure Add_Resource (R : in out Reservation; Key, Value : String) is
   begin
      R.Resource_List.Insert (Key      => To_Unbounded_String (Key),
                              New_Item => Resources.New_Resource (Name  => Key,
                                                                  Value => Value));
   end Add_Resource;

   function To_Key (S : String) return Queue_Key is
   begin
      return Queue_Key (Queue_Keys.To_Bounded_String (Source => S,
                                                      Drop   => Ada.Strings.Right));
   end To_Key;

   function Get_Group (R : Reservation) return String is
   begin
      return To_String (R.Group);
   end Get_Group;

   function Get_Account (R : Reservation) return String is
   begin
      return To_String (R.Account);
   end Get_Account;

   function Get_Start_Time (R : Reservation) return Ada.Calendar.Time is
   begin
      return R.Start_Time;
   end Get_Start_Time;

   function Get_End_Time (R : Reservation) return Ada.Calendar.Time is
   begin
      return R.End_Time;
   end Get_End_Time;

   function Get_Submission_Time (R : Reservation) return Ada.Calendar.Time is
   begin
      return R.Submission_Time;
   end Get_Submission_Time;

   function Get_Resources (R : Reservation) return Resources.Hashed_List is
   begin
      return R.Resource_List;
   end Get_Resources;

   function Get_Name (Q : Queue) return String is
   begin
      return To_String (Q.Name);
   end Get_Name;

   function Get_Slots (Q : Queue) return String is
   begin
      return Q.Slots'Img;
   end Get_Slots;


end SGE.Advance_Reservations;
