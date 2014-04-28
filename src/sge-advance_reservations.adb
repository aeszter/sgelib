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
      C : Node;
      A : Attr;
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
                    (Integer'Value (Value (First_Child (C)))));
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
      return R.State_String;
   end Get_State;

   function Get_Start (R : Reservation) return Ada.Calendar.Time is
   begin
      return R.Start_Time;
   end Get_Start;

   function Get_Duration (R : Reservation) return Duration is
   begin
      return R.Time_Span;
   end Get_Duration;

   function Has_Error_Log_Entries (R : Reservation) return Boolean is
   begin
      return not R.Error_Log.Is_Empty;
   end Has_Error_Log_Entries;

   procedure Iterate_Error_Log (R       : Reservation;
                                Process : not null access procedure (Message : String)) is
      procedure Wrapper (Position : Utils.String_Lists.Cursor) is
      begin
         Process (To_String (Element (Position)));
      end Wrapper;

   begin
      R.Error_Log.Iterate (Wrapper'Access);
   end Iterate_Error_Log;

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

   procedure Record_Error (R : in out Reservation; Message : String) is
   begin
      R.Error_Log.Append (To_Unbounded_String (Message));
   end Record_Error;

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

end SGE.Advance_Reservations;
