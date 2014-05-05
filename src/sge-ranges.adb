with SGE.Ranges; use SGE.Ranges.Range_Lists;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Hash;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with SGE.Utils; use SGE.Utils; use SGE.Utils.Hash_Strings;
with Ada.Strings.Maps; use Ada.Strings.Maps;
with Ada.Text_IO;

package body SGE.Ranges is

   ---------------
   -- New_Range --
   --  Purpose: Create a new slot range with given values
   --  Parameter Min: lower bound of slots
   --  Parameter Max: upper bound of slots
   --  Parameter Step: stride length of slots
   --  Returns: the newly created slot range
   ---------------

   function New_Range
     (Min, Step, Max : Natural)
      return Step_Range
   is
      R : Step_Range;
   begin
      R.Min := Min;
      R.Max := Max;
      R.Step := Step;
      return R;
   end New_Range;

   -------------------
   -- To_Step_Range --
   -------------------

   function To_Step_Range (From : String) return Step_Range is
      R : Step_Range;
      Dash, Colon : Natural;
   begin
      Dash := Index (Source  => From,
                     Pattern => "-");
      Colon := Index (Source  => From,
                      Pattern => ":");
      if Dash = 0 and then Colon = 0 then
         R.Min := Natural'Value (From);
         R.Max := R.Min;
         R.Step := 1;
      else
         if Dash = 0 then
            raise Constraint_Error;
         elsif
           Colon = 0 then
            raise Constraint_Error;
         elsif Dash <= From'First then
            raise Constraint_Error;
         elsif Colon <= Dash + 1 then
            raise Constraint_Error;
         elsif From'Last <= Colon then
            raise Constraint_Error;
         end if;
         --  Sanity checks: everything but a pure number or a pattern of the
         --  form [0-9]+-[0-9]+:[0-9]+ is illegal
         R.Min := Natural'Value (From (From'First .. Dash - 1));
         R.Max := Natural'Value (From (Dash + 1 .. Colon - 1));
         R.Step := Natural'Value (From (Colon + 1 .. From'Last));
      end if;
      return R;
   end To_Step_Range;

   ------------------------
   -- To_Step_Range_List --
   ------------------------

   function To_Step_Range_List (From : String) return Step_Range_List is
      List : Step_Range_List;
      Comma, Prev : Natural := From'First - 1;
   begin
      loop
         Comma := Index (Source  => From (Prev + 1 .. From'Last),
                         Pattern => ",",
                         Mapping => To_Mapping (From => ";",
                                                To   => ","));
         if Comma = 0 then
            List.Append (New_Item => To_Step_Range (From (Prev + 1 .. From'Last)));
            Condense (List);
            return List;
         else
            List.Append (New_Item => To_Step_Range (From (Prev + 1 .. Comma - 1)));
         end if;
         Prev := Comma;
      end loop;
   end To_Step_Range_List;


   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (What : Step_Range) return Boolean is
   begin
      if What.Max < What.Min then
         raise Constraint_Error with What.Min'Img & "-" & What.Max'Img
           & ":" & What.Step'Img;
      end if;
      if What.Step = 0 then
         return True;
      else
         return False;
      end if;

   end Is_Empty;

   overriding function Is_Empty (What : Step_Range_List) return Boolean is
      Cursor : Range_Lists.Cursor := What.First;
   begin
      if Range_Lists.Is_Empty (List (What)) then
         return True;
      end if;
      while Cursor /= No_Element loop
         if not Is_Empty (Element (Cursor)) then
            return False;
         end if;
         Next (Cursor);
      end loop;
      return True;
   end Is_Empty;



   -----------
   -- Count --
   -----------

   function Count (What : Step_Range) return Natural is
   begin
      if Is_Empty (What) then
         return 1;
      else
         return (What.Max - What.Min) / What.Step + 1;
      end if;
   end Count;

   function Count (What : Step_Range_List) return Natural is
      N : Natural := 0;
      Cursor : Range_Lists.Cursor := What.First;
   begin
      while Cursor /= Range_Lists.No_Element loop
         N := N + Count (Element (Cursor));
         Next (Cursor);
      end loop;
      if N = 0 then
         return 1;
      else
         return N;
      end if;
   end Count;

      ------------------
   -- Is_Collapsed --
   ------------------

   function Is_Collapsed (What : Step_Range) return Boolean is
   begin
      if Is_Empty (What) then
         return False;
      elsif What.Min = What.Max then
         return True;
      else
         return False;
      end if;
   end Is_Collapsed;

   function Is_Collapsed (What : Step_Range_List) return Boolean is
   begin
      if What.Length = 1
        and then Is_Collapsed (What.First_Element) then
         return True;
      else
         return False;
      end if;
   end Is_Collapsed;
   ---------------
   -- To_String --
   ---------------

   function To_String (What : Step_Range; Short : Boolean) return String is
   begin
      if Short and then What.Min = What.Max then
         return What.Min'Img;
      elsif What.Min + What.Step = What.Max then
         return What.Min'Img & "," & What.Max'Img;
      elsif What.Step = 1 then
         return What.Min'Img & " -" & What.Max'Img;
      else
         return What.Min'Img & " -" & What.Max'Img
                & " / " & What.Step'Img;
      end if;
   end To_String;

   function To_Unbounded_String (What : Step_Range; Short : Boolean) return Unbounded_String is
   begin
      return To_Unbounded_String (To_String (What => What, Short => Short));
   end To_Unbounded_String;


   ----------
   -- Hash --
   ----------

   function Hash (List : Step_Range_List) return String is
   begin
      return To_String (List.Hash_String);
   end Hash;

   ----------
   -- Hash --
   ----------

   function Hash (S : Step_Range) return Hash_Type is
      Str : Unbounded_String := To_Unbounded_String (What => S, Short => False);
   begin
      return Hash (Str);
   end Hash;

   ---------
   -- Min --
   ---------

   function Min (List : Step_Range_List) return Natural is
   begin
      if List.Is_Empty then
         raise Program_Error with "Tried to get Min of empty list";
      else
         return List.First_Element.Min;
      end if;
   end Min;

   function Max (List : Step_Range_List) return Natural is
   begin
      if List.Is_Empty then
         raise Program_Error with "Tried to get Max of empty list";
      else
         return List.Last_Element.Max;
      end if;
   end Max;

   procedure Rehash (List : in out Step_Range_List) is
      Temp : Ada.Containers.Hash_Type := 0;
      Pos  : Range_Lists.Cursor := List.First;
      Last : Natural := 0;
   begin
      while Pos /= No_Element loop
         if Element (Pos).Min < Last then
            raise Assumption_Error with "unordered Step_Range_List encountered";
         end if;
         Last := Element (Pos).Max;
         Temp := Temp xor Hash (Element (Pos));
         Next (Pos);
      end loop;
      List.Hash_String := To_Hash_String (Temp'Img);
      List.Hash_Value := Temp;
   end Rehash;

   overriding procedure Append (Container : in out Step_Range_List;
                                New_Item  : Step_Range;
                                Count     : Count_Type := 1) is
   begin
      Range_Lists.Append (Container => List (Container),
                          New_Item  => New_Item,
                          Count     => Count);
      Container.Hash_Value := Container.Hash_Value xor Hash (New_Item);
      Container.Hash_String := To_Hash_String (Container.Hash_Value'Img);
   end Append;


   function To_String (What : Step_Range_List; Short : Boolean) return String is
      Pos : Range_Lists.Cursor := What.First;
      S   : Unbounded_String;
   begin
      while Pos /= Range_Lists.No_Element loop
         S := S & To_String (What => Element (Pos), Short => Short);
         Next (Pos);
         if Pos /= Range_Lists.No_Element then
            S := S & ",";
         end if;
      end loop;
      return To_String (S);
   end To_String;

   --  procedure Condense
   --  Purpose: look for "runs" of condensed step ranges of the form
   --          a, a+d, a+2d, ..., a+nd
   --          and replace them with a non-condensed range
   --          min => a, step => d, max => a+nd
   procedure Condense (List : in out Step_Range_List) is
      First     : Range_Lists.Cursor := List.First;
      --  earliest possible start of a run; always well-defined
      Pos       : Range_Lists.Cursor;
      Item      : Step_Range;
      --  step range under consideration
      Last      : Range_Lists.Cursor;
      --  possible end of a run; No_Element if undefined
      Step      : Natural := 0;
      --  step size of current (possibly incomplete) run;
      --  zero if undetermined
      Target    : Step_Range_List;

      procedure Trace (Message : String);

      procedure Append_New is
         Condensate : Step_Range;
      begin
         Trace ("Appending accumulated list");
         Condensate := New_Range (Min  => Element (First).Min,
                                  Step => Step,
                                  Max  => Element (Last).Max);
         Step := 0;
         Target.Append (Condensate);
      end Append_New;

      procedure Trace (Message : String) is
         use Ada.Text_IO;
      begin
         null;
--         if Has_Element (Pos) then
--            Put_Line (Message & Element (Pos).Min'Img);
--         else
--            Put_Line (Message & "[none]");
--         end if;
      end Trace;

   begin
      Pos := First;
      Last := No_Element;
      while Pos /= Range_Lists.No_Element loop
         Item := Element (Pos);
         if not Is_Collapsed (Item) then
            Trace ("Complex entry");
            if Last /= No_Element then
               Append_New;
               Last := No_Element;
            end if;
            Target.Append (Item);
            Next (Pos);
            First := Pos;
         else
            if Pos = First then
               Trace ("Singleton");
               Next (Pos);
            else
               if Last = No_Element then
                  Step := Element (Pos).Min - Element (First).Max;
                  Trace ("Setting Step to" & Step'Img);
                  if Step = 0 then
                     raise Program_Error
                       with "zero step size while condensing range list";
                  end if;
                  Last := Pos;
                  Next (Pos);
                  if Pos = No_Element then
                     Append_New;
                  end if;
               elsif Step = Element (Pos).Min - Element (Last).Max then
                  Trace ("Step fits");
                  Last := Pos;
                  Next (Pos);
                  if Pos = No_Element then
                     Append_New;
                  end if;
               else
                  Trace ("Default: step does not fit");
                  Append_New;
                  Last := No_Element;
                  First := Pos;
                  Next (Pos);
                  if Pos = No_Element then
                     Target.Append (Element (First));
                  end if;
               end if;
            end if;
         end if;
      end loop;
      List := Target;
   end Condense;

end SGE.Ranges;
