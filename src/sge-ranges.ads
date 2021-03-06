with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers; use Ada.Containers;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with SGE.Utils;

package SGE.Ranges is

   type Step_Range is record
      Min, Step, Max : Natural := 0;
   end record;

   function New_Range (Min, Step, Max : Natural)
                       return Step_Range;
   function To_Step_Range (From : String) return Step_Range;
   --  Create a Step_Range from a string of the form a-b:s, or from a string
   --  representing a number

   function Is_Empty (What : Step_Range) return Boolean;
   --  An empty range does not represent any number.

   function Count (What : Step_Range) return Natural;
   --  how many different entries does the range represent?
   function Is_Collapsed (What : Step_Range) return Boolean;
   --  return True if the range collapses to a single number

   package Range_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Element_Type => Step_Range);

   type Step_Range_List is new Range_Lists.List with private;

   Empty_Range : constant Step_Range_List;

   overriding procedure Append (Container : in out Step_Range_List;
                                New_Item  : Step_Range;
                                Count     : Count_Type := 1);
   procedure Iterate (Container : Step_Range_List;
                      Process   : not null access procedure (Number : Natural));


   function Precedes (Left, Right : Step_Range_List) return Boolean;

   function To_Step_Range_List (From : String) return Step_Range_List;
   --  Create a Step_Range from a string of the form {SR}(,{SR})* where {SR} is
   --  a step range (see above)

   function Count (What : Step_Range_List) return Natural;
   --  How many different entries does the list represent?
   --  This does not mean the number of Step_Ranges in the list, but rather the
   --  sum of Count(SR) for each entry SR.

   overriding function Is_Empty (What : Step_Range_List) return Boolean;
   --  An empty range list does not represent any number. It contains at most
   --  empty ranges.

   function Is_Collapsed (What : Step_Range_List) return Boolean;
   --  return True if the list collapses to a single number: it contains
   --  only a single entry, and that entry is collapsed.
   --  Degenerate cases are not handled correctly: multiple identical entries,
   --  or multiple entries, all but one being empty, are not recognized as collapsed.

   function Is_Subset (Subset, Of_Set : Step_Range_List) return Boolean;
   function Intersects (Left, Right : Step_Range_List) return Boolean;
   function Is_Contained (Element : Natural; Contained_In : Step_Range_List) return Boolean;

   ---------------
   -- To_String --
   --  Purpose: Format the contents of a slot range (Slots) as a String
   --  Parameter What: The slot range to format
   --  Parameter Short: Whether to collapse ranges "n - n" to "n"
   --  Returns: The formatted String
   ---------------

   function To_String (What : Step_Range; Short : Boolean) return String;
   function To_String (What : Step_Range_List; Short : Boolean) return String;
   function To_SGE_Input (What : Step_Range) return String;
   function To_SGE_Input (What : Step_Range_List) return String;

   ---------------
   -- To_Unbounded_String --
   --  Purpose: Format the contents of a slot range (Slots) as a String
   --  Parameter What: The slot range to format
   --  Parameter Short: Whether to collapse ranges "n - n" to "n"
   --  Returns: The formatted String
   ---------------
   function To_Unbounded_String (What : Step_Range; Short : Boolean) return Unbounded_String;

   function Hash (List : Step_Range_List) return String;
   function Hash (S : Step_Range) return Hash_Type;

   --  return the "Minimum" number represented by the SRL. For now, the list is
   --  assumed to be ordered, so the Min of the first SR in the list is returned.
   function Min (List : Step_Range_List) return Natural;

   --  return the "Maximum" number represented by the SRL. For now, the list is
   --  assumed to be ordered, so the Max of the last SR in the list is returned.
   function Max (List : Step_Range_List) return Natural;

   overriding function Copy (Source : Step_Range_List) return Step_Range_List;

private
   type Step_Range_List is new Range_Lists.List with
      record
         Hash_Value : Hash_Type := 0;
         Hash_String : Utils.Hash_String_Type;
      end record;

   --  Purpose: unconditionally compute the List's hash value
   procedure Rehash (List : in out Step_Range_List);
   procedure Condense (List : in out Step_Range_List);

   Empty_Range : constant Step_Range_List := (Range_Lists.Empty_List
                                              with Hash_Value => 0,
                                              Hash_String     => Utils.Hash_Strings.Null_Bounded_String);

end SGE.Ranges;
