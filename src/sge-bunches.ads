with Ada.Containers.Doubly_Linked_Lists;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with SGE.Jobs; use SGE.Jobs;
with SGE.Resources;
with SGE.Ranges; use SGE.Ranges;

package SGE.Bunches is

   Other_Error : exception;

   type Bunch is private;

   procedure Build_List;

   function New_Bunch (J : Job) return Bunch;

   function "=" (Left : Bunch; Right : Job) return Boolean;
   function "=" (Left : Job; Right : Bunch) return Boolean;
   --  return True if a given Job belongs to a certain Bunch

private
   type Bunch is record
      PE, Queue      : Unbounded_String;
      Slot_Number    : Unbounded_String;
      Slot_List      : Ranges.Step_Range_List;
      Hard, Soft     : Resources.Hashed_List;
      Total, On_Hold : Natural;
      Error, Waiting : Natural;
      Balancer       : Boolean;
   end record;

   package Bunch_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Element_Type => Bunch);

   List : Bunch_Lists.List;
end SGE.Bunches;
