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

   procedure Iterate (Process : access procedure (B : Bunch));

   function Has_Error (B : Bunch) return Boolean;
   function Has_Waiting (B : Bunch) return Boolean;
   function Get_PE (B : Bunch) return String;
   function Get_Slot_Number (B : Bunch) return String;
   function Get_Slot_Numbers (B : Bunch) return String;
   function Get_CPU_Slot_Numbers (B : Bunch) return String;
   function Get_GPU_Slot_Numbers (B : Bunch) return String;
   function Get_Queue (B : Bunch) return String;
   function Get_Total_Jobs (B : Bunch) return Natural;
   function Get_Waiting_Jobs (B : Bunch) return Natural;
   function Get_Errors (B : Bunch) return Natural;
   function Get_Jobs_On_Hold (B : Bunch) return Natural;
   function Get_Quota_Inhibited_Jobs (B : Bunch) return Natural;
   function Has_Balancer (B : Bunch; Capability : Balancer_Capability) return Boolean;
   function Get_Hard_Resources (B : Bunch) return String;
   function Get_Soft_Resources (B : Bunch) return String;
   function Get_Slot_Hash (B : Bunch) return String;
   function Get_Hard_Hash (B : Bunch) return String;
   function Get_Soft_Hash (B : Bunch) return String;

private
   type Bunch is record
      PE, Queue       : Unbounded_String;
      Slot_Number     : Unbounded_String;
      Slot_List       : Ranges.Step_Range_List;
      GPU_Slot_List, CPU_Slot_List : Unbounded_String;
      Hard, Soft      : Resources.Hashed_List;
      Total, On_Hold  : Natural;
      Error, Waiting  : Natural;
      Quota_Inhibited : Natural;
      Balancer        : Balancer_Support;
   end record;

   package Bunch_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Element_Type => Bunch);

   List : Bunch_Lists.List;
end SGE.Bunches;
