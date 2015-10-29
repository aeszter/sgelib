with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with SGE.Host_Properties; use SGE.Host_Properties;
with SGE.Parser; use SGE.Parser;
with SGE.Containers; use SGE.Containers;
with Ada.Containers.Indefinite_Doubly_Linked_Lists;
with Ada.Iterator_Interfaces;
with Ada.Finalization; use Ada.Finalization;

package SGE.Queues is

   type Queue is tagged private;
   type List is new SGE.Containers.Container with private
     with Default_Iterator => Iterate,
     Iterator_Element => Queue'Class;
      type Cursor is private;

   function Has_Element (Position : Cursor) return Boolean;

   package List_Iterator_Interfaces is
     new Ada.Iterator_Interfaces (Cursor, Has_Element);

   overriding function Length (Collection : List) return Natural;
   overriding function Is_Empty (Collection:List) return Boolean;
   overriding procedure Clear (Collection : in out List);

   procedure Append_List (Storage : in out List; Input_Nodes : Node_List);
   function Iterate (Container : List) return
     List_Iterator_Interfaces.Reversible_Iterator'Class;
   function New_Queue (List : Node_List) return Queue;
   procedure Occupy_Slots  (Q : in out Queue; How_Many : Natural);

   procedure Set_Host_Name (Q : in out Queue; Long_Name : String);
   procedure Decompose_Long_Name (Long_Name : String; Queue : out Unbounded_String; Host : out Host_Name);

   function Precedes_By_Resources (Left, Right : Queue'Class) return Boolean;
   function Precedes_By_Sequence (Left, Right : Queue'Class) return Boolean;

   procedure Sort (What : in out List);
   procedure Sort_By_Sequence (What : in out List);

   function Get_Properties (Q : Queue) return Set_Of_Properties;
   function Get_Name (Q : Queue) return Unbounded_String;
   function Get_Name (Q : Queue) return String;
   function Get_Host_Name (Q : Queue) return Host_Properties.Host_Name;
   function Get_Slot_Count (Q : Queue) return Natural;
   function Get_Used_Slots (Q : Queue) return Natural;
   function Get_Reserved_Slots (Q : Queue) return Natural;
   function Get_Free_Slots (Q : Queue) return Natural;
   function Is_Offline (Q : Queue) return Boolean;
   function Is_Disabled (Q : Queue) return Boolean;
   function Is_Suspended (Q : Queue) return Boolean;
   function Get_Type (Q : Queue) return String;

   function Has_Error (Q : Queue) return Boolean;
   function Has_Disabled (Q : Queue) return Boolean;
   function Has_Calendar_Disabled (Q : Queue) return Boolean;
   function Has_Unreachable (Q : Queue) return Boolean;
   function Has_Suspended (Q : Queue) return Boolean;
   function Has_Old_Config (Q : Queue) return Boolean;
   function Is_Batch (Q : Queue) return Boolean;
   function Is_Interactive (Q : Queue) return Boolean;
   function Is_Parallel (Q : Queue) return Boolean;

private
   type State_Flag is (alarm, disabled, error, unreachable, old, suspended, calendar_disabled);
   type Type_Flag is (B, I, P);
   type State_Array is array (State_Flag) of Boolean;
   type Type_Array is array (Type_Flag) of Boolean;

   type Queue is tagged record
      Used, Reserved, Total : Natural;
      Sequence              : Natural := 0;
      Name                  : Unbounded_String;
      Host                  : Host_Properties.Host_Name;
      Properties            : Set_Of_Properties;
      State                 : State_Array := (others => False);
      Q_Type                : Type_Array := (others => False);
   end record;

   package Queue_Lists is
     new Ada.Containers.indefinite_Doubly_Linked_Lists (Element_Type => Queue'Class);

   package Sorting_By_Resources is
     new Queue_Lists.Generic_Sorting ("<" => Precedes_By_Resources);
   package Sorting_By_Sequence is
      new Queue_Lists.Generic_Sorting ("<" => Precedes_By_Sequence);

   type Cursor is new Queue_Lists.Cursor;
   type Iterator is new Limited_Controlled and
     List_Iterator_Interfaces.Reversible_Iterator with
      null record;

   overriding function First (Object : Iterator) return Cursor;
   overriding function Last  (Object : Iterator) return Cursor;

   overriding function Next
     (Object   : Iterator;
      Position : Cursor) return Cursor;

   overriding function Previous
     (Object   : Iterator;
      Position : Cursor) return Cursor;


   type List is new Container with record
      Data : Queue_Lists.List;
   end record;
end SGE.Queues;
