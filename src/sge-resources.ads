with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with SGE.Utils; use SGE.Utils;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Ordered_Maps;
with Ada.Strings.Bounded;

package SGE.Resources is

   type Resource is record
      Value          : Unbounded_String;
      Numerical      : Integer;
      Boolean_Valued : Boolean;
      State          : Tri_State;
   end record;

   package Strings is new Ada.Strings.Bounded.Generic_Bounded_Length (Max => 100);

   type Network is (none, eth, ib, ibswitch);
   pragma Compile_Time_Warning (True, "hardcoded config");
   type CPU_Model is new Strings.Bounded_String;
   No_CPU : constant CPU_Model := CPU_Model (Strings.To_Bounded_String (""));
   type GPU_Model is new Strings.Bounded_String;
   No_GPU : constant GPU_Model := GPU_Model (Strings.To_Bounded_String (""));

   type Gigs is delta 0.001 digits 7;

   Resource_Error : exception;

   function To_Network (S : String) return Network;
   function To_CPU (S : String) return CPU_Model;
   overriding function To_String (CPU : CPU_Model) return String;
   function To_GPU (S : String) return GPU_Model;
   overriding function To_String (GPU : GPU_Model) return String;
   function Format_Duration (Secs : Natural) return String;
   function Unformat_Duration (Dur : String) return Natural;
   function To_Gigs (Memory : String) return Gigs;
   function To_String (Memory : Gigs) return String;

   function New_Resource (Name  : String;
                          Value : Unbounded_String;
                          Boolean_Valued : Boolean;
                          State : Tri_State)
                          return Resource;
   function New_Resource (Name : String; Value : String)
                          return Resource;

   function Hash (R : Resource) return Hash_Type;



   package Resource_Lists is
     new Ada.Containers.Ordered_Maps (Element_Type => Resource,
                                      Key_Type     => Unbounded_String);

   type Hashed_List is new Resource_Lists.Map with private;

   Empty_List : constant Hashed_List;

   overriding procedure Insert
     (Container : in out Hashed_List;
      Key       : Unbounded_String;
      New_Item  : Resource;
      Position  : out Resource_Lists.Cursor;
      Inserted  : out Boolean);

   overriding procedure Insert
     (Container : in out Hashed_List;
      Key       : Unbounded_String;
      Position  : out Resource_Lists.Cursor;
      Inserted  : out Boolean);

   overriding procedure Insert
     (Container : in out Hashed_List;
      Key       : Unbounded_String;
      New_Item  : Resource);

   overriding procedure Include
     (Container : in out Hashed_List;
      Key       : Unbounded_String;
      New_Item  : Resource);

   function Contains (Container : Hashed_List; Key : String) return Boolean;

   function "<" (Left, Right : Resource) return Boolean;


   --  Purpose: Return the Lists's hash value
   function Hash (List : Hashed_List) return String;
   function Precedes (Left, Right : Hashed_List) return Boolean;

   function To_Unbounded_String (L : Hashed_List) return Unbounded_String;
   function To_String (L : Hashed_List) return String;

   function Value (L : Hashed_List; Name : String) return String;
   function Numerical (L : Hashed_List; Name : String) return Integer;

   overriding function Copy (Source : Hashed_List) return Hashed_List;

private
   type Hashed_List is new Resource_Lists.Map with
      record
         Hash_Value : Hash_Type := 0;
         Hash_String : Utils.Hash_String_Type;
      end record;

   --  Purpose: unconditionally compute the List's hash value
   procedure Rehash (List : in out Hashed_List);

   Empty_List : constant Hashed_List := (Resource_Lists.Empty_Map
                                         with Hash_Value => 0,
                                         Hash_String     => Utils.Hash_Strings.Null_Bounded_String);

end SGE.Resources;
