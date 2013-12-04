with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Ordered_Sets;
with Ada.Containers.Ordered_Maps;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Bounded;
with SGE.Queues; use SGE.Queues;
with SGE.Host_Properties; use SGE.Host_Properties;
with SGE.Utils;

package SGE.Partitions is

   package Host_Names is new Ada.Strings.Bounded.Generic_Bounded_Length (Max => 12);
   subtype Host_Name is Host_Names.Bounded_String;
   package Countable_Sets is new Ada.Containers.Ordered_Sets (Element_Type => Host_Name,
                                                              "<"          => Host_Names."<",
                                                              "="          => Host_Names."=");
   package Countable_Maps is new Ada.Containers.Ordered_Maps (Key_Type     => Host_Name,
                                                              Element_Type => Natural,
                                                              "<"          => Host_Names."<",
                                                              "="          => "=");

   type Countable_Map is new Countable_Maps.Map with null record;

   function Sum (Over : Countable_Map) return Natural;

   type Partition is record
      Used_Slots,
      Reserved_Slots,
      Suspended_Slots : Natural := 0;
      Total_Slots     : Countable_Map;
      Available_Hosts,
      Total_Hosts,
      Offline_Hosts,
      Reserved_Hosts,
      Used_Hosts,
      Disabled_Hosts  : Countable_Sets.Set;
      Available_Slots,
      Disabled_Slots,
      Offline_Slots   : Countable_Map;
      Name            : Unbounded_String;
      Properties      : Set_Of_Properties;
      Error_Log       : Utils.String_List;
   end record;

   function Get_Available_Hosts (P : Partition) return Natural;
   function Get_Available_Slots (P : Partition) return Natural;
   function Get_Offline_Slots (P : Partition) return Natural;
   function Get_Total_Slots (P : Partition) return Natural;
   function Get_Network (P : Partition) return String;
   function Get_Runtime (P : Partition) return String;
   function Get_Cores (P : Partition) return Natural;
   function Get_Model (P : Partition) return String;
   function Get_Memory (P : Partition) return String;
   function Has_GPU (P : Partition) return Boolean;
   function Has_SSD (P : Partition) return Boolean;


   type State is (total, available, used, reserved, disabled, offline);
   type State_Count is array (State) of Countable_Map;

   procedure Iterate_Summary (Process : not null access procedure (Item : State));
   function Get_Summary (From : State) return Natural;


   overriding procedure Include
     (Container : in out Countable_Map;
      Key       : Host_Name;
      New_Item  : Natural);


   package Partition_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Element_Type => Partition);

   type Summarized_List is new Partition_Lists.List with
   record
     Summary : State_Count;
   end record;
   function To_String (Source : State) return String;

   procedure Build_List;
   procedure Iterate (Process : not null access procedure (P : Partition));
   function New_Partition (Q : Queue) return Partition;

   function "=" (Left : Partition; Right : Queue) return Boolean;
   function "=" (Left : Queue; Right : Partition) return Boolean;
private
   List : Summarized_List;

   procedure Record_Error (P : in out Partition; Message : String);
   --  Purpose: store an error message for retrieval by the calling application
   --  without raising an exception (so we can resume Library oprations)

end SGE.Partitions;
