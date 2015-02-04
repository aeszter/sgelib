with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Ordered_Sets;
with Ada.Containers.Ordered_Maps;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Bounded;
with SGE.Queues; use SGE.Queues;
with SGE.Host_Properties; use SGE.Host_Properties;
with SGE.Loggers; use SGE.Loggers;

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

   type Partition is new Logger with private;
   function Sum (Over : Countable_Map) return Natural;

   procedure Iterate_Available_Slots (P       : Partition;
                                      Process : not null access procedure (Position : Countable_Maps.Cursor));
   function Get_Properties (P : Partition) return Set_Of_Properties;
   function Get_Available_Hosts (P : Partition) return Natural;
   function Get_Available_Slots (P : Partition) return Natural;
   function Get_Offline_Hosts (P : Partition) return Natural;
   function Get_Offline_Slots (P : Partition) return Natural;
   function Get_Suspended_Slots (P : Partition) return Natural;
   function Get_Total_Slots (P : Partition) return Natural;
   function Get_Total_Hosts (P : Partition) return Natural;
   function Get_Used_Slots (P : Partition) return Natural;
   function Get_Used_Hosts (P : Partition) return Natural;
   function Get_Reserved_Slots (P : Partition) return Natural;
   function Get_Reserved_Hosts (P : Partition) return Natural;
   function Get_Disabled_Slots (P : Partition) return Natural;
   function Get_Disabled_Hosts (P : Partition) return Natural;
   function Get_Network (P : Partition) return String;
   function Get_Runtime (P : Partition) return String;
   function Get_Cores (P : Partition) return Natural;
   function Get_Model (P : Partition) return String;
   function Get_GPU (P : Partition) return String;
   function Get_Memory (P : Partition) return String;
   function Has_GPU (P : Partition) return Boolean;
   function Has_SSD (P : Partition) return Boolean;
   function Get_Name (P : Partition) return String;


   type State is (total, available, used, reserved, disabled, offline);
   type State_Count is array (State) of Countable_Map;

   procedure Iterate_Summary (Process : not null access procedure (Item : State));
   function Get_Summary (From : State) return Natural;


   overriding procedure Include
     (Container : in out Countable_Map;
      Key       : Host_Name;
      New_Item  : Natural);


   function To_String (Source : State) return String;

   procedure Build_List;
   procedure Iterate (Process : not null access procedure (P : Partition));
   function New_Partition (Q : Queue) return Partition;

   function "=" (Left : Partition; Right : Queue) return Boolean;
   function "=" (Left : Queue; Right : Partition) return Boolean;

private
   type Partition is new Logger with record
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
   end record;

   package Partition_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Element_Type => Partition);

   type Summarized_List is new Partition_Lists.List with
   record
     Summary : State_Count;
   end record;

   overriding function Copy (Source : Summarized_List) return Summarized_List;

   List : Summarized_List;

end SGE.Partitions;
