with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with SGE.Resources; use SGE.Resources;
with SGE.Parser;
with Ada.Strings.Bounded;

package SGE.Host_Properties is

   Unsupported_Error, Other_Error : exception;
   type Set_Of_Properties is private;
   type Fixed is delta 0.01 digits 6 range 0.0 .. 1000.0;
   type Load is new Fixed range 0.0 .. 1000.0;

   package Names is new Ada.Strings.Bounded.Generic_Bounded_Length (Max => 12);
   type Host_Name is new Names.Bounded_String;
   overriding function "<" (Left, Right : Host_Name) return Boolean;
   pragma Inline ("<");
   overriding function "=" (Left, Right : Host_Name) return Boolean;
   pragma Inline ("=");

   function To_Host_Name (S : String) return Host_Name;
   function Value (Host : Host_Name) return String;

   function Get_Memory (Props :  Set_Of_Properties) return Gigs;
   function Get_Cores (Props : Set_Of_Properties) return Positive;
   function Get_Network (Props : Set_Of_Properties) return Network;
   function Get_Model (Props : Set_Of_Properties) return CPU_Model;
   function Get_GPU (Props : Set_Of_Properties) return GPU_Model;
   function Get_Runtime (Props : Set_Of_Properties) return String;
   function Get_Runtime (Props : Set_Of_Properties) return Natural;
   function Get_Load_One (Props : Set_Of_Properties) return Load;
   function Has_SSD (Props : Set_Of_Properties) return Boolean;
   function Has_GPU (Props : Set_Of_Properties) return Boolean;
   function Get_Slots (Props : Set_Of_Properties) return Natural;
   procedure Set_Slots (Props : in out Set_Of_Properties; Slots : Natural);
   procedure Set_Memory (Props : in out Set_Of_Properties;
                         S     : String);
   procedure Set_Cores (Props : in out Set_Of_Properties; Cores : Positive);
   procedure Set_Network (Props : in out Set_Of_Properties; Net : Network);
   procedure Set_Model (Props : in out Set_Of_Properties; Model : String);
   procedure Set_Model (Props : in out Set_Of_Properties; Model : CPU_Model);
   procedure Set_Runtime (Props : in out Set_Of_Properties; Runtime : Unbounded_String);
   procedure Set_SSD (Props : in out Set_Of_Properties);
   procedure Set_GPU (Props : in out Set_Of_Properties; Model : GPU_Model);
   procedure Set_GPU (Props : in out Set_Of_Properties; Model : String);
   procedure Set_GPU (Props : in out Set_Of_Properties);

   procedure Init (Props : out Set_Of_Properties;
                   Net, Memory, Cores, Model, SSD, GPU : String);

   function "<" (Left, Right : Set_Of_Properties) return Boolean;
   overriding function "=" (Left, Right : Set_Of_Properties) return Boolean;

   function Get_Mismatch (Left, Right : Set_Of_Properties) return String;

   procedure Parse_Resource (Props : in out Set_Of_Properties;
                             N     : Parser.Node);
   ---------------------
   -- Parse_Resource --
   --  Purpose: Given a Node of an XML DOM tree,
   --  read host properties
   --  Parameter Props : The Set_Of_Properties to update
   --  Parameter V : The Node to read from
   ---------------------

   function To_String (Props : Set_Of_Properties) return String;

private
   type Set_Of_Properties is record
      Network               : Resources.Network := none;
      Model                 : Resources.CPU_Model := none;
      GPU                   : Resources.GPU_Model := none;
      GPU_present           : Boolean := False;
      Memory                : Resources.Gigs := 0.0;
      Cores                 : Positive := 1;
      Runtime               : Unbounded_String;
      SSD                   : Boolean := False;
      Load_One, Load_Five   : Load := 0.0;
      Available_Slots       : Natural := 0;
   end record;
end SGE.Host_Properties;
