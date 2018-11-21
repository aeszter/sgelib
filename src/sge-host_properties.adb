with SGE.Parser; use SGE.Parser;
with Ada.Exceptions; use Ada.Exceptions;
with SGE.Utils;
with Ada.Strings.Fixed;


package body SGE.Host_Properties is

   overriding function "<" (Left, Right : Host_Name) return Boolean is
   begin
      return Names."<" (Names.Bounded_String (Left), Names.Bounded_String (Right));
   end "<";

   pragma Inline ("<");
   overriding function "=" (Left, Right : Host_Name) return Boolean is
   begin
      return Names."=" (Names.Bounded_String (Left), Names.Bounded_String (Right));
   end "=";

   pragma Inline ("=");

   function To_Host_Name (S : String) return Host_Name is
      Separator : constant Natural := Ada.Strings.Fixed.Index (Source => S,
                                                                Pattern => ".");
      Short_Name : constant String := (if Separator = 0 then S
                                       else S (S'First .. Separator - 1));
   begin
      return Host_Name (Names.To_Bounded_String (Source => Short_Name));
   end To_Host_Name;

   function Value (Host : Host_Name) return String is
   begin
      return Names.To_String (Names.Bounded_String (Host));
   end Value;

   function Has_Exclusive (Props : Set_Of_Properties) return Boolean is
   begin
      return Props.Exclusive;
   end Has_Exclusive;

   function Has_SSD (Props : Set_Of_Properties) return Boolean is
   begin
      return Props.SSD;
   end Has_SSD;

   function Has_GPU (Props : Set_Of_Properties) return Boolean is
   begin
      if (Props.GPU = No_GPU and then Props.GPU_present)
        or else (Props.GPU /= No_GPU and then not Props.GPU_present)
      then
         raise SGE.Utils.Operator_Error with "inconsistent GPU config";
      end if;
      return Props.GPU_present;
   end Has_GPU;


   function Get_Load_One (Props : Set_Of_Properties) return Load is
   begin
      return Props.Load_One;
   end Get_Load_One;

   function Get_Queue (Props : Set_Of_Properties) return String is
   begin
      return To_String (Props.Queue_Name);
   end Get_Queue;

   function Get_Runtime (Props : Set_Of_Properties) return String is
   begin
      return To_String (Props.Runtime);
   end Get_Runtime;

   ----------------
   -- Get_Memory --
   ----------------

   function Get_Memory (Props :  Set_Of_Properties) return Gigs is
   begin
      return Props.Memory;
   end Get_Memory;

   function Get_GPU_Memory (Props : Set_Of_Properties) return Gigs is
   begin
      return Props.GPU_Memory;
   end Get_GPU_Memory;

   function Get_PE (Props : Set_Of_Properties) return String is
   begin
      return To_String (Props.PE);
   end Get_PE;

   ---------------
   -- Get_Cores --
   ---------------

   function Get_Cores (Props : Set_Of_Properties) return Positive is
   begin
      return Props.Cores;
   end Get_Cores;

   -----------------
   -- Get_Network --
   -----------------

   function Get_Network (Props : Set_Of_Properties) return Network is
   begin
      return Props.Network;
   end Get_Network;

   function Get_Model (Props : Set_Of_Properties) return CPU_Model is
   begin
      return Props.Model;
   end Get_Model;

   function Get_GPU (Props : Set_Of_Properties) return GPU_Model is
   begin
      return Props.GPU;
   end Get_GPU;

   function Get_Slots (Props : Set_Of_Properties) return Natural is
   begin
      return Props.Available_Slots;
   end Get_Slots;

   procedure Set_Slots (Props : in out Set_Of_Properties; Slots : Natural) is
   begin
      Props.Available_Slots := Slots;
   end Set_Slots;

   procedure Set_Cores (Props : in out Set_Of_Properties; Cores : Positive) is
   begin
      Props.Cores := Cores;
   end Set_Cores;

   procedure Set_Network (Props : in out Set_Of_Properties; Net : Network) is
   begin
      Props.Network := Net;
   end Set_Network;

   procedure Set_Model (Props : in out Set_Of_Properties; Model : CPU_Model) is
   begin
      Props.Model := Model;
   end Set_Model;

   procedure Set_Runtime (Props : in out Set_Of_Properties; Runtime : Unbounded_String) is
   begin
      Props.Runtime := Runtime;
   end Set_Runtime;

   procedure Set_PE (Props : in out Set_Of_Properties; PE : Unbounded_String) is
   begin
      Props.PE := PE;
   end Set_PE;

   procedure Set_Exclusive (Props : in out Set_Of_Properties) is
   begin
      Props.Exclusive := True;
   end Set_Exclusive;

   procedure Set_SSD (Props : in out Set_Of_Properties) is
   begin
      Props.SSD := True;
   end Set_SSD;

   procedure Set_GPU (Props : in out Set_Of_Properties; Model : GPU_Model) is
   begin
      Props.GPU := Model;
   end Set_GPU;

   procedure Set_GPU (Props : in out Set_Of_Properties) is
   begin
      Props.GPU_present := True;
   end Set_GPU;

   procedure Init (Props : out Set_Of_Properties;
                   Net, Memory, Cores, SSD : String;
                   Queue                   : String;
                   Model                   : CPU_Model;
                   GPU                     : GPU_Model) is
   begin
      Set_Network (Props, Network'Value (Net));
      Set_Memory (Props, Memory);
      Set_Cores (Props => Props,
                 Cores => Positive'Value (Cores));
      Set_Model (Props => Props,
                 Model => Model);
      Set_GPU (Props => Props,
               Model => GPU);
      Set_Queue (Props => Props,
                 Queue => Queue);
      if SSD = "TRUE" then
         Set_SSD (Props => Props);
      end if;
   end Init;

   procedure Set_Memory (Props : in out Set_Of_Properties;
                         S     : String) is
   begin
      if S /= "" then
         Props.Memory := To_Gigs (S);
      else
         Props.Memory := 0.0;
      end if;
   end Set_Memory;

   procedure Set_GPU_Memory (Props : in out Set_Of_Properties;
                             S     : String) is
   begin
      if S /= "" then
         Props.GPU_Memory := To_Gigs (S);
      else
         Props.GPU_Memory := 0.0;
      end if;
   end Set_GPU_Memory;

   procedure Set_Queue (Props : in out Set_Of_Properties; Queue : String) is
   begin
      Props.Queue_Name := To_Unbounded_String (Queue);
   end Set_Queue;

   function "<" (Left, Right : Set_Of_Properties) return Boolean is
   begin
      if Left.Network < Right.Network then
         return True;
      elsif Left.Network > Right.Network then
         return False;
      elsif Left.GPU < Right.GPU then
            return True;
      elsif Left.GPU > Right.GPU then
            return False;
      elsif Left.Model < Right.Model then
         return True;
      elsif Left.Model > Right.Model then
         return False;
      elsif Left.Cores < Right.Cores then
         return True;
      elsif Left.Cores > Right.Cores then
         return False;
      elsif Left.Runtime < Right.Runtime then
         return True;
      elsif Left.Runtime > Right.Runtime then
         return False;
      elsif Left.Queue_Name < Right.Queue_Name then
         return True;
      elsif Left.Queue_Name > Right.Queue_Name then
         return False;
      elsif Left.SSD < Right.SSD then
         return True;
      elsif Left.SSD > Right.SSD then
         return False;
      elsif Left.GPU < Right.GPU then
         return True;
      elsif Left.GPU > Right.GPU then
         return False;
      elsif Left.Memory < Right.Memory then
         return True;
      elsif Left.Memory > Right.Memory then
         return False;

      else
         return False;
      end if;
   end "<";

   overriding function "=" (Left, Right : Set_Of_Properties) return Boolean is
   begin
      if Left.Network /= Right.Network then
         return False;
      elsif Left.Model /= Right.Model then
         return False;
      elsif Left.Cores /= Right.Cores then
         return False;
      elsif Left.Runtime /= Right.Runtime then
         return False;
      elsif Left.Queue_Name /= Right.Queue_Name then
         return False;
      elsif Left.SSD /= Right.SSD then
         return False;
      elsif Left.GPU /= Right.GPU then
         return False;
      elsif Left.Memory < Right.Memory * 0.99 then
         return False;
      elsif Left.Memory > Right.Memory * 1.01 then
         return False;

      else
         return True;
      end if;
   end "=";


   function Get_Mismatch (Left, Right : Set_Of_Properties) return String is
   begin
      if Left.Network /= Right.Network then
         return Left.Network'Img & "/=" & Right.Network'Img;
      elsif Left.Model /= Right.Model then
         return To_String (Left.Model) & "/=" & To_String (Right.Model);
      elsif Left.Memory /= Right.Memory then
         return Left.Memory'Img & "/=" & Right.Memory'Img;
      elsif Left.Cores /= Right.Cores then
         return Left.Cores'Img & "/=" & Right.Cores'Img;
      elsif Left.Runtime /= Right.Runtime then
         return To_String (Left.Runtime) & "/=" & To_String (Right.Runtime);
      elsif Left.SSD /= Right.SSD then
         return "SSD: " & Left.SSD'Img & "/=" & Right.SSD'Img;
      elsif Left.GPU /= Right.GPU then
         return "GPU: " & To_String (Left.GPU) & "/=" & To_String (Right.GPU);
      else
         return "matching properties";
      end if;
   end Get_Mismatch;

   ---------------------
   -- Parse_Resources --
   --  Purpose: Given a Node of an XML DOM tree,
   --  read in resources of a Host
   --  Parameter H: The Host to update
   --  Parameter V: The XML Node to read from
   ---------------------

   procedure Parse_Resource (Props : in out Set_Of_Properties;
                             N     : Parser.Node) is
      A : Parser.Attr;
   begin
      A := Get_Attr (N, "name");
      if Value (A) = "num_proc" then
         Set_Cores (Props, Integer (Fixed'Value (Value (First_Child (N)))));
            --  Fixed'Value is important here, as SGE interprets numerical
            --  resources as rational numbers
      elsif Value (A) = "load_short" then
         Props.Load_One := Load'Value (Value (First_Child (N)));
      elsif Value (A) = "load_medium" then
         Props.Load_Five := Load'Value (Value (First_Child (N)));
      elsif Value (A) = "ethernet" then
         if Fixed'Value (Value (First_Child (N))) = 1.0 then
            --  Fixed'Value is important here, as SGE interprets boolean
            --  resources as rational numbers (0.000000 or 1.000000)
            Props.Network := eth;
         end if;
      elsif Value (A) = "infiniband" then
         if Fixed'Value (Value (First_Child (N))) = 1.0 and then
           Props.Network = none
         then
            --  see above for Fixed'Value
            Props.Network := ib;
         end if;
      elsif Value (A) = "ib-switch" then
         if Fixed'Value (Value (First_Child (N))) = 1.0 then
            --  see above for Fixed'Value
            Props.Network := ibswitch;
         end if;
      elsif Value (A) = "cpu_model" then
         Props.Model := To_CPU (Value (First_Child (N)));
      elsif Value (A) = "mem_total" then
         Props.Memory := To_Gigs (Value (First_Child (N)));
      elsif Value (A) = "gpu_memory" then
         Props.GPU_Memory := To_Gigs (Value (First_Child (N)));
      elsif Value (A) = "slots" then
         Props.Available_Slots := Integer (Fixed'Value (Value (First_Child (N))));
      elsif Value (A) = "gpu_model" then
         Props.GPU := To_GPU (Value (First_Child (N)));
      elsif Value (A) = "gpu" then
         Props.GPU_present := True;
      elsif Value (A) = "ssd" then
         Props.SSD := True;
      else
         raise Unsupported_Error with Value (A) & " -> " & Value (First_Child (N));
      end if;
   exception
      when Unsupported_Error => raise;
      when E : others =>
         raise Other_Error with "Unable to read resource: "
                     & Value (A) & " " & Exception_Message (E);
   end Parse_Resource;

   function To_String (Props : Set_Of_Properties) return String is
   begin
      return "(net=>" & Props.Network'Img
        & ",model=>" & To_String (Props.Model)
        & ",mem=>" & Props.Memory'Img
        & ",cores=>" & Props.Cores'Img
        & ",rt=>" & To_String (Props.Runtime)
      & ")";
   end To_String;

   function Get_Runtime (Props : Set_Of_Properties) return Natural is
   begin
      return Integer'Value (To_String (Props.Runtime));
   exception
      when Constraint_Error -- Value is not a number of seconds
         => return Unformat_Duration (To_String (Props.Runtime));
   end Get_Runtime;

end SGE.Host_Properties;
