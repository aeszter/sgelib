package SGE.Taint is

   --  data of type Trusted_String can be passed as parameters to external programs
   type Trusted_String (<>) is private;

   function "&" (Left, Right : Trusted_String) return Trusted_String;

   --  data of type Trusted_Command_Name can be used to call external programs across
   --  privilege borders
   type Trusted_Command_Name (<>) is private;

   --  Extract the string value from a trusted string
   function Value (S : Trusted_String) return String;

   --  Extract the string value from a trusted string
   function Value (S : Trusted_Command_Name) return String;

   --  convert untrusted user data to a trusted string by removing/replacing
   --  any offending characters
   function Sanitise (S : String) return Trusted_String;

   --  convert untrusted user data to a trusted string by removing/replacing
   --  any offending characters;
   --  This function specialises in job id lists and can therefore be
   --  more agressive.
   function Sanitise_Job_List (S : String) return Trusted_String;

   --  convert an implicitly trusted String to a Trusted_String;
   --  Never!! pass untrusted user data to this function.
   --  It is meant for program-internal data only.
   function Implicit_Trust (S : String) return Trusted_String;

   --  convert an implicitly trusted String to a Trusted_Command;
   --  Never!! pass untrusted user data to this function.
   --  It is meant for program-internal data only.
   function Trust_As_Command (S : String) return Trusted_Command_Name;

   Cmd_Qstat, Cmd_Qmod, Cmd_Qhost, Cmd_Qquota : constant Trusted_Command_Name;

private
   type Trusted_String is new String;
   type Trusted_Command_Name is new String;

   Cmd_Qstat : constant Trusted_Command_Name := "qstat";
   Cmd_Qmod : constant Trusted_Command_Name := "qmod";
   Cmd_Qhost : constant Trusted_Command_Name := "qhost";
   Cmd_Qquota : constant Trusted_Command_Name := "qquota";
end SGE.Taint;
