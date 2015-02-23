with Ada.Characters.Handling;

package body SGE.Taint is

   overriding function "&" (Left, Right : Trusted_String) return Trusted_String is
   begin
      return Trusted_String (String (Left) & String (Right));
   end "&";

   function Value (S : Trusted_String) return String is
   begin
      return String (S);
   end Value;

   function Value (S : Trusted_Command_Name) return String is
   begin
      return String (S);
   end Value;


   --------------
   -- Sanitise --
   --------------

   function Sanitise (S : String) return Trusted_String is
      Output : String := S;

      function Is_Harmless_Dash (Char : in Character; Where : Positive) return Boolean is
      begin
         if Char = '-' and then
           Where > Output'First and then
           Ada.Characters.Handling.Is_Alphanumeric (Output (Where - 1))
         then
            return True; -- a dash, not the first character, and the previous one is alphanumeric
            --  so this does not start a commandline switch
         else
            return False; -- not a dash, or not preceded by a harmless character
         end if;
      end Is_Harmless_Dash;


   begin
      for Pos in Output'Range loop
         if not Ada.Characters.Handling.Is_Letter (Output (Pos))
           and then not Ada.Characters.Handling.Is_Decimal_Digit (Output (Pos))
           and then Output (Pos) /= ';' -- does not start a new command when passed to exec()
           and then Output (Pos) /= ','
           and then Output (Pos) /= '='
           and then not Is_Harmless_Dash (Char  => Output (Pos), Where => Pos)
         then
            Output (Pos) := '_';
         end if;
      end loop;
      return Trusted_String (Output);
   end Sanitise;

   function Implicit_Trust (S : String) return Trusted_String is
   begin
      return Trusted_String (S);
   end Implicit_Trust;

   function Trust_As_Command (S : String) return Trusted_Command_Name is
   begin
      return Trusted_Command_Name (S);
   end Trust_As_Command;

end SGE.Taint;
