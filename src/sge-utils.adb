with Ada.Strings;
with Ada.Strings.Fixed;
with SGE.Utils; use SGE.Utils.String_Lists;
with GNAT.Calendar.Time_IO;
with Ada.Calendar.Conversions;
with Interfaces.C;
with SGE.Parser;
with SGE.Spread_Sheets;
with SGE.Taint; use SGE.Taint;

package body SGE.Utils is

   ------------------
   -- To_Tri_State --
   ------------------

   function To_Tri_State (Truth : String) return Tri_State is
   begin
      if Truth = "true" then
         return True;
      elsif Truth = "false" then
         return False;
      else
         return Undecided;
      end if;
   end To_Tri_State;

   function To_Tri_State (Truth : Boolean) return Tri_State is
   begin
      if Truth then
         return True;
      else
         return False;
      end if;
   end To_Tri_State;

   --------------------
   -- To_Hash_String --
   --------------------

   function To_Hash_String (S : String) return Hash_String_Type is
      First : Natural := S'First;
   begin
      if S (First) = ' ' then
         First := S'First + 1;
      end if;
      if S'Last - First + 1 > Hash_Strings.Max_Length then
         raise Ada.Strings.Length_Error;
      end if;
      return Hash_Strings.To_Bounded_String (Source => S (First .. S'Last));
   end To_Hash_String;

   --------------------
   -- To_String_List --
   --------------------

   procedure To_String_List
     (Source  : String;
      Dest   : out POSIX_String_List)
   is
      Next_Index : Natural := 1;
      Index_List : array (1 .. 256) of Natural;
   begin
      Index_List (Next_Index) := Source'First;
      while Index_List (Next_Index) <= Source'Last loop
         Next_Index := Next_Index + 1;
         Index_List (Next_Index) := 1 + Ada.Strings.Fixed.Index (Source (Index_List (Next_Index - 1) .. Source'Last), " ");
         if Index_List (Next_Index) = 1 then
            Index_List (Next_Index) := Source'Last + 2;
         end if;
         POSIX.Append (Dest, To_POSIX_String (Source (Index_List (Next_Index - 1) .. Index_List (Next_Index) - 2)));
      end loop;
   end To_String_List;

   function To_User_Name (User : String) return User_Name is
      use Ada.Strings.Fixed;
   begin
      return User_Name (Head (Source => User,
                              Count  => User_Name'Length));
   end To_User_Name;

   function To_String (User : User_Name) return String is
   begin
      return String (User);
   end To_String;

   function To_Time (Time_String : String) return Ada.Calendar.Time is
      Time_Buffer : String (1 .. 19);
   begin
      if Time_String'Length > 11 and then
        Time_String (Time_String'First + 10) = 'T'
      then
         Time_Buffer := Time_String;
         Time_Buffer (11) := ' ';
         return GNAT.Calendar.Time_IO.Value (Time_Buffer);
      else
         return Ada.Calendar.Conversions.To_Ada_Time
           (Interfaces.C.long'Value (Time_String));
      end if;
   end To_Time;

   function User_Is_In_List (User : String; List_Selector : Trusted_String) return Boolean is
      use SGE.Spread_Sheets;

      Exit_Status : Natural;
      Result : Spread_Sheet;
   begin
      if User = "" then
         return False;
      end if;
      SGE.Parser.Setup_No_XML (Command     => Trust_As_Command ("qconf"),
                               Subpath     => Implicit_Trust ("/bin/linux-x64/"),
                               Selector    => List_Selector,
                               Output      => Result,
                               Exit_Status => Exit_Status);
      if Exit_Status /= 0 then
         return False;
      end if;
      Result.Rewind;
      while not Result.At_End loop
         if Result.Current = User then
            return True;
         end if;
         Result.Next;
      end loop;

      return False;
   end User_Is_In_List;

   function User_Is_Operator (User : String) return Boolean is
   begin
      return User_Is_In_List (User, Implicit_Trust ("-so"));
   end User_Is_Operator;

   function User_Is_Manager (User : String) return Boolean is
   begin
      return User_Is_In_List (User, Implicit_Trust ("-sm"));
   end User_Is_Manager;

end SGE.Utils;
