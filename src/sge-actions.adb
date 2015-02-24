with POSIX; use POSIX;
with POSIX.Process_Primitives; use POSIX.Process_Primitives;
with POSIX.Process_Identification; use POSIX.Process_Identification;
with POSIX.IO;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Strings.Fixed;
with CGI;
with SGE.Taint; use SGE.Taint;

package body SGE.Actions is

   type Mode is (enable, disable, clear_job, clear_queue);

   procedure Call_Q_Tool (Object : Trusted_String; Action : Mode; Use_Sudo : Boolean);

   procedure Call_Q_Tool (Object : Trusted_String; Action : Mode; Use_Sudo : Boolean) is
      PID          : Process_ID;
      Return_Value : Termination_Status;
      Args         : POSIX.POSIX_String_List;
      Template     : Process_Template;
      Authenticated_User : constant String := CGI.Get_Environment ("REMOTE_USER");
   begin
      if Use_Sudo then
         if Authenticated_User = "" then
            raise Subcommand_Error with "unauthorized";
         end if;
         Append (Args, "sudo");
         Append (Args, "-u");
         Append (Args, To_POSIX_String (Authenticated_User));
         Append (Args, "/cm/shared/apps/sge/current/bin/linux-x64/qmod");
      else
         Append (Args, "qmod");
      end if;
      case Action is
         when enable =>
         Append (Args, "-e");
      when disable =>
            Append (Args, "-d");
         when clear_job =>
            Append (Args, "-cj");
         when clear_queue =>
            Append (Args, "-cq");
      end case;
      Append (Args, To_POSIX_String (Value (Object)));
      Open_Template (Template);
      Set_File_Action_To_Close (Template => Template,
                                File     => POSIX.IO.Standard_Output);
      Start_Process (Child    => PID,
                     Template => Template,
                     Pathname => (if Use_Sudo then "/usr/bin/sudo" else
                        "/cm/shared/apps/sge/current/bin/linux-x64/qmod"),
                     Arg_List => Args);
      Wait_For_Child_Process (Status => Return_Value, Child => PID);
      case Exit_Status_Of (Return_Value) is
         when Normal_Exit => return;
         when Failed_Creation_Exit => raise Subcommand_Error with "Failed to create qmod process";
         when Unhandled_Exception_Exit => raise Subcommand_Error with "Unhandled exception in qmod";
         when 1 => return; -- qmod seems to return 1 every time; no documentation found
         when others => raise Subcommand_Error with "qmod exited with status" & Exit_Status_Of (Return_Value)'Img;
      end case;
   exception
      when E : POSIX_Error =>
         raise Subcommand_Error with "qmod raised error when called with " & Action'Img
              & Value (Object) & ": " & Exception_Message (E);
   end Call_Q_Tool;

   procedure Enable (The_Node : String) is
   begin
      Call_Q_Tool (Object   => Sanitise (The_Node),
                 Action   => enable,
                 Use_Sudo => False);
   end Enable;

   procedure Disable (The_Node : String) is
   begin
      Call_Q_Tool (Object   => Sanitise (The_Node),
                 Action   => disable,
                 Use_Sudo => False);
   end Disable;

   procedure Clear_Error (The_Node : String) is
   begin
      Call_Q_Tool (Object   => Sanitise (The_Node),
                 Action   => clear_queue,
                 Use_Sudo => True);
   end Clear_Error;

   procedure Clear_Error (The_Job : Positive) is
      package Str renames Ada.Strings;
   begin
      Call_Q_Tool (Object   => Sanitise (Str.Fixed.Trim (The_Job'Img, Str.Left)),
                 Action   => clear_job,
                 Use_Sudo => True);
   end Clear_Error;

end SGE.Actions;
