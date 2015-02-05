with POSIX; use POSIX;
with POSIX.Process_Primitives; use POSIX.Process_Primitives;
with POSIX.Process_Identification; use POSIX.Process_Identification;
with POSIX.IO;
with Ada.Exceptions; use Ada.Exceptions;

package body SGE.Actions is

   type Mode is (enable, disable, clear_job, clear_queue);

   procedure Disable_Or_Enable (Object : String; Action : Mode);

   procedure Disable_Or_Enable (Object : String; Action : Mode) is

      PID          : Process_ID;
      Return_Value : Termination_Status;
      Args         : POSIX.POSIX_String_List;
      Template     : Process_Template;
   begin
      Append (Args, "qmod");
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
      Append (Args, To_POSIX_String (Object));
      Open_Template (Template);
      Set_File_Action_To_Close (Template => Template,
                                File     => POSIX.IO.Standard_Output);
      Start_Process (Child    => PID,
                     Template => Template,
                     Pathname => "/cm/shared/apps/sge/current/bin/linux-x64/qmod",
                     Arg_List => Args);
      Wait_For_Child_Process (Status => Return_Value, Child => PID);
      case Exit_Status_Of (Return_Value) is
         when Normal_Exit => return;
         when Failed_Creation_Exit => raise Subcommand_Error with "Failed to create qmod process";
         when Unhandled_Exception_Exit => raise Subcommand_Error with "Unhandled exception in qmod";
         when others => raise Subcommand_Error with "qmod exited with status" & Exit_Status_Of (Return_Value)'Img;
      end case;
   exception
      when E : POSIX_Error =>
         raise Subcommand_Error with "qmod raised error when called with " & Action'Img
              & Object & ": " & Exception_Message (E);
   end Disable_Or_Enable;

   procedure Enable (The_Node : String) is
   begin
      Disable_Or_Enable (Object => The_Node,
                          Action   => enable);
   end Enable;

   procedure Disable (The_Node : String) is
   begin
      Disable_Or_Enable (Object => The_Node,
                          Action   => disable);
   end Disable;

   procedure Clear_Error (The_Node : String) is
   begin
      Disable_Or_Enable (Object => The_Node,
                         Action => clear_queue);
   end Clear_Error;

   procedure Clear_Error (The_Job : Positive) is
   begin
      Disable_Or_Enable (Object => The_Job'Img,
                         Action => clear_job);
   end Clear_Error;

end SGE.Actions;
