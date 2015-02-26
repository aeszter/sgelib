with POSIX.Process_Primitives; use POSIX.Process_Primitives;
with POSIX.Process_Environment; use POSIX.Process_Environment;
with Ada.IO_Exceptions;
with Ada.Strings.Fixed;
with SGE.Utils;

package body SGE.Plain_Pipe_Streams is

   ---------------
   -- Next_Char --
   ---------------

   procedure Next_Char
     (From : in out Plain_Pipe_Stream;
      C    : out Character)
   is
   begin
      if From.Position >= Integer (From.Last_Read) then
         POSIX.IO.Read (File           => From.Pipe,
                        Buffer         => From.Buffer,
                        Last           => From.Last_Read);
         From.Position := 0;
      end if;
      From.Position := From.Position + 1;
      C := Standard.Character (From.Buffer (From.Position));
   exception
      when Ada.IO_Exceptions.End_Error =>
         From.Eof_Reached := True;
         C := Standard.Character (LF);
      when others =>
         raise;
   end Next_Char;

   ---------
   -- Eof --
   ---------

   function Eof (From : Plain_Pipe_Stream) return Boolean is
   begin
      return From.Eof_Reached;
   end Eof;

   -----------
   -- Close --
   -----------

   procedure Close (Input : in out Plain_Pipe_Stream; Exit_Status : out Natural) is
      Status : Termination_Status;
   begin
      Wait_For_Child_Process (Status => Status, Child => Input.PID);
      Exit_Status := Natural (Exit_Status_Of (Status));
      case Exit_Status_Of (Status) is
         when Normal_Exit => return;
         when Failed_Creation_Exit => raise Failed_Creation_Error;
         when Unhandled_Exception_Exit => raise Exception_Error;
         when others => return;
      end case;
   end Close;

   -------------
   -- Execute --
   -------------

   procedure Execute (P : in out Plain_Pipe_Stream;
                      Command : Trusted_Command_Name;
                      Arguments : Trusted_String;
                     Environment : Trusted_String)
   is
      To_QView : POSIX.IO.File_Descriptor;
      Template : Process_Template;
      Arg_List : POSIX_String_List;
      Trusted_Environment : constant String := Value (Environment);
      Separator : Natural := Ada.Strings.Fixed.Index (Trusted_Environment, "=");
      Env : POSIX.Process_Environment.Environment;
   begin
      POSIX.IO.Create_Pipe (Read_End  => P.Pipe,
                            Write_End => To_QView);
      Open_Template (Template);
      Set_File_Action_To_Close (Template => Template,
                                File     => P.Pipe);
      Set_File_Action_To_Duplicate (Template  => Template,
                                    File      => Standard_Output,
                                    From_File => To_QView);

      Utils.To_String_List (Source => Value (Command) & " " & Value (Arguments),
                            Dest   => Arg_List);
      Set_Environment_Variable
        (Name  => To_POSIX_String (Trusted_Environment (Trusted_Environment'First .. Separator - 1)),
         Value => To_POSIX_String (Trusted_Environment (Separator + 1 .. Trusted_Environment'Last)),
         Env   => Env);

      Start_Process (Child    => P.PID,
                     Pathname => To_POSIX_String (Value (Command)),
                     Template => Template,
                     Arg_List => Arg_List,
                     Env_List => Env);
      Close (File => To_QView);
   end Execute;

end SGE.Plain_Pipe_Streams;
