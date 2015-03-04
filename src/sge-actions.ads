package SGE.Actions is
   procedure Enable (The_Node : String; Use_Sudo : Boolean := False);
   procedure Disable (The_Node : String);
   procedure Clear_Error (The_Node : String);
   procedure Clear_Error (The_Job : Positive);
   procedure Kill_Job (The_Job : Positive);

   Subcommand_Error : exception;
private

end SGE.Actions;
