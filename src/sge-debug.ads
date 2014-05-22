package SGE.Debug is
   type Facility is (Default, Queues, Trace);
   type Level is range 1 .. 10;
   type Callback is access procedure (Message : String);
   procedure Enable (What : Facility);
   procedure Set_Level (Severity : Level);
   procedure Disable;
   procedure Log (Message : String; Where : Facility; Severity : Level);
   procedure Trace (Entering : String; Params : String);
   procedure Initialize (Input            : String;
                         Output_Procedure : Callback);
private
   Enabled : array (Facility) of Boolean := (Default => False, Queues => False, Trace => False);
   Debug_Level : Level;
   Output : Callback;
end SGE.Debug;
