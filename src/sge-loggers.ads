with SGE.Utils;
package SGE.Loggers is
   type Logger is tagged private;

   function Has_Errors (Object : Logger) return Boolean;
   function Errors_Exist return Boolean;
   procedure Record_Error (Object : in out Logger; Message : String);
   procedure Record_Error (Message : String);
   procedure Iterate_Errors (Object  : Logger;
                             Process : not null access procedure (Message : String));
   procedure Iterate_Errors (Process : not null access procedure (Message : String));

private
   type Logger is tagged record
      Error_Log : SGE.Utils.String_List;
   end record;
end SGE.Loggers;
