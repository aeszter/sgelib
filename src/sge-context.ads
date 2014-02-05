with Ada.Containers.Indefinite_Ordered_Maps;


package SGE.Context is
   type Key_Type is (Slots_CPU, Slots_GPU, Slots_Reduce, Slots_Extend,
                     Wait_Reduce, Last_Reduction, Last_Extension, Last_Migration,
                    Reduced_Runtime);
   type Literal_Type is array (Key_Type) of access constant String;

   type List is tagged private;

   Slots_CPU_String       : aliased constant String := "SLOTSCPU";
   Slots_GPU_String       : aliased constant String := "SLOTSGPU";
   Slots_Reduce_String    : aliased constant String := "SLOTSREDUCE";
   Slots_Extend_String    : aliased constant String := "SLOTSEXTEND";
   Wait_Reduce_String     : aliased constant String := "WAITREDUCE";
   Last_Reduction_String  : aliased constant String := "LASTRED";
   Last_Extension_String  : aliased constant String := "LASTEXT";
   Last_Migration_String  : aliased constant String := "LASTMIG";
   Reduced_Runtime_String : aliased constant String := "RTREDUCE";
   Literal : Literal_Type :=
               (Slots_CPU       => Slots_CPU_String'Access,
                Slots_GPU       => Slots_GPU_String'Access,
                Slots_Reduce    => Slots_Reduce_String'Access,
                Slots_Extend    => Slots_Extend_String'Access,
                Wait_Reduce     => Wait_Reduce_String'Access,
                Last_Reduction  => Last_Reduction_String'Access,
                Last_Extension  => Last_Extension_String'Access,
                Last_Migration  => Last_Migration_String'Access,
                Reduced_Runtime => Reduced_Runtime_String'Access);

   function Get (From : List; Key : Key_Type) return String;
   function Contains (Container : List; Key : Key_Type) return Boolean;
   procedure Include (Container : in out List; Key : String; New_Item : String);
   function Is_Empty (Container : List) return Boolean;
   procedure Iterate (Container : List;
                      Process   : not null access procedure (Key, Element : String));

private
   package Lists is new Ada.Containers.Indefinite_Ordered_Maps (Key_Type => String,
                                                            Element_Type => String);
   type List is new Lists.Map with null record;
end SGE.Context;
