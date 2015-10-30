with ada.Iterator_Interfaces;

generic
   type Element_Type (<>) is tagged private;
package SGE.Containers is
   type Container is abstract tagged null record;

   function Length (Collection:Container) return Natural is abstract;
   procedure Clear (Collection : in out Container) is abstract;
   function Is_Empty (Collection: Container) return Boolean is abstract;
   procedure Iterate (Collection: Container;
                      Process   : not null access procedure (Item : aliased Element_Type));



end ;
