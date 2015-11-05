with ada.Iterator_Interfaces;

package SGE.Containers is
   type Container is abstract tagged null record;

   function Length (Collection:Container) return Natural is abstract;
   procedure Clear (Collection : in out Container) is abstract;



end ;
