
package body SGE.Context is
   function Get (From : List; Key : Key_Type) return String is
   begin
      if From.Contains (Literal (Key).all) then
         return From.Element (Literal (Key).all);
      else
         return "";
      end if;
   end Get;

   function Contains (Container : List; Key : Key_Type) return Boolean is
   begin
      return Container.Contains (Literal (Key).all);
   end Contains;

   overriding procedure Include (Container : in out List; Key : String; New_Item : String) is
   begin
      Lists.Include (Container => Lists.Map (Container),
                     Key       => Key,
                     New_Item  => New_Item);
   end Include;

   overriding function Is_Empty (Container : List) return Boolean is
   begin
      return Lists.Is_Empty (Lists.Map (Container));
   end Is_Empty;

   procedure Iterate (Container : List;
                      Process   : not null access procedure (Key, Element : String)) is
      procedure Wrapper (Position : Lists.Cursor) is
      begin
         Process (Key => Lists.Key (Position), Element => Lists.Element (Position));
      end Wrapper;

   begin
      Lists.Iterate (Container => Lists.Map (Container),
                     Process   => Wrapper'Access);
   end Iterate;

end SGE.Context;
