with DOM.Core; with DOM.Core.Documents;
with DOM.Core.Nodes;
with POSIX.Process_Environment;
with SGE.Spread_Sheets;
with SGE.Taint; use SGE.Taint;


package SGE.Parser is
   subtype Tree is DOM.Core.Document;
   subtype Attr is DOM.Core.Attr;
   subtype Node is DOM.Core.Node;
   subtype Node_List is DOM.Core.Node_List;

   package Env renames POSIX.Process_Environment;

   Parser_Error : exception;

   function Get_Attr (N : Node; Name : String) return Attr;
   function Value (N : Node) return String renames DOM.Core.Nodes.Node_Value;
   function Name (N : Node) return String renames DOM.Core.Nodes.Node_Name;
   function First_Child (N : Node) return Node renames DOM.Core.Nodes.First_Child;
   function Length (List : Node_List) return Natural renames DOM.Core.Nodes.Length;
   function Has_Child_Nodes (N : Node) return Boolean renames DOM.Core.Nodes.Has_Child_Nodes;
   function Child_Nodes (N : Node) return Node_List renames DOM.Core.Nodes.Child_Nodes;
   function Item (List : Node_List; Index : Natural) return Node renames DOM.Core.Nodes.Item;

   function Setup (Command  : Trusted_Command_Name;
                   Selector : Trusted_String) return DOM.Core.Document;
   procedure Setup_No_XML (Command  : Trusted_Command_Name;
                          Selector : Trusted_String;
                          Subpath  : Trusted_String := Implicit_Trust ("/utilbin/linux-x64/");
                          Output : out Spread_Sheets.Spread_Sheet;
                          Exit_Status : out Natural);
   procedure Free;
   function Get_Elements_By_Tag_Name
     (Doc : DOM.Core.Document; Tag_Name : DOM.Core.DOM_String := "*")
      return DOM.Core.Node_List
      renames DOM.Core.Documents.Get_Elements_By_Tag_Name;

   function Get_Job_Nodes_From_Qstat_J (Doc : DOM.Core.Document)
                                        return DOM.Core.Node_List;
   function Get_Message_Nodes_From_Qstat_J (Doc : DOM.Core.Document)
                                        return DOM.Core.Node_List;
   function Get_Job_Nodes_From_Qstat_U (Doc : DOM.Core.Document)
                                           return DOM.Core.Node_List;

private
   sgeroot : constant String := POSIX.To_String (Env.Environment_Value_Of ("SGE_ROOT"));

end SGE.Parser;
