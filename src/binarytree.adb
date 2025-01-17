with Ada.Text_IO;            use Ada.Text_IO;
with Ada.Integer_Text_IO;    use Ada.Integer_Text_IO;
with Ada.Unchecked_Deallocation;

package body BinaryTree is

	procedure Free is
		new Ada.Unchecked_Deallocation (Object => T_Node, Name => T_BinaryTree);

   -- Initialize empty tree
	procedure initTree(ABR: out T_BinaryTree) is
	begin
		ABR := Null;
   end initTree;

   -- Initialize root node
   procedure initRoot(ABR: in out T_BinaryTree; Element : in T_Element) is
      --Node : T_BinaryTree := new T_Node'(Element, Null, Null);
   begin
      ABR := new T_Node'(Element, Null, Null);
   end initRoot;

   -- Verify if ABR is empty (null)
	function isEmpty (ABR : T_BinaryTree) return Boolean is
	begin
		return ABR = Null;
	end isEmpty;

   -- Verify if Element is present in ABR
   function isPresent (ABR: in T_BinaryTree; Element : in T_Element) return boolean is
   begin
      if isEmpty (ABR) then
         return False;
      elsif ABR.all.Element = Element then
         return True;
      else
         return isPresent(ABR.all.Left, Element) or else isPresent (ABR.all.Right, Element);
      end if;
   end isPresent;

   -- Get the size (number of elements) of ABR
	function getSize (ABR : in T_BinaryTree) return Integer is
	begin
		if ABR = null then
         return 0;
      else
         return 1 + getSize(ABR.all.Left) + getSize (ABR.all.Right);
      end if;
	end getSize;

   -- Search Tree by Element
   function getTree (ABR : in T_BinaryTree; Element : in T_Element) return T_BinaryTree is
   begin
      return Null;
   end getTree;

   -- Add a node to the tree (to the left or the right)
   procedure addNode (ABR : in out T_BinaryTree; NewNode : in T_BinaryTree; TargetElement : in T_Element; Position : in T_Position) is
      TempTree : T_BinaryTree;
   begin
      if ABR.all.Element /= TargetElement then
         TempTree := getTree(ABR, TargetElement);
      else
         TempTree := ABR;
      end if;

      case Position is
         when RIGHT =>
            --TempTree.all.Right := new T_Node'(NewElement, null, null);
            TempTree.all.Right := NewNode;
         when LEFT =>
            --TempTree.all.Left := new T_Node'(NewElement, null, null);
            TempTree.all.Left := NewNode;
      end case;
   end addNode;

   -- TODO
   procedure deleteNode (ABR : in out T_BinaryTree; Element : in T_Element) is
      Tree : T_BinaryTree;
   begin
      if ABR.all.Element /= Element then
         Tree := getTree(ABR, Element);
      else
         Tree := ABR;
      end if;

		Free(Tree);
	end deleteNode;

   -- TODO
   procedure deleteNodeRecursive (ABR : in out T_BinaryTree; Element : in T_Element) is
	begin
		Null;	-- TODO : à changer
	end deleteNodeRecursive;

   -- TODO
   procedure clean (ABR : in out T_BinaryTree) is
   begin
      Null;
   end clean;

   -- TODO
   procedure show (ABR : in T_BinaryTree) is
   begin
      if not isEmpty (ABR) then
         Put_Generic(ABR.all.Element);
         show(ABR.all.Right);
         show(ABR.all.Left);
      end if;
   end show;

end BinaryTree;
