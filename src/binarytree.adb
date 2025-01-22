with Ada.Text_IO;            use Ada.Text_IO;
with Ada.Integer_Text_IO;    use Ada.Integer_Text_IO;
with Ada.Unchecked_Deallocation;
with Utils; use Utils;

package body BinaryTree is

	procedure Free is
		new Ada.Unchecked_Deallocation (Object => T_Node, Name => T_BinaryTree);

   -- Initialize empty tree
	procedure initTree(ABR: out T_BinaryTree) is
	begin
		ABR := Null;
   end initTree;

   -- Initialize root node
   procedure initRoot(ABR: out T_BinaryTree; Key : in Integer; Element : in T_Element) is
   begin
      ABR := new T_Node'(Key, Element, Null, Null);
   end initRoot;

   -- Verify if ABR is empty (null)
	function isEmpty (ABR : in T_BinaryTree) return Boolean is
	begin
		return ABR = Null;
	end isEmpty;

   -- Verify if Element is present in ABR
   function isPresent (ABR: in T_BinaryTree; Key : in Integer) return boolean is
   begin
      if isEmpty (ABR) then
         return False;
      elsif ABR.all.Key = Key then
         return True;
      else
         return isPresent(ABR.all.Left, Key) or else isPresent (ABR.all.Right, Key);
      end if;
   end isPresent;

   -- Get the size (number of elements) of ABR
	function getSize (ABR : in T_BinaryTree) return Integer is
	begin
		if isEmpty(ABR) then
         return 0;
      else
         return 1 + getSize(ABR.all.Left) + getSize (ABR.all.Right);
      end if;
	end getSize;

   -- Search Tree by Element
   function getNode (ABR : in T_BinaryTree; Key : in Integer) return T_BinaryTree is
      TempResult : T_BinaryTree;
   begin
      if isEmpty(ABR) then
         return null;
      elsif ABR.all.Key = Key then
         return ABR;
      else
         TempResult := getNode(ABR.all.Left, Key);

         if TempResult /= Null then
            return TempResult;
         else
            return getNode (ABR.all.Right, Key);
         end if;
      end if;
   end getNode;

   -- Add a node to the tree (to the left or the right)
   procedure addNode (ABR : in out T_BinaryTree; NewNode : in T_BinaryTree; TargetKey : in Integer; Position : in T_Position) is
      TempTree : T_BinaryTree;
   begin
      if ABR.all.Key /= TargetKey then
         TempTree := getNode(ABR, TargetKey);
      else
         TempTree := ABR;
      end if;

      case Position is
         when ROOT =>
            Null;
         when RIGHT =>
            --TempTree.all.Right := new T_Node'(NewElement, null, null);
            TempTree.all.Right := NewNode;
         when LEFT =>
            --TempTree.all.Left := new T_Node'(NewElement, null, null);
            TempTree.all.Left := NewNode;
      end case;
   end addNode;

   -- TODO
   procedure deleteNode (ABR : in out T_BinaryTree; Key : in Integer) is
      Tree : T_BinaryTree;
   begin
      if ABR.all.Key /= Key then
         Tree := getNode(ABR, Key);
      else
         Tree := ABR;
      end if;

		Free(Tree);
	end deleteNode;

   -- TODO
   procedure deleteNodeRecursive (ABR : in out T_BinaryTree; Key : in Integer) is
	begin
		Null;	-- TODO : à changer
	end deleteNodeRecursive;

   -- TODO
   procedure clean (ABR : in out T_BinaryTree) is
   begin
      Null;
   end clean;

   procedure showTree (ABR : in T_BinaryTree; PropToShow : in T_PropToShow := Keys; Depth : in Integer := 0; Position : in T_Position := ROOT; Verbosity : in Integer := 1) is
   begin
      if not isEmpty (ABR) then
         if PropToShow = Keys then
            Put_Line(getIndent(Depth) & getBinaryTreePrefix(Position) & Integer'Image(ABR.all.Key));
         else
            PutGeneric(ABR.all.Element, ABR.all.Key, Depth, Position, Verbosity);
         end if;
         showTree(ABR.all.Right, PropToShow, Depth + 1, RIGHT);
         showTree(ABR.all.Left, PropToShow, Depth + 1, LEFT);
      end if;
   end showTree;

end BinaryTree;
