with Ada.Text_IO;            use Ada.Text_IO;
with Ada.Integer_Text_IO;    use Ada.Integer_Text_IO;
with Ada.Unchecked_Deallocation;

package body BinaryTree is

	procedure Free is
		new Ada.Unchecked_Deallocation (Object => T_Node, Name => T_BinaryTree);

   -- TODO
	procedure initializeBinaryTree(ABR: out T_BinaryTree) is
	begin
		ABR := Null;
	end initializeBinaryTree;

   -- TODO
	function isEmpty (ABR : T_BinaryTree) return Boolean is
	begin
		return ABR = Null;
	end isEmpty;

   -- TODO
	function getSize (ABR : in T_BinaryTree) return Integer is
	begin
		if ABR = null then
         return 0;
      else
         return 1 + getSize(ABR.all.Left) + getSize (ABR.all.Right);
      end if;
	end getSize;

   -- TODO
   function isPresent (ABR: in T_BinaryTree; Key: Integer) return boolean is
   begin
      if isEmpty (ABR) then
         return False;
      elsif ABR.all.key = Key then
         return True;
      else
         return isPresent(ABR.all.Left, Key) or else isPresent (ABR.all.Right, Key);
      end if;
   end isPresent;

   function getTree (ABR : in T_BinaryTree; Key : in Integer) return T_BinaryTree is
   begin
      return Null;
   end getTree;

	procedure setData (ABR : in out T_BinaryTree ; Key : in Integer ; Data : in T_Data) is
      Tree : T_BinaryTree;
	begin
      Tree := getTree(ABR, Key);
      Tree.all.Data := Data;
   end setData;

   function getData (ABR : in T_BinaryTree ; Key : in Integer) return T_Data is
	begin
		return getTree(ABR, Key).all.Data;
	end getData;

   procedure addNode (ABR : in out T_BinaryTree; Key : in Integer; Node : in T_Node; Position : in T_Position) is
      Tree : T_BinaryTree;
   begin
      if ABR.all.Key /= Key then
         Tree := getTree(ABR, Key);
      else
         Tree := ABR;
      end if;

      case Position is
         when RIGHT =>
            Tree.all.Right.all := Node;
         when LEFT =>
            Tree.all.Left.all := Node;
      end case;
   end addNode;

   -- TODO
   procedure deleteNode (ABR : in out T_BinaryTree ; Key : in Integer) is
      Tree : T_BinaryTree;
   begin
      if ABR.all.Key /= Key then
         Tree := getTree(ABR, Key);
      else
         Tree := ABR;
      end if;

		Free(Tree);
	end deleteNode;

   -- TODO
   procedure deleteNodeRecursive (ABR : in out T_BinaryTree ; Key : in Integer) is
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
         Put(ABR.all.Key);
         show(ABR.all.Right);
         show(ABR.all.Left);
      end if;
   end show;

end BinaryTree;
