with Ada.Text_IO;            use Ada.Text_IO;
with Ada.Integer_Text_IO;    use Ada.Integer_Text_IO;
with Ada.Unchecked_Deallocation;

package body BinaryTree is

	procedure Free is
		new Ada.Unchecked_Deallocation (Object => T_Node, Name => T_BinaryTree);

   -- TODO
	procedure initializeBinaryTree(ABR: out T_BinaryTree) is
	begin
		Null;	-- TODO : à changer
	end initializeBinaryTree;

   -- TODO
	function isEmpty (ABR : T_BinaryTree) return Boolean is
	begin
		return False;	-- TODO : à changer
	end isEmpty;

   -- TODO
	function getSize (ABR : in T_BinaryTree) return Integer is
	begin
		return 0;	-- TODO : à changer
	end getSize;

   -- TODO
   function isPresent (ABR: in T_BinaryTree; Key: T_Key) return boolean is
   begin
      return False;
   end isPresent;

	procedure setData (ABR : in out T_BinaryTree ; Key : in T_Key ; Data : in T_Data) is
	begin
		Null;	-- TODO : à changer
	end insertData;

   -- TODO
   procedure getData (ABR : in out T_BinaryTree ; Key : in T_Key) is
	begin
		Null;	-- TODO : à changer
	end deleteNode;

   -- TODO
   procedure deleteNode (ABR : in out T_BinaryTree ; Key : in T_Key) is
	begin
		Null;	-- TODO : à changer
	end deleteNode;

   -- TODO
   procedure deleteNodeRecursive (ABR : in out T_BinaryTree ; Key : in T_Key) is
	begin
		Null;	-- TODO : à changer
	end deleteNode;

   -- TODO
   procedure clean (ABR : in out T_BinaryTree) is
   begin
      Null;
   end clean;

   -- TODO
   procedure show (ABR : in T_BinaryTree) is
   begin
      Null;
   end show;

end BinaryTree;
