-- with BinaryTree; use BinaryTree;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with BinaryTree;

procedure TestBinaryTree is
   -- �Tree : T_BinaryTree;
   package StringBinaryTree is
     new BinaryTree(T_Data => Integer);
   use StringBinaryTree;

   Tree : T_BinaryTree;

begin
   Put("Hello");

   -- initializeBinaryTree(Tree);

   Tree := new T_Node'(Key => 4, Data => 1, Left => Null, Right => Null);


end TestBinaryTree;
