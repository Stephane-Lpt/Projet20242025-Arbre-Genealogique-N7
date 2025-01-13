-- with BinaryTree; use BinaryTree;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with BinaryTree;

procedure TestBinaryTree is
   -- ùTree : T_BinaryTree;
   package StringBinaryTree is
     new BinaryTree(T_Data => Integer);
   use StringBinaryTree;

begin
   Put("Hello");
end TestBinaryTree;
