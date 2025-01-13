with Ada.Text_IO; use Ada.Text_IO;
with BinaryTree;

procedure TestBinaryTree is
   package IntegerBinaryTree is
      new BinaryTree(T_Data => Integer); -- Instantiate BinaryTree with Integer data
   use IntegerBinaryTree;

   Tree : T_BinaryTree; -- Declare a binary tree
   RootNode : T_Node;

begin
   Put_Line("Hello");

   -- Initialize the tree (it starts as empty)
   initializeBinaryTree(Tree);

end TestBinaryTree;
