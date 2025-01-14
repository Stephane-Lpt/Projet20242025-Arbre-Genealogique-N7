with Ada.Text_IO; use Ada.Text_IO;
with BinaryTree;

procedure TestBinaryTree is
   package IntegerBinaryTree is
      new BinaryTree(T_Data => Integer); -- Instantiate BinaryTree with Integer data
   use IntegerBinaryTree;

   Fils : T_BinaryTree;
   Parent1 : T_BinaryTree;
   Parent2 : T_BinaryTree;

begin
   Put_Line("Hello");

   -- Initialize the tree (it starts as empty)
   initializeTree(Fils);
   initializeTree(Parent1);
   initializeTree(Parent2);

   setRootNode(Fils, 1, 1);

end TestBinaryTree;
