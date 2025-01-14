with Ada.Text_IO; use Ada.Text_IO;
with BinaryTree;

procedure TestBinaryTree is

   procedure Put_Integer(Int : in Integer) is
   begin
      Put_Line(Int);
   end;

   package IntegerBinaryTree is
      new BinaryTree(Put_Generic => Put_Integer,T_Element => Integer, ); -- Instantiate BinaryTree with Integer data
   use IntegerBinaryTree;

   Fils : T_BinaryTree;
   Parent1 : T_BinaryTree;
   Parent2 : T_BinaryTree;

begin
   Put_Line("Hello");

   -- Initialize the tree (it starts as empty)
   initTree(Fils);
   initTree(Parent1);
   initTree(Parent2);

   setRootNode(Fils, 1);

end TestBinaryTree;
