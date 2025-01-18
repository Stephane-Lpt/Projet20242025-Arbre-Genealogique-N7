with Ada.Text_IO; use Ada.Text_IO;
with Utils; use Utils;
with BinaryTree;

procedure TestBinaryTree is
    procedure PutInteger (Element : in Integer; Key : in Integer; Depth : in Integer := 0; Position : in T_Position := ROOT) is
    begin
      Put_Line(getIndent(Depth) & getBinaryTreePrefix(Position) & Integer'Image(Key) & ":" & Integer'Image(Element));
   end PutInteger;

   package IntegerBinaryTree is new BinaryTree
     (
      Put_Generic => PutInteger,
      T_Element   => Integer
     );
   use IntegerBinaryTree;

   -- TEST isEmpty --
   procedure TestIsEmpty is
      Tree1, Tree2 : T_BinaryTree;
   begin
      initTree(Tree1);

      initRoot(Tree2, 1, 1);

      pragma Assert (isEmpty(Tree1));
      pragma Assert (not isEmpty(Tree2));
   end TestIsEmpty;

   -- TEST addNode --
   procedure TestAddNode is
      RootTree, LeftTree, RightTree : T_BinaryTree;
   begin
      initRoot(RootTree, 1, 12);
      initRoot(LeftTree, 2, 82);
      initRoot(RightTree, 3, 23);

      addNode(RootTree, LeftTree, 1, LEFT);
      addNode(RootTree, RightTree, 1, RIGHT);

      -- not working!
      --pragma Assert(RootTree.all.Left.all = LeftTree);
      --pragma Assert(RootTree.all.Right.all = RightTree);
      --pragma Assert(RootTree.all.Left.Key = 2);
      --pragma Assert(RootTree.all.Right.Key = 3);
      --pragma Assert(RootTree.Left.all.Element = 82);
      --pragma Assert(RootTree.Right.all.Element = 23);

      -- TODO: add tests with ID's that aren't directly the ABR's root (need to implement getNode)
   end TestAddNode;

   -- TEST isPresent --
   procedure TestIsPresent is
      RootTree, Tree1, Tree2 : T_BinaryTree;
   begin
      initRoot(RootTree, 1, 12);
      initRoot(Tree1, 2, 97);
      initRoot(Tree2, 3, 78);

      addNode (RootTree, Tree1, 1, LEFT);
      addNode (Tree1, Tree2, 2, RIGHT);

      pragma Assert (isPresent (RootTree, 1));
      pragma Assert (isPresent (RootTree, 2));
      pragma Assert (isPresent (RootTree, 3));
      pragma Assert (not isPresent (RootTree, 4));
   end TestIsPresent;

   -- TEST getSize --
   procedure TestGetSize is
      Tree1, Tree2, Tree3, Tree4, Tree5 : T_BinaryTree;
   begin
      initTree (Tree1);

      pragma Assert (getSize (Tree1) = 0);

      initRoot(Tree1, 1, 12);
      initRoot(Tree2, 2, 97);
      initRoot(Tree3, 3, 78);
      initRoot(Tree4, 4, 34);
      initRoot(Tree5, 5, 90);

      pragma Assert (getSize (Tree1) = 1);

      addNode (Tree1, Tree2, 1, LEFT);
      addNode (Tree1, Tree3, 1, RIGHT);

      pragma Assert (getSize (Tree1) = 3);

      addNode (Tree2, Tree4, 2, LEFT);
      addNode (Tree3, Tree5, 3, RIGHT);

      pragma Assert (getSize (Tree1) = 5);
      pragma Assert (getSize (Tree2) = 2);
      pragma Assert (getSize (Tree5) = 1);
   end TestGetSize;

   procedure TestGetNode is
      Tree1, Tree2, Tree3, Tree4, Tree5, FoundTree : T_BinaryTree;
   begin
      initRoot(Tree1, 1, 10);
      initRoot(Tree2, 2, 20);
      initRoot(Tree3, 3, 30);
      initRoot(Tree4, 4, 40);
      initRoot(Tree5, 5, 50);

      addNode(Tree1, Tree2, 1, LEFT);
      addNode(Tree1, Tree3, 1, RIGHT);
      addNode(Tree3, Tree4, 3, LEFT);
      addNode(Tree4, Tree5, 4, RIGHT);

      --  showTree(Tree1);

      FoundTree := getNode(Tree1, 3);

      showTree(FoundTree);


   end TestGetNode;

begin
    TestIsEmpty;
    --TestAddNode;
    TestIsPresent;
   TestGetSize;
   TestGetNode;

end TestBinaryTree;

