with Ada.Text_IO; use Ada.Text_IO;
with utils;       use utils;
with BinaryTree;

procedure playground is
    procedure PutDataWithKey
       (Element  : in Character; Key : in Integer; Depth : in Integer := 0;
        Position : in T_Position := ROOT)
    is
    begin
        Put_Line
           (getIndent (Depth) & getBinaryTreePrefix (Position) &
            Integer'Image (Key) & ":" & Element);
    end PutDataWithKey;

    package StringBinaryTree is new BinaryTree
       (Put_Generic => PutDataWithKey, T_Element => Character);
    use StringBinaryTree;

    Root      : T_BinaryTree;
    Tree1     : T_BinaryTree;
    Tree2     : T_BinaryTree;
    Tree3     : T_BinaryTree;
    Tree4     : T_BinaryTree;
    Tree5     : T_BinaryTree;
    Tree6     : T_BinaryTree;
    Tree7     : T_BinaryTree;
    Tree8     : T_BinaryTree;
    EmptyTree : T_BinaryTree;

begin
    -- INITIALIZATION
    initRoot (Root, 1, 'A');
    initRoot (Tree1, 2, 'B');
    initRoot (Tree2, 3, 'C');
    initRoot (Tree3, 4, 'D');
    initRoot (Tree4, 5, 'E');
    initRoot (Tree5, 6, 'F');
    initRoot (Tree6, 7, 'G');
    initRoot (Tree7, 8, 'H');
    initRoot (Tree8, 9, 'I');
    initTree (EmptyTree);

    addNode (ABR => Root, NewNode => Tree1, TargetKey => 1, Position => LEFT);
    addNode (ABR => Root, NewNode => Tree2, TargetKey => 1, Position => RIGHT);
    addNode
       (ABR => Tree1, NewNode => Tree3, TargetKey => 2, Position => RIGHT);
    addNode (ABR => Tree1, NewNode => Tree4, TargetKey => 2, Position => LEFT);
    addNode
       (ABR => Tree2, NewNode => Tree5, TargetKey => 3, Position => RIGHT);
    addNode (ABR => Tree2, NewNode => Tree6, TargetKey => 3, Position => LEFT);
    addNode
       (ABR => Tree3, NewNode => Tree7, TargetKey => 4, Position => RIGHT);
    addNode (ABR => Tree6, NewNode => Tree8, TargetKey => 7, Position => LEFT);

    --showTree(Root, PropToShow => Elements);

    -- TESTS --
    -- isEmpty function --
    pragma Assert (isEmpty (EmptyTree));
end playground;
