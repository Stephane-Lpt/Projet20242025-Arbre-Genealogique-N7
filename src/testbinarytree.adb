with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO;
with Utils; use Utils;
with BinaryTree;

procedure TestBinaryTree is

    procedure Put_Integer (Int : in Integer; Depth : in Integer := 0; Position : in T_Position := ROOT) is
    begin
        Ada.Integer_Text_IO.Put (Int);
    end Put_Integer;

    package IntegerBinaryTree is new BinaryTree
       (Put_Generic => Put_Integer,
        T_Element   => Integer); -- Instantiate BinaryTree with Integer data
    use IntegerBinaryTree;

    Fils    : T_BinaryTree;
    Parent1 : T_BinaryTree;
    Parent2 : T_BinaryTree;
    GrandParent11    : T_BinaryTree;
    GrandParent12 : T_BinaryTree;
    GrandParent21    : T_BinaryTree;
    GrandParent22 : T_BinaryTree;
    GrandGrandParent221    : T_BinaryTree;
    GrandGrandParent112    : T_BinaryTree;

begin
    initRoot (Fils, 1, 10);
    initRoot (Parent1, 2, 20);
    initRoot (Parent2, 3, 30);
    initRoot (GrandParent11, 4, 40);
    initRoot (GrandParent12, 5, 50);
    initRoot (GrandParent21, 6, 60);
    initRoot (GrandParent22, 7, 70);
    initRoot (GrandGrandParent221, 8, 80);
    initRoot (GrandGrandParent112, 9, 90);


    addNode
       (ABR => Fils, NewNode => Parent1, TargetKey => 1, Position => LEFT);
    addNode
      (ABR => Fils, NewNode => Parent2, TargetKey => 1, Position => RIGHT);
    addNode
       (ABR => Parent1, NewNode => GrandParent11, TargetKey => 2, Position => RIGHT);
    addNode
       (ABR => Parent1, NewNode => GrandParent12, TargetKey => 2, Position => LEFT);
    addNode
       (ABR => Parent2, NewNode => GrandParent21, TargetKey => 3, Position => RIGHT);
    addNode
       (ABR => Parent2, NewNode => GrandParent22, TargetKey => 3, Position => LEFT);
    addNode
       (ABR => GrandParent22, NewNode => GrandGrandParent221, TargetKey => 7, Position => RIGHT);
    addNode
       (ABR => GrandParent11, NewNode => GrandGrandParent112, TargetKey => 4, Position => LEFT);

    showTree (Fils);
end TestBinaryTree;
