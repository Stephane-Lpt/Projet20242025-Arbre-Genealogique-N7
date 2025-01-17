with Utils; use Utils;

generic
   type T_Element is private;
   with procedure Put_Generic(Element : in T_Element; Key : in Integer; Depth : Integer := 0; Position : in T_Position := ROOT);
      
package BinaryTree is

   type T_BinaryTree is limited private;
   type T_PropToShow is (Keys, Elements);

   --Present_Key_Exception : exception;      -- une clé est déjà présente dans un ABR
   --Absent_Key_Exception  : exception;      -- une clé est absente d'un ABR

   -- Initialize empty tree
   procedure initTree (ABR : out T_BinaryTree) with
     Post => isEmpty (ABR);
   
   -- Initialize root node
   procedure initRoot(ABR : in out T_BinaryTree; Key : in Integer; Element : in T_Element);

   -- Verify if ABR is empty (null)
   function isEmpty (ABR : T_BinaryTree) return Boolean;

   -- Verify if Element is present in ABR
   function isPresent (ABR : in T_BinaryTree; Key : in Integer) return Boolean;

   -- Get the size (number of elements) of ABR
   function getSize (ABR : in T_BinaryTree) return Integer with
     Post => getSize'Result >= 0 and (getSize'Result = 0) = isEmpty (ABR);

   -- Search Tree by Element
   function getNode (ABR: in T_BinaryTree; Key : in Integer) return T_BinaryTree;

   -- Add a node to the tree (to the left or the right)
   procedure addNode (ABR : in out T_BinaryTree; NewNode : in T_BinaryTree; TargetKey : in Integer; Position : in T_Position) with
     Pre => isPresent(ABR, TargetKey) and Position /= ROOT;
     --Pre => isPresent(ABR, TargetElement),
     --Post =>
      --(Position = LEFT and getTree(ABR, TargetElement).all.Left.all.Element = NewElement) or
      --(Position = RIGHT and getTree(ABR, TargetElement).all.Right.all.Element = NewElement);

   -- Delete a node element
   procedure deleteNode (ABR : in out T_BinaryTree; Key : in Integer) with
     Post => getSize (ABR) = getSize (ABR)'Old - 1 and not isPresent (ABR, Key);

   -- Delete a node element and all his children
   procedure deleteNodeRecursive
     (ABR : in out T_BinaryTree; Key : in Integer) with
     Pre => isPresent (ABR, Key), 
     Post => getSize(ABR) = getSize (ABR)'Old - getSize(getNode(ABR, Key))'Old and not isPresent (ABR, Key);

   -- Delete all elements in Tree
   procedure clean (ABR : in out T_BinaryTree) with
     Post => isEmpty (ABR);

   -- Show binary tree (parcours infixe)
   procedure showTree (ABR : in T_BinaryTree; PropToShow : T_PropToShow := Keys; Depth : Integer := 0; Position : T_Position := ROOT);

private
   
   type T_Node;
   type T_BinaryTree is access T_Node;
   
   type T_Node is record
      Key : Integer;
      Element : T_Element;
      Left : T_BinaryTree;
      Right  : T_BinaryTree;
   end record;

end BinaryTree;
