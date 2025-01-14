generic
   type T_Element is private;
   procedure Put_Generic is private;

package BinaryTree is

   type T_BinaryTree is limited private;
   
   type T_Position is (LEFT, RIGHT);

   Present_Key_Exception : exception;      -- une clé est déjà présente dans un ABR
   Absent_Key_Exception  : exception;      -- une clé est absente d'un ABR

   -- Initialize empty tree
   procedure initTree (ABR : out T_BinaryTree) with
     Post => isEmpty (ABR);
   
   -- Initialize root node
   procedure setRootNode (ABR : in out T_BinaryTree; Element : in T_Element);

   -- Verify if ABR is empty (null)
   function isEmpty (ABR : T_BinaryTree) return Boolean;

   -- Verify if Element is present in ABR
   function isPresent (ABR : in T_BinaryTree; Element : in T_Element) return Boolean;

   -- Get the size (number of elements) of ABR
   function getSize (ABR : in T_BinaryTree) return Integer with
     Post => getSize'Result >= 0 and (getSize'Result = 0) = isEmpty (ABR);

   -- Search Tree by Element
   function getTree (ABR: in T_BinaryTree; Element : in T_Element) return T_BinaryTree;

   -- Add a node to the tree (to the left or the right)
   procedure addNode (ABR : in out T_BinaryTree; NewElement : in T_Element; TargetElement : in T_Element; Position : in T_Position) with
     Pre => isPresent(ABR, TargetElement);
     --Pre => isPresent(ABR, TargetElement),
     --Post =>
      --(Position = LEFT and getTree(ABR, TargetElement).all.Left.all.Element = NewElement) or
      --(Position = RIGHT and getTree(ABR, TargetElement).all.Right.all.Element = NewElement);

   -- Delete a node element
   procedure deleteNode (ABR : in out T_BinaryTree; Element : in T_Element) with
     Post => getSize (ABR) = getSize (ABR)'Old - 1 and not isPresent (ABR, Element);

   -- Delete a node element and all his children
   procedure deleteNodeRecursive
     (ABR : in out T_BinaryTree; Element : in T_Element) with
     Pre => isPresent (ABR, Element), 
     Post => getSize(ABR) = getSize (ABR)'Old - getSize(getTree(ABR, Element))'Old and not isPresent (ABR, Element);

   -- Delete all elements in Tree
   procedure clean (ABR : in out T_BinaryTree) with
     Post => isEmpty (ABR);

   -- Show binary tree (parcours infixe)
   procedure show (ABR : in T_BinaryTree);

private
   
   type T_Node;
   type T_BinaryTree is access T_Node;
   
   type T_Node is record
      Element : T_Element;
      Left : T_BinaryTree;
      Right  : T_BinaryTree;
   end record;

end BinaryTree;
