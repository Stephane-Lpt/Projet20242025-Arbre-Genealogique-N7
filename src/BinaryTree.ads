generic
   type T_Data is private;

package BinaryTree is

   type T_BinaryTree is private;
   type T_Node is record
      Key               : Integer;
      Data              : T_Data;
      Left : T_BinaryTree;
      Right  : T_BinaryTree;
   end record;
   
   type T_Position is (LEFT, RIGHT);

   Present_Key_Exception : exception;      -- une clé est déjà présente dans un ABR
   Absent_Key_Exception  : exception;      -- une clé est absente d'un ABR

   -- Initialiser un ABR ABR.  L'ABR est vide.
   procedure initializeBinaryTree (ABR : out T_BinaryTree) with
     Post => isEmpty (ABR);

   -- Est-ce qu'un ABR ABR est vide ?
   function isEmpty (ABR : T_BinaryTree) return Boolean;

   -- Obtenir le nombre d'éléments d'un ABR.
   function getSize (ABR : in T_BinaryTree) return Integer with
     Post => getSize'Result >= 0 and (getSize'Result = 0) = isEmpty (ABR);

   -- Modifier la donnée Donnée associée à la clé Clé dans l'ABR ABR.
   -- Exception : Key_Absente_Exception si Clé n'est pas utilisée dans l'ABR
   procedure setData
     (ABR : in out T_BinaryTree; Key : in Integer; Data : in T_Data) with
     Post => getData (ABR, Key) = Data;              -- donnée mise à jour
           
   -- Obtenir la donnée associée à la clé Key dans l'ABR ABR.
   -- Exception : Key_Absente_Exception si Clé n'est pas utilisée dans l'ABR
   function getData (ABR : in T_BinaryTree; Key : in Integer) return T_Data;

   procedure addNode (ABR : in out T_BinaryTree; Key : in Integer; Node : in T_Node; Position : in T_Position) with
     Pre => isPresent(ABR, Key);
     --Post =>
     --  (Position = LEFT and getTree(ABR, Key).all.Left = Node) or
     --  (Position = RIGHT and getTree(ABR, Key).all.Right = Node);

   function getTree (ABR: in T_BinaryTree; Key : in Integer) return T_BinaryTree;

   -- Supprimer la donnée associée à la clé Clé dans l'ABR ABR.
   -- Exception : Key_Absente_Exception si Clé n'est pas utilisée dans l'ABR
   procedure deleteNode (ABR : in out T_BinaryTree; Key : in Integer) with
     Post => getSize (ABR) = getSize (ABR)'Old - 1 and not isPresent (ABR, Key);

   -- 6. Supprimer, pour un arbre, un nœud et ses ancêtres.
   procedure deleteNodeRecursive
     (ABR : in out T_BinaryTree; Key : in Integer) with
     Pre => isPresent (ABR, Key), -- NODE Id_Node IS PRESENT IN FAMILY TREE
     Post =>
      isPresent (ABR, Key) =
      false; --TODO: DEFINE A CORRECT POST CONDITION TO DESCRIBE THE BEHAVIOUR (ALL ANCESTORS BEING DELETED)

   function isPresent (ABR : in T_BinaryTree; Key : in Integer) return Boolean;

   -- Supprimer tous les éléments d'un ABR.
   -- Doit être utilisée dès qu'on sait qu'un ABR ne sera plus utilisé.
   procedure clean (ABR : in out T_BinaryTree) with
     Post => isEmpty (ABR);

   -- Afficher un ABR ABR dans l'ordre croissant des clés (parcours infixe)
   procedure show (ABR : in T_BinaryTree);

private

   type T_BinaryTree is access T_Node;

end BinaryTree;
