generic
   type T_Data is private;
   type T_Key is private;

package BinaryTree is

   type T_BinaryTree is private;
   type T_Node is private;

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
     (ABR : in out T_BinaryTree; Key : in T_Key; Data : in T_Data) with
     Post => getData (ABR, Key) = Data;              -- donnée mise à jour

   -- Supprimer la donnée associée à la clé Clé dans l'ABR ABR.
   -- Exception : Key_Absente_Exception si Clé n'est pas utilisée dans l'ABR
   procedure deleteNode (ABR : in out T_BinaryTree; Key : in T_Key) with
     Post => getSize (ABR) = Taille (ABR)'Old - 1 and not isPresent (ABR, Key);

   -- 6. Supprimer, pour un arbre, un nœud et ses ancêtres.
   procedure deleteNodeRecursive
     (ABR : in out T_BinaryTree; Id_Node : in Integer) with
     Pre => isPresent (ABR, Id_Node), -- NODE Id_Node IS PRESENT IN FAMILY TREE
     Post =>
      isPresent (ABR, Id_Node) =
      false; --TODO: DEFINE A CORRECT POST CONDITION TO DESCRIBE THE BEHAVIOUR (ALL ANCESTORS BEING DELETED)

   -- Obtenir la donnée associée à la clé Key dans l'ABR ABR.
   -- Exception : Key_Absente_Exception si Clé n'est pas utilisée dans l'ABR
   function getData (ABR : in T_BinaryTree; Key : in T_Key) return T_Data;

   function isPresent (ABR : in T_BinaryTree; Key : in T_Key) return Boolean;

   -- Supprimer tous les éléments d'un ABR.
   -- Doit être utilisée dès qu'on sait qu'un ABR ne sera plus utilisé.
   procedure clean (ABR : in out T_BinaryTree) with
     Post => isEmpty (ABR);

   -- Afficher un ABR ABR dans l'ordre croissant des clés (parcours infixe)
   procedure show (ABR : in T_BinaryTree);

private

   type T_BinaryTree is access T_Node;
   type T_Node is record
      Key               : T_Key;
      Data              : T_Data;
      Left : T_BinaryTree;
      Right  : T_BinaryTree;
   end record;

end BinaryTree;
