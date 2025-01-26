with Ada.Text_IO;         use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Unchecked_Deallocation;
with utils;               use utils;

package body BinaryTree is

   procedure Free is new Ada.Unchecked_Deallocation
     (Object => T_Node, Name => T_BinaryTree);

   -- Initialize empty tree
   procedure initTree (ABR : out T_BinaryTree) is
   begin
      ABR := null;
   end initTree;

   -- Initialize root node
   procedure initRoot
     (ABR : out T_BinaryTree; Key : in Integer; Element : in T_Element)
   is
   begin
      ABR := new T_Node'(Key, Element, null, null);
   end initRoot;

   -- Verify if ABR is empty (null)
   function isEmpty (ABR : in T_BinaryTree) return Boolean is
   begin
      return ABR = null;
   end isEmpty;

   -- Verify if Element is present in ABR
   function isPresent (ABR : in T_BinaryTree; Key : in Integer) return Boolean
   is
   begin
      if isEmpty (ABR) then
         return False;
      elsif ABR.all.Key = Key then
         return True;
      else
         return
           isPresent (ABR.all.Left, Key)
           or else isPresent (ABR.all.Right, Key);
      end if;
   end isPresent;

   -- Get the size (number of elements) of ABR
   function getSize (ABR : in T_BinaryTree) return Integer is
   begin
      if isEmpty (ABR) then
         return 0;
      else
         return 1 + getSize (ABR.all.Left) + getSize (ABR.all.Right);
      end if;
   end getSize;

   -- Search Tree by Element
   function getNode
     (ABR : in T_BinaryTree; Key : in Integer) return T_BinaryTree
   is
      TempResult : T_BinaryTree;
   begin
      if isEmpty (ABR) then
         return null;
      elsif ABR.all.Key = Key then
         return ABR;
      else
         TempResult := getNode (ABR.all.Left, Key);

         if TempResult /= null then
            return TempResult;
         else
            return getNode (ABR.all.Right, Key);
         end if;
      end if;
   end getNode;

   -- Add a node to the tree (to the left or the right)
   procedure addNode
     (ABR       : in out T_BinaryTree; NewNode : in T_BinaryTree;
      TargetKey : in     Integer; Position : in T_Position)
   is
      TempTree : T_BinaryTree;
   begin
      if ABR.all.Key /= TargetKey then
         TempTree := getNode (ABR, TargetKey);
      else
         TempTree := ABR;
      end if;

      case Position is
         when ROOT =>
            null;
         when RIGHT =>
            --TempTree.all.Right := new T_Node'(NewElement, null, null);
            TempTree.all.Right := NewNode;
         when LEFT =>
            --TempTree.all.Left := new T_Node'(NewElement, null, null);
            TempTree.all.Left := NewNode;
      end case;
   end addNode;

   -- TODO
   procedure deleteNode (ABR : in out T_BinaryTree; Key : in Integer) is
      Tree : T_BinaryTree;
   begin
      if ABR.all.Key /= Key then
         Tree := getNode (ABR, Key);
      else
         Tree := ABR;
      end if;

      Free (Tree);
   end deleteNode;

   procedure deleteNodeRecursive (ABR : in out T_BinaryTree; Key : in Integer)
   is
      Stop_Flag : Boolean := False;
      ABRParent : T_BinaryTree;
      -- Procédure pour supprimer tous les nœuds descendants d'un nœud
      procedure deleteSubTree
        (ABR  : in out T_BinaryTree; Parent : in out T_BinaryTree;
         Stop : in out Boolean)
      is
      begin

         -- Si l'arbre est vide ou si le parcours doit être arrêté, rien à faire
         if ABR = null or else Stop then
            return;
         end if;

         -- Si la clé du nœud courant correspond à la clé recherchée
         if ABR.all.Key = Key then
            -- Mettre à jour le pointeur du parent pour supprimer la référence au sous-arbre
            if Parent /= null then
               if Parent.all.Left = ABR then
                  Parent.all.Left :=
                    null;  -- Mise à null si c'est le sous-arbre gauche
               elsif Parent.all.Right = ABR then
                  Parent.all.Right :=
                    null;  -- Mise à null si c'est le sous-arbre droit
               end if;
            end if;

            -- Supprimer le nœud courant (libère la mémoire) et ses descendants
            Free (ABR);
            Stop := True;  -- On arrête le parcours après suppression
            return;
         end if;

         -- Parcours du sous-arbre gauche
         if ABR.all.Left /= null then
            traverseTreeAndApply
              (ABR.all.Left, ABR, deleteSubTree'Access, Stop);
         end if;

         -- Parcours du sous-arbre droit
         if ABR.all.Right /= null then
            traverseTreeAndApply
              (ABR.all.Right, ABR, deleteSubTree'Access, Stop);
         end if;
      end deleteSubTree;

   begin

      if isPresent (ABR, Key) = False then
         raise Absent_Key_Exception; -- La clé n'est pas présente dans l'arbre
      end if;

      initTree (ABRParent);
      -- Initialiser le parent à null car il n'y a pas de parent au niveau de la racine
      traverseTreeAndApply (ABR, ABRParent, deleteSubTree'Access, Stop_Flag);
   end deleteNodeRecursive;

   -- Nettoie l'arbre en supprimant tous les noeuds et ainsi libérant la mémoire
   procedure clean (ABR : in out T_BinaryTree) is
   begin
      if not isEmpty (ABR) then
         deleteNodeRecursive
           (ABR, ABR.Key); -- Supprime récursivement à partir de la racine
         ABR := null;
      end if;
   end clean;

   procedure showTree
     (ABR       : in T_BinaryTree; PropToShow : in T_PropToShow := Keys;
      Depth     : in Integer := 0; Position : in T_Position := ROOT;
      Verbosity : in Integer := 1)
   is
   begin
      if not isEmpty (ABR) then
         if PropToShow = Keys then
            Put_Line
              (GetIndent (Depth) & GetBinaryTreePrefix (Position) &
               Integer'Image (ABR.all.Key));
         else
            PutGeneric
              (ABR.all.Element, ABR.all.Key, Depth, Position, Verbosity);
         end if;
         showTree (ABR.all.Left, PropToShow, Depth + 1, LEFT, Verbosity);
         showTree (ABR.all.Right, PropToShow, Depth + 1, RIGHT, Verbosity);
      end if;
   end showTree;

   procedure traverseTreeAndApply
     (ABR            : in out T_BinaryTree; Parent : in out T_BinaryTree;
      ActionCallback :        not null access procedure
        (ABR  : in out T_BinaryTree; Parent : in out T_BinaryTree;
         Stop : in out Boolean);
      Stop           : in out Boolean)
   is
   begin

      if ABR = null then
         return;
      end if;

      -- Appliquer la fonction de traitement sur le nœud courant
      ActionCallback (ABR, Parent, Stop);

      -- Si l'arbre est vide ou si le flag Stop est à True, on arrête immédiatement
      if Stop then
         return;
      end if;

      -- Parcours du sous-arbre gauche si présent et si le flag Stop n'est pas à True
      traverseTreeAndApply (ABR.all.Left, ABR, ActionCallback, Stop);

      -- Parcours du sous-arbre droit si présent et si le flag Stop n'est pas à True
      traverseTreeAndApply (ABR.all.Right, ABR, ActionCallback, Stop);
   end traverseTreeAndApply;

   function getKey (ABR : T_BinaryTree) return Integer is
   begin
      return ABR.Key;
   end getKey;

   function getLeftChild (ABR : T_BinaryTree) return T_BinaryTree is
   begin
      return ABR.Left;
   end getLeftChild;

   procedure setLeftChild (ABR : in out T_BinaryTree; Child : T_BinaryTree) is
   begin
      ABR.Left := Child;
   end setLeftChild;

   function getRightChild (ABR : T_BinaryTree) return T_BinaryTree is
   begin
      return ABR.Right;
   end getRightChild;

   procedure setRightChild (ABR : in out T_BinaryTree; Child : T_BinaryTree) is
   begin
      ABR.Right := Child;
   end setRightChild;

   function getElement (ABR : T_BinaryTree) return T_Element is
   begin
      return ABR.Element;
   end getElement;

end BinaryTree;
