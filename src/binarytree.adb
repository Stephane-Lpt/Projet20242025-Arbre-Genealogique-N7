with Ada.Text_IO;            use Ada.Text_IO;
with Ada.Integer_Text_IO;    use Ada.Integer_Text_IO;
with Ada.Unchecked_Deallocation;
with Utils; use Utils;

package body BinaryTree is

	procedure Free is
		new Ada.Unchecked_Deallocation (Object => T_Node, Name => T_BinaryTree);

   -- Initialize empty tree
	procedure initTree(ABR: out T_BinaryTree) is
	begin
		ABR := Null;
   end initTree;

   -- Initialize root node
   procedure initRoot(ABR: out T_BinaryTree; Key : in Integer; Element : in T_Element) is
   begin
      ABR := new T_Node'(Key, Element, Null, Null);
   end initRoot;

   -- Verify if ABR is empty (null)
	function isEmpty (ABR : in T_BinaryTree) return Boolean is
	begin
		return ABR = Null;
	end isEmpty;

   -- Verify if Element is present in ABR
   function isPresent (ABR: in T_BinaryTree; Key : in Integer) return boolean is
   begin
      if isEmpty (ABR) then
         return False;
      elsif getKey(ABR) = Key then
         return True;
      else
         return isPresent(getLeftChild(ABR), Key) or else isPresent (getRightChild(ABR), Key);
      end if;
   end isPresent;

   -- Get the size (number of elements) of ABR
	function getSize (ABR : in T_BinaryTree) return Integer is
	begin
		if isEmpty(ABR) then
         return 0;
      else
         return 1 + getSize(getLeftChild(ABR)) + getSize (getRightChild(ABR));
      end if;
	end getSize;

   -- Search Tree by Element
   function getNode (ABR : in T_BinaryTree; Key : in Integer) return T_BinaryTree is
      TempResult : T_BinaryTree;
   begin
      if isEmpty(ABR) then
         return null;
      elsif getKey(ABR) = Key then
         return ABR;
      else
         TempResult := getNode(getLeftChild(ABR), Key);

         if TempResult /= Null then
            return TempResult;
         else
            return getNode (getRightChild(ABR), Key);
         end if;
      end if;
   end getNode;

   -- Add a node to the tree (to the left or the right)
   procedure addNode (ABR : in out T_BinaryTree; NewNode : in T_BinaryTree; TargetKey : in Integer; Position : in T_Position) is
      TempTree : T_BinaryTree;
   begin
      if ABR.all.Key /= TargetKey then
         TempTree := getNode(ABR, TargetKey);
      else
         TempTree := ABR;
      end if;

      case Position is
         when ROOT =>
            Null;
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
         Tree := getNode(ABR, Key);
      else
         Tree := ABR;
      end if;

		Free(Tree);
	end deleteNode;

   procedure deleteNodeRecursive (ABR : in out T_BinaryTree; Key : in Integer) is
      childNode : T_BinaryTree;
      Stop_Flag : Boolean := False;
      Result: T_BinaryTree;
      -- Callback vérifie si le noeud courant est l'enfant du noeud à supprimer (génération -1)
      procedure getNodeChild (ABR : in out T_BinaryTree; Stop : in out Boolean; Result : in out T_BinaryTree) is
         LeftChild: T_BinaryTree;
         RightChild: T_BinaryTree;
         nodeToDelete: T_BinaryTree := getNode(ABR, Key);
      begin
         if ABR = null then
            Stop := True;
         elsif getLeftChild(ABR) = nodeToDelete then
            LeftChild := getLeftChild(ABR); -- Copie du noeud enfant
            setLeftChild(ABR, null);
            ABR := LeftChild;
            Stop := True;
         elsif getRightChild(ABR) = nodeToDelete then
            RightChild := getRightChild(ABR); -- Copie du noeud enfant
            setRightChild(ABR, null);
            ABR := RightChild;
            Stop := True;
         else
            getNodeChild (LeftChild, Stop_Flag, Result); -- Parcours du sous-arbre gauche
            getNodeChild (RightChild, Stop_Flag, Result); -- Parcours du sous-arbre droit
         end if;   
      end getNodeChild;

      -- Callback vérifie si le noeud courant est l'enfant du noeud à supprimer (génération -1)         
      procedure deleteSubTreeWithoutOriginalPointer (ABR : in out T_BinaryTree; Stop : in out Boolean; Result: in out T_BinaryTree) is
         LeftChild: T_BinaryTree;
         RightChild: T_BinaryTree;
      begin
         if not isEmpty(ABR) then
            LeftChild := getLeftChild(ABR);
            if not isEmpty(LeftChild)  then
               deleteSubTreeWithoutOriginalPointer(LeftChild, Stop, Result);
            end if;

            RightChild := getRightChild(ABR);
            if not isEmpty(RightChild) then
               deleteSubTreeWithoutOriginalPointer(RightChild, Stop, Result);
            end if;
            
            Free(ABR);
         end if;   
      end deleteSubTreeWithoutOriginalPointer;
      
	begin
      traverseTreeAndApply(ABR, getNodeChild'Access, Stop_Flag, Result);
      traverseTreeAndApply(ABR, deleteSubTreeWithoutOriginalPointer'Access, Stop_Flag, Result);
	end deleteNodeRecursive;

   -- TODO
   procedure clean (ABR : in out T_BinaryTree) is
   begin
      Null;
   end clean;

   procedure showTree (ABR : in T_BinaryTree; PropToShow : in T_PropToShow := Keys; Depth : in Integer := 0; Position : in T_Position := ROOT) is
   begin
      if not isEmpty (ABR) then
         if PropToShow = Keys then
            Put_Line(getIndent(Depth) & getBinaryTreePrefix(Position) & Integer'Image(ABR.all.Key));
         else
            Put_Generic(ABR.all.Element, ABR.all.Key, Depth, Position);
         end if;
         showTree(ABR.all.Right, PropToShow, Depth + 1, RIGHT);
         showTree(ABR.all.Left, PropToShow, Depth + 1, LEFT);
      end if;
   end showTree;

   procedure Display_Tree (ABR : T_BinaryTree; Depth : Integer := 0) is
   Indentation : String(1 .. Depth * 2); -- Crée une chaîne d'indentation
   begin
      -- Générer l'indentation
      for I in 1 .. Depth * 2 loop
         Indentation(I) := ' ';
      end loop;

      if not isEmpty(ABR) then
         -- Afficher la clé du noeud avec l'indentation pour la profondeur
         Put_Line(Indentation & "Node Key: " & Integer'Image(getKey(ABR)));

         -- Afficher le lien vers l'enfant gauche avec flèche
         if not isEmpty(getLeftChild(ABR)) then
            Put_Line(Indentation & "  |-- Link to Left Child --> " & Integer'Image(getKey(getLeftChild(ABR))));
         else
            Put_Line(Indentation & "  |-- No Left Child");
         end if;

         -- Afficher le lien vers l'enfant droit avec flèche
         if not isEmpty(getRightChild(ABR)) then
            Put_Line(Indentation & "  |-- Link to Right Child --> " & Integer'Image(getKey(getRightChild(ABR))));
         else
            Put_Line(Indentation & "  |-- No Right Child");
         end if;

         -- Récursion pour afficher les sous-arbres
         Display_Tree(getLeftChild(ABR), Depth + 1);
         Display_Tree(getRightChild(ABR), Depth + 1);
      end if;
   end Display_Tree;



   procedure traverseTreeAndApply (ABR : in out T_BinaryTree; 
                              ActionCallback : not null access procedure (ABR : in out T_BinaryTree; Stop : in out Boolean; Result: in out T_BinaryTree); 
                              Stop : in out Boolean;
                              Result: in out T_BinaryTree) is
   begin
      -- Si l'arbre est vide ou si le flag Stop est à True, on arrête immédiatement
      if ABR = null or else Stop then
         return;
      end if;

      -- Appliquer la fonction de traitement sur le nœud courant
      ActionCallback(ABR, Stop, Result);

      -- Si le parcours doit s'arrêter, on ne continue pas
      if Stop then
         return;
      end if;

      -- Parcours du sous-arbre gauche si présent et si le flag Stop n'est pas à True
      if ABR.all.Left /= null then
         traverseTreeAndApply(ABR.all.Left, ActionCallback, Stop, Result);
      end if;

      -- Parcours du sous-arbre droit si présent et si le flag Stop n'est pas à True
      if ABR.all.Right /= null then
         traverseTreeAndApply(ABR.all.Right, ActionCallback, Stop, Result);
      end if;
   end traverseTreeAndApply;

   function getKey (ABR : T_BinaryTree) return Integer is
   begin
      return ABR.Key;
   end getKey;

   function getLeftChild (ABR : T_BinaryTree) return T_BinaryTree is
   begin
      return ABR.Left;
   end getLeftChild;

   procedure setLeftChild(ABR : in out T_BinaryTree; Child : T_BinaryTree) is
   begin
      ABR.Left := Child;
   end setLeftChild;


   function getRightChild (ABR : T_BinaryTree) return T_BinaryTree is
   begin
      return ABR.Right;
   end getRightChild;

   procedure setRightChild(ABR : in out T_BinaryTree; Child : T_BinaryTree) is
   begin
      ABR.Right := Child; 
   end setRightChild;


   


end BinaryTree;
