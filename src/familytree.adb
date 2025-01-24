with Ada.Text_IO;         use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with utils;               use utils;

package body FamilyTree is

   procedure initFamilyTree(ABR : out T_FamilyTree) is
   begin
      initTree(ABR);
   end initFamilyTree;

   -- 1. Créer un arbre minimal contenant le seul nœud racine, sans père ni mère.
   procedure initChild(ABR : out T_BinaryTree; Key: in Integer; Person : in T_Person) is
   begin
      initRoot (ABR, Key, Person);
   end initChild;

   procedure addAncestor (ABR : in out T_FamilyTree; TargetKey : in Integer; Position : in T_Position; NewKey : in Integer; NewPerson: in T_Person) is
      NewNode : T_BinaryTree;
   begin
      initRoot(NewNode, NewKey, NewPerson);
      addNode (ABR, NewNode, TargetKey, Position);
   end addAncestor;

   procedure deleteAncestor (ABR : in out T_FamilyTree; TargetKey : in Integer) is
   begin
      deleteNodeRecursive (ABR, TargetKey);
   end deleteAncestor;

   -- Renvoie le nombre de générations dans un arbre donné
   function getGenerationsCount (ABR : in T_BinaryTree) return Integer is
      function Max_Depth(Node : T_BinaryTree) return Integer is
         Left_Depth  : Integer := 0;
         Right_Depth : Integer := 0;
      begin
         if isEmpty(Node) then
            return 0;
         else
            -- Calcul récursif des profondeurs gauche/droite
            Left_Depth  := Max_Depth(getLeftChild(Node));
            Right_Depth := Max_Depth(getRightChild(Node));
            
            -- Retourne la profondeur max + 1 (niveau courant)
            return 1 + Integer'Max(Left_Depth, Right_Depth);
         end if;
      end Max_Depth;

   begin
      return Max_Depth(ABR);
   end getGenerationsCount;

   -- 3. Obtenir le nombre d’ancêtres connus (lui compris) d’un individu donné
   function getAncestorsCount
   (ABR : in T_FamilyTree; Key : in Integer) return Integer 
   is
      Node : constant T_FamilyTree := getNode(ABR, Key);
   begin
      return getSize(Node); -- Utilise la fonction getSize existante du BinaryTree
   end getAncestorsCount;

   -- 4. Obtenir l’ensemble des ancêtres situés à une certaine génération d’un individu donné.
   function getAncestorsByGeneration (ABR : in T_FamilyTree; 
                                    Key : in Integer; 
                                    Generation : in Integer) return TreeVector.Vector is

      TargetABR : constant T_FamilyTree := getNode(ABR, Key);

      -- Fonction helper récursive pour collecter les ancêtres à la génération cible
      function Helper (ABR : T_FamilyTree; CurrentGen : Integer) return TreeVector.Vector is
         Result : TreeVector.Vector;
      begin
         if isEmpty(ABR) then
            return Result;  -- Fin de branche
         end if;

         -- Si on a atteint la génération cible
         if CurrentGen = Generation then
            Result.Append(ABR);  -- Ajouter le nœud courant
            return Result;
         end if;

         -- Explorer récursivement les deux parents avec génération+1
         declare
            LeftResult  : constant TreeVector.Vector := Helper(getLeftChild(ABR), CurrentGen + 1);
            RightResult : constant TreeVector.Vector := Helper(getRightChild(ABR), CurrentGen + 1);
         begin
            -- Fusionner les résultats
            for E of LeftResult loop
               Result.Append(E);
            end loop;
            for E of RightResult loop
               Result.Append(E);
            end loop;
         end;

         return Result;
      end Helper;

   begin
      -- Gestion des cas d'erreur
      if isEmpty(TargetABR) or Generation < 0 then
         return TreeVector.Empty_Vector;
      end if;

      -- Démarrer la récursion depuis le nœud cible (génération 0)
      return Helper(TargetABR, 0);
   end getAncestorsByGeneration;

   -- Afficher l’arbre.
   procedure showFamilyTree (ABR : in T_BinaryTree; Verbosity : in Integer := 1) is
   begin
      showTree (ABR => ABR, PropToShow => Elements, Verbosity => Verbosity);
   end showFamilyTree;

   -- 5. Afficher l’arbre à partir d’un nœud donné.
   procedure showFamilyTreeFromId (ABR : in T_BinaryTree; Key : in Integer; Verbosity : in Integer := 1) is
   begin
      showFamilyTree (ABR => getNode(ABR, Key), Verbosity => Verbosity);
   end showFamilyTreeFromId;

   -- 7. Obtenir l’ensemble des individus qui n’ont qu’un parent connu.
   function getSingleParentIndividuals (ABR : in T_BinaryTree; Key : in Integer) return TreeVector.Vector is
      SingleParentIndividuals : TreeVector.Vector;
      TargetNode : constant T_BinaryTree := getNode(ABR, Key);

      procedure TraverseAndCollect(Node : T_BinaryTree) is
         Left, Right : T_BinaryTree;
      begin
         if isEmpty(Node) then
            return;
         end if;

         Left := getLeftChild(Node);
         Right := getRightChild(Node);

         -- Vérifier si un seul parent est connu
         if (not isEmpty(Left) and isEmpty(Right)) or 
            (isEmpty(Left) and not isEmpty(Right)) 
         then
            SingleParentIndividuals.Append(Node);
         end if;

         -- Explorer récursivement les deux branches
         TraverseAndCollect(Left);
         TraverseAndCollect(Right);
      end TraverseAndCollect;

   begin
      if not isEmpty(TargetNode) then
         TraverseAndCollect(TargetNode);
      end if;

      return SingleParentIndividuals;
   end getSingleParentIndividuals;

   -- 8. Obtenir l’ensemble des individus dont les deux parents sont connus.
   function getDualParentIndividuals (ABR : in T_BinaryTree; Key : in Integer) return TreeVector.Vector is
      DualParentIndividuals : TreeVector.Vector;
      TargetNode : constant T_BinaryTree := getNode(ABR, Key);

      procedure TraverseAndCollect(Node : T_BinaryTree) is
         Left, Right : T_BinaryTree;
      begin
         if isEmpty(Node) then
            return;
         end if;

         Left := getLeftChild(Node);
         Right := getRightChild(Node);

         -- Vérifier si les deux parents sont connus
         if not isEmpty(Left) and not isEmpty(Right) then
            DualParentIndividuals.Append(Node);
         end if;

         -- Explorer récursivement les deux branches
         TraverseAndCollect(Left);
         TraverseAndCollect(Right);
      end TraverseAndCollect;

   begin
      if not isEmpty(TargetNode) then
         TraverseAndCollect(TargetNode);
      end if;

      return DualParentIndividuals;
   end getDualParentIndividuals;

   -- Getters
   --function 

   function getNode(ABR : in T_FamilyTree; Key : in Integer ) return T_FamilyTree is
   begin
      return Tree.getNode(ABR, Key);
   end getNode;

   function isEmpty (ABR : in T_FamilyTree) return Boolean is
   begin
      return Tree.isEmpty(ABR);
   end isEmpty;

   procedure clean (ABR : in out T_FamilyTree) is
   begin
      Tree.clean(ABR); -- Appel à la version générique
   end clean;

   function isPresent(ABR : in T_FamilyTree; Key : in Integer) return Boolean is
   begin
      return Tree.isPresent(ABR, Key);
   end isPresent;

   function getKey (ABR : T_FamilyTree) return Integer is
   begin
      return Tree.getKey(ABR);
   end getKey;

   -- Vector helpers
   function Length(Vector: TreeVector.Vector) return Integer is
   begin
      return Integer(Vector.Length);
   end Length;

   function First_Element(Vector: TreeVector.Vector) return T_FamilyTree is
   begin
      return Vector.First_Element;
   end First_Element;


end FamilyTree;
