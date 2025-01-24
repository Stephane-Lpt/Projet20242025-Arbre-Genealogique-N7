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
      Null;
   end deleteAncestor;

   -- Renvoie le nombre de générations dans un arbre donné
   function getGenerationsCount (ABR : in T_BinaryTree) return Integer is
   begin
      return 0;
   end getGenerationsCount;

   -- 3. Obtenir le nombre d’ancêtres connus (lui compris) d’un individu donné
   function getAncestorsCount
   (ABR : in T_FamilyTree; Key : in Integer) return Integer 
   is
      Node : constant T_FamilyTree := getFamilyNode(ABR, Key);
   begin
      if IsNull(Node) then
         return 0;
      else
         return getSize(Node); -- Utilise la fonction getSize existante du BinaryTree
      end if;
   end getAncestorsCount;

   -- 4. Obtenir l’ensemble des ancêtres situés à une certaine génération d’un individu donné.
   function getAncestorsByGeneration (ABR : in T_FamilyTree; 
                                    Key : in Integer; 
                                    Generation : in Integer) return TreeVector.Vector is

      TargetABR : constant T_FamilyTree := getFamilyNode(ABR, Key);

      -- Fonction helper récursive pour collecter les ancêtres à la génération cible
      function Helper (ABR : T_FamilyTree; CurrentGen : Integer) return TreeVector.Vector is
         Result : TreeVector.Vector;
      begin
         if IsNull(ABR) then
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
      if IsNull(TargetABR) or Generation < 0 then
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
   begin
      return SingleParentIndividuals;
   end getSingleParentIndividuals;

   -- 8. Obtenir l’ensemble des individus dont les deux parents sont connus.
   function getDualParentIndividuals (ABR : in T_BinaryTree; Key : in Integer) return TreeVector.Vector is
      DualParentIndividuals : TreeVector.Vector;
   begin
      return DualParentIndividuals;
   end getDualParentIndividuals;

   -- Getters
   --function 
   
   function getParent (ABR : in T_FamilyTree; Position : in T_Position) return T_FamilyTree is
   begin
      case Position is
         when ROOT =>
            raise Wrong_Position_Exception;
         when LEFT =>
            return getLeftChild (ABR);
         when RIGHT =>
            return getLeftChild (ABR);
      end case;
   end getParent;

   function getFamilyNode(ABR : in T_FamilyTree; Key : in Integer ) return T_FamilyTree is
   begin
      return getNode(ABR, Key);
   end getFamilyNode;

   function IsNull (ABR : in T_FamilyTree) return Boolean is
   begin
      return isEmpty(ABR);
   end IsNull;

   function getLength(Vector: TreeVector.Vector) return Integer is
   begin
      return Integer(Vector.Length);
   end getLength;


end FamilyTree;
