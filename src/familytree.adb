with Ada.Text_IO;         use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with utils;               use utils;

package body FamilyTree is

   function isEmpty(ABR : in T_FamilyTree) return Boolean is
   begin
      return Tree.isEmpty (ABR);
   end isEmpty;

   function isPresent(ABR : in T_FamilyTree; Key : in Integer) return Boolean is
   begin
      return Tree.isPresent (ABR, Key);
   end isPresent;

   procedure initFamilyTree(ABR : out T_FamilyTree) is
   begin
      initTree(ABR);
   end initFamilyTree;

   function getEmptyFamilyTree return T_FamilyTree is
      Tree : T_FamilyTree;
   begin
      initFamilyTree (Tree);
      return Tree;
   end getEmptyFamilyTree;

   -- 1. Créer un arbre minimal contenant le seul nœud racine, sans père ni mère.
   procedure initChild(ABR : out T_FamilyTree; Key: in Integer; Person : in T_Person) is
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
     (ABR : in T_BinaryTree; Key : in Integer) return Integer is
   begin
      return 0;
   end getAncestorsCount;

   -- 4. Obtenir l’ensemble des ancêtres situés à une certaine génération d’un individu donné.
   function getAncestorsByGeneration (ABR : in T_BinaryTree;
                                   Key : in Integer;
                                   Generation : in Integer) return TreeVector.Vector is
   -- A vector to store the ancestors at the specified generation
   Ancestors : TreeVector.Vector := TreeVector.Empty_Vector;
   Stop : Boolean := False;

   -- Find the node with the given Key
   NodeToFind : T_FamilyTree := getNode(ABR, Key);

   -- A helper procedure to process each ancestor during the traversal
   procedure processAncestor (ABR : in out T_BinaryTree;
                              Parent : in out T_BinaryTree;
                              Stop : in out Boolean) is
      GenLevel : Integer := 0;
      begin
         if isEmpty(ABR) or else Stop then
            return;
         end if;

         -- If we are at the target generation level, add the ancestor to the vector
         if GenLevel = Generation then
            -- Add ABR to Ancestors (you may need to define how TreeVector works)
            Ancestors.append(ABR);
            Stop := True;  -- Stop further traversal after finding the generation
         end if;

         -- Continue the traversal upward if we are not yet at the desired generation
         if GenLevel < Generation then
            GenLevel := GenLevel + 1;
         end if;

      end processAncestor;

   begin
      -- Check if the node to find exists
      if isEmpty(NodeToFind) then
         return Ancestors;  -- Return empty vector if node not found
      end if;

      -- Traverse the tree and apply the processAncestor procedure
      --traverseTreeAndApply(ABR, Parent => null, ActionCallback => processAncestor, Stop => Stop);

      return Ancestors;
   end getAncestorsByGeneration;

   -- Afficher l’arbre.
   procedure showFamilyTree (ABR : in T_FamilyTree; Verbosity : in Integer := 1) is
   begin
      Put_Line("Size: " & getSize(ABR)'Image);
      showTree (ABR => ABR, PropToShow => Elements, Verbosity => Verbosity);
   end showFamilyTree;

   -- 5. Afficher l’arbre à partir d’un nœud donné.
   procedure showFamilyTreeFromId (ABR : in T_FamilyTree; Key : in Integer; Verbosity : in Integer := 1) is
   begin
      showFamilyTree (ABR => getNode(ABR, Key), Verbosity => Verbosity);
   end showFamilyTreeFromId;

   -- 7. Obtenir l’ensemble des individus qui n’ont pas de parents connus.
   function getOrphanIndividuals (ABR : in T_FamilyTree; Key : in Integer) return TreeVector.Vector is
       OrphanIndividuals : TreeVector.Vector;
   begin
      return OrphanIndividuals;
   end getOrphanIndividuals;

   -- 8. Obtenir l’ensemble des individus qui n’ont qu’un parent connu.
   function getSingleParentIndividuals (ABR : in T_BinaryTree; Key : in Integer) return TreeVector.Vector is
       SingleParentIndividuals : TreeVector.Vector;
   begin
      return SingleParentIndividuals;
   end getSingleParentIndividuals;

   -- 9. Obtenir l’ensemble des individus dont les deux parents sont connus.
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

   procedure printKey(ABR : in T_FamilyTree) is 
   begin
      Put_Line(Integer'Image(getKey(ABR)));
   end printKey;

end FamilyTree;
