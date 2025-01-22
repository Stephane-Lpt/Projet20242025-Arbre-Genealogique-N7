with Ada.Text_IO;         use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with utils;               use utils;

package body FamilyTree is

   -- 1. Créer un arbre minimal contenant le seul nœud racine, sans père ni mère.
   procedure initializeFamilyTree
     (ABR : out T_BinaryTree; Person : in T_Person) is
   begin
      Null;
   end initializeFamilyTree;

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
                                   Generation : in Integer) return TreeVector is
   -- A vector to store the ancestors at the specified generation
   Ancestors : TreeVector := (others => null);
   Stop : Boolean := False;
   
   -- Find the node with the given Key
   NodeToFind : T_BinaryTree := getNode(ABR, Key);

   -- A helper procedure to process each ancestor during the traversal
   procedure processAncestor (ABR : in out T_BinaryTree; 
                              Parent : in out T_BinaryTree; 
                              Stop : in out Boolean) is
      GenLevel : Integer := 0;
      begin
         if ABR = null or else Stop then
            return;
         end if;

         -- If we are at the target generation level, add the ancestor to the vector
         if GenLevel = Generation then
            -- Add ABR to Ancestors (you may need to define how TreeVector works)
            Ancestors := Ancestors & (ABR);
            Stop := True;  -- Stop further traversal after finding the generation
         end if;

         -- Continue the traversal upward if we are not yet at the desired generation
         if GenLevel < Generation then
            GenLevel := GenLevel + 1;
         end if;

      end processAncestor;

   begin
      -- Check if the node to find exists
      if NodeToFind = null then
         return Ancestors;  -- Return empty vector if node not found
      end if;

      -- Traverse the tree and apply the processAncestor procedure
      traverseTreeAndApply(ABR, Parent => null, ActionCallback => processAncestor, Stop => Stop);

      return Ancestors;
   end getAncestorsByGeneration;

   -- 5. Afficher l’arbre à partir d’un nœud donné.
   procedure showFamilyTree (ABR : in T_BinaryTree; Key : in Integer) is
   begin
      Null;
   end showFamilyTree;

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
   function 

end FamilyTree;
