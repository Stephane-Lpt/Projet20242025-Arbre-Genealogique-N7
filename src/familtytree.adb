with Ada.Text_IO;         use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Unchecked_Deallocation;
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
      Null;
   end getGenerationsCount;

   -- 3. Obtenir le nombre d’ancêtres connus (lui compris) d’un individu donné
   function getAncestorsCount
     (ABR : in T_BinaryTree; Key : in Integer) return Integer is
   begin
      Null;
   end getAncestorsCount;

   -- 4. Obtenir l’ensemble des ancêtres situés à une certaine génération d’un individu donné.
   function getAncestorsByGeneration (ABR : in T_BinaryTree; Key : in Integer; Generation : in Integer) return TreeVector is
   begin
      Null;
   end getAncestorsByGeneration;

   -- 5. Afficher l’arbre à partir d’un nœud donné.
   procedure showFamilyTree (ABR : in T_BinaryTree; Key : in Integer) is
   begin
      Null;
   end showFamilyTree;

   -- 7. Obtenir l’ensemble des individus qui n’ont qu’un parent connu.
   function getSingleParentIndividuals (ABR : in T_BinaryTree; Key : in Integer) return TreeVector is
   begin
      Null;
   end getSingleParentIndividuals;

   -- 8. Obtenir l’ensemble des individus dont les deux parents sont connus.
   function getDualParentIndividuals (ABR : in T_BinaryTree; Key : in Integer) return TreeVector is
   begin
      Null;
   end getDualParentIndividuals;

end FamilyTree;
