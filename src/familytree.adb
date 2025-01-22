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
     (ABR : in T_BinaryTree; Key : in Integer) return Integer is
   begin
      return 0;
   end getAncestorsCount;

   -- 4. Obtenir l’ensemble des ancêtres situés à une certaine génération d’un individu donné.
   function getAncestorsByGeneration (ABR : in T_BinaryTree; Key : in Integer; Generation : in Integer) return TreeVector.Vector is
      Ancestors : TreeVector.Vector;
   begin
      return Ancestors;
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

end FamilyTree;
