with BinaryTree;
with Person;     use Person;
with Ada.Containers.Vectors;

package FamilyTree is

   -- GENERIC USAGE OF BINARY TREE
   package Tree is new BinaryTree
     (
      PutGeneric => showPerson,
      T_Element   => T_Person
     );
   use Tree;

   -- TREE CHAINED LIST
   package TreeVector is new Ada.Containers.Vectors (Index_Type => Natural, Element_Type => T_BinaryTree);

   -- 1. Créer un arbre minimal contenant le seul nœud racine, sans père ni mère.
   procedure initializeFamilyTree
     (ABR : out T_BinaryTree; Person : in T_Person) with
     Post =>
      getGenerationsCount (ABR) = 0; -- SHOULD HAVE ONE SINGLE GENERATION

   -- Renvoie le nombre de générations dans un arbre donné
   function getGenerationsCount (ABR : in T_BinaryTree) return Integer;

   -- 3. Obtenir le nombre d’ancêtres connus (lui compris) d’un individu donné
   function getAncestorsCount
     (ABR : in T_BinaryTree; Key : in Integer) return Integer;

   -- 4. Obtenir l’ensemble des ancêtres situés à une certaine génération d’un individu donné.
   function getAncestorsByGeneration
     (ABR : in T_BinaryTree; Key : in Integer; Generation : in Integer)
      return TreeVector.Vector;

   -- 5. Afficher l’arbre à partir d’un nœud donné.
   procedure showFamilyTree (ABR : in T_BinaryTree; Key : in Integer);

   -- 7. Obtenir l’ensemble des individus qui n’ont qu’un parent connu.
   function getSingleParentIndividuals
     (ABR : in T_BinaryTree; Key : in Integer) return TreeVector.Vector;

   -- 8. Obtenir l’ensemble des individus dont les deux parents sont connus.
   function getDualParentIndividuals
     (ABR : in T_BinaryTree; Key : in Integer) return TreeVector.Vector;

end FamilyTree;
