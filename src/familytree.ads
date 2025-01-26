with BinaryTree;
with Person; use Person;
with Ada.Containers.Vectors;
with utils;  use utils;

package FamilyTree is

   -- GENERIC USAGE OF BINARY TREE
   package Tree is new BinaryTree
     (PutGeneric => showPerson, T_Element => T_Person);
   use Tree;

   subtype T_FamilyTree is T_BinaryTree;

   -- TREE CHAINED LIST
   package TreeVector is new Ada.Containers.Vectors
     (Index_Type => Natural, Element_Type => T_FamilyTree);

   function GetExampleFamilyTree return T_FamilyTree;

   function getEmptyFamilyTree return T_FamilyTree;

   -- INITIALIZES AN EMPTY FAMILY TREE
   procedure initFamilyTree (ABR : out T_FamilyTree) with
     Post => isEmpty (ABR);

   -- 1. Créer un arbre minimal contenant le seul nœud racine, sans père ni mère.
   procedure initChild
     (ABR : out T_FamilyTree; Key : in Integer; Person : in T_Person) with
     Post =>
      getGenerationsCount (ABR) = 1; -- SHOULD HAVE ONE SINGLE GENERATION

   procedure addAncestor
     (ABR      : in out T_FamilyTree; TargetKey : in Integer;
      Position : in T_Position; NewKey : in Integer; NewPerson : in T_Person);

   procedure deleteAncestor
     (ABR : in out T_FamilyTree; TargetKey : in Integer);

   -- Renvoie le nombre de générations dans un arbre donné
   function getGenerationsCount (ABR : in T_FamilyTree) return Integer;

   -- 3. Obtenir le nombre d’ancêtres connus (lui compris) d’un individu donné
   function getAncestorsCount
     (ABR : in T_FamilyTree; Key : in Integer) return Integer;

   -- 4. Obtenir l’ensemble des ancêtres situés à une certaine génération d’un individu donné.
   function getAncestorsByGeneration
     (ABR : in T_FamilyTree; Key : in Integer; Generation : in Integer)
      return TreeVector.Vector;

   procedure showFamilyTree
     (ABR : in T_FamilyTree; Verbosity : in Integer := 1);
   procedure showFamilyTreeFromId
     (ABR : in T_BinaryTree; Key : in Integer; Verbosity : in Integer := 1);

   -- 7. Obtenir l’ensemble des individus qui n’ont pas de parents connus.
   function getOrphanIndividuals
     (ABR : in T_FamilyTree; Key : in Integer) return TreeVector.Vector;

   -- 8. Obtenir l’ensemble des individus qui n’ont qu’un parent connu.
   function getSingleParentIndividuals
     (ABR : in T_FamilyTree; Key : in Integer) return TreeVector.Vector;

   -- 9. Obtenir l’ensemble des individus dont les deux parents sont connus.
   function getDualParentIndividuals
     (ABR : in T_FamilyTree; Key : in Integer) return TreeVector.Vector;

   function getNode
     (ABR : in T_FamilyTree; Key : in Integer) return T_FamilyTree;

   function getPerson(ABR : in T_FamilyTree) return T_Person with
      Pre => not isEmpty (ABR);

   function isEmpty (ABR : in T_FamilyTree) return Boolean;

   procedure clean (ABR : in out T_FamilyTree);

   function isPresent (ABR : in T_FamilyTree; Key : in Integer) return Boolean;

   function getKey (ABR : T_FamilyTree) return Integer;

   -- Vectors helper functions

   function Length (Vector : TreeVector.Vector) return Integer;

   function First_Element (Vector : TreeVector.Vector) return T_FamilyTree;

   function getRightChild (ABR : in T_FamilyTree) return T_FamilyTree;

   function getLeftChild (ABR : in T_FamilyTree) return T_FamilyTree;

   function GetKeysStringFromTreeVector
     (Vector : in TreeVector.Vector; Verbosity : in Integer) return String;
end FamilyTree;
