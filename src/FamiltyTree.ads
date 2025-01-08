with BinaryTree; use BinaryTree;
with Person;     use Person;
with Ada.Containers.Vectors;

package FamilyTree is

   -- CHAINED LIST FOR PERSON
   package PersonVector is new Ada.Containers.Vectors (Element_Type => Person);

   -- GENERIC USAGE OF BINARY TREE
   package P_FamilyTree is new BinaryTree
     (T_Key => Integer, T_Data => T_Person);
   use P_FamilyTree;

   -- 1. Créer un arbre minimal contenant le seul nœud racine, sans père ni mère.
   procedure initialize (ABR : out T_BinaryTree; Individu : in T_Person) with
     Post =>
      getGenerationsCount (ABR) = 0; -- SHOULD HAVE ONE SINGLE GENERATION

   -- Renvoie le nombre de générations dans un arbre donné
   function getGenerationsCount (ABR : in T_BinaryTree) return Integer;

   -- 2. Ajouter un parent (mère ou père) à un noeud donné.
   procedure addParent
     (ABR    : in out T_BinaryTree; Id_Node : in Integer;
      Parent : in     T_Person) with
     Pre  =>
      getAncestorsCount (ABR, Id_Node, 0) <
      2, -- SHOULD NOT ALREADY HAVE TWO PARENTS
     Post =>
      getAncestorsCount (ABR, Id_Node, 0)'Result =
      getAncestorsCount (ABR, Id_Node, 0) +
        1; -- THE NUMBER OF PARENTS SHOULD BE +1 AT THE END

   -- 3. Obtenir le nombre d’ancêtres connus (lui compris) d’un individu donné
   function getAncestorsCount
     (ABR : in T_BinaryTree; Id_Node : in Integer) return Integer;

   -- 4. Obtenir l’ensemble des ancêtres situés à une certaine génération d’un individu donné.
   function getAncestorsByGeneration
     (ABR : in T_BinaryTree; Id_Node : in Integer; Generation : in Integer)
      return PersonVector;

   -- 5. Afficher l’arbre à partir d’un nœud donné.
   procedure showNode (ABR : in T_BinaryTree; Id_Node : in Integer);

   -- 6. Supprimer, pour un arbre, un nœud et ses ancêtres.
   procedure deleteNode (ABR : in out T_BinaryTree; Id_Node : in Integer) with
     Pre => isPresent (ABR, Id_Node), -- NODE Id_Node IS PRESENT IN FAMILY TREE
     Post =>
      isPresent (ABR, Id_Node) =
      false; --TODO: DEFINE A CORRECT POST CONDITION TO DESCRIBE THE BEHAVIOUR (ALL ANCESTORS BEING DELETED)

   -- 7. Obtenir l’ensemble des individus qui n’ont qu’un parent connu.
   function getSingleParentIndividuals
     (ABR : in T_BinaryTree; Id_Node : in Integer) return PersonVector;

   -- 8. Obtenir l’ensemble des individus dont les deux parents sont connus.
   function getDualParentIndividuals
     (ABR : in T_BinaryTree; Id_Node : in Integer) return PersonVector;

   -- Get node by ID
   function getNode
     (ABR : in T_BinaryTree; Id_Node : in Integer) return T_Node;

end FamilyTree;
