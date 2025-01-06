with BinaryTree; use BinaryTree;
with Person; use Person;
with Vector; use Vector;

package FamilyTree is

   package T_FamilyTree is
         new BinaryTree (
            T_Key => Hash,
            T_Data => T_Person
         );
   use T_FamilyTree;

   -- 1. Créer un arbre minimal contenant le seul nœud racine, sans père ni mère.
   procedure initialize (ABR : out T_FamilyTree; Individu : in T_Person) with
      Post => getGenerationsCount(ABR) = 0;

   -- Renvoie le nombre de générations dans un arbre donné
   function getGenerationsCount (ABR : in T_FamilyTree) return Integer;

   -- 2. Ajouter un parent (mère ou père) à un noeud donné.
   procedure addParent (ABR : in out T_FamilyTree; Id_Node : in Hash; Parent : in T_Person) with
      Pre => getAncestorsCount(ABR, Id_Node, 0) < 2;
      -- Post?

   -- 3. Obtenir le nombre d’ancêtres connus (lui compris) d’un individu donné
   function getAncestorsCount (ABR : in T_FamilyTree; Id_Node : in Hash) return T_Vector;

   -- 4. Obtenir l’ensemble des ancêtres situés à une certaine génération d’un individu donné.
   function getAncestorsByGeneration (ABR : in T_FamilyTree; Id_Node : in Hash; Generation : in Integer) return T_Vector;

   -- 5. Afficher l’arbre à partir d’un nœud donné.
   procedure showNode (ABR : in T_FamilyTree; Id_Node : in Hash);

   -- 6. Supprimer, pour un arbre, un nœud et ses ancêtres.
   procedure deleteNode (ABR: in out T_FamilyTree; Id_Node: in Hash);

   -- 7. Obtenir l’ensemble des individus qui n’ont qu’un parent connu.
   function getSingleParentIndividuals (ABR : in T_FamilyTree; Id_Node : in Hash) return T_Vector;

   -- 8. Obtenir l’ensemble des individus dont les deux parents sont connus.
   function getDualParentIndividuals (ABR : in T_FamilyTree; Id_Node : in Hash) return T_Vector;

end;