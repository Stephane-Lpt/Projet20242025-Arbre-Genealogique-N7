generic
   type T_Donnee is private;
   type T_Cle is private;

package ABR is

	type T_ABR is limited private;

	Cle_Presente_Exception : Exception;	-- une clé est déjà présente dans un ABR
	Cle_Absente_Exception  : Exception;	-- une clé est absente d'un ABR

	-- Initialiser un ABR Abr.  L'ABR est vide.
	procedure Initialiser(Abr: out T_ABR) with
		Post => Est_Vide (Abr);

	-- Est-ce qu'un ABR Abr est vide ?
	function Est_Vide (Abr : T_Abr) return Boolean;

	-- Obtenir le nombre d'éléments d'un ABR. 
	function Taille (Abr : in T_ABR) return Integer with
		Post => Taille'Result >= 0
			and (Taille'Result = 0) = Est_Vide (Abr);

	-- Insérer la donnée Donnée associée à la clé Clé dans l'ABR Abr.
	-- Exception : Cle_Presente_Exception si la clé est déjà dans l'Abr.
	procedure Inserer (Abr : in out T_ABR; Cle : in Character; Donnee : in Integer) with
		Post => La_Donnee (Abr, Cle) = Donnee			-- donnée insérée
			; -- XXX and Taille (Abr) = Taille (Abr)'Old + 1; -- un élément de plus

	-- Modifier la donnée Donnée associée à la clé Clé dans l'ABR Abr.
	-- Exception : Cle_Absente_Exception si Clé n'est pas utilisée dans l'Abr
	procedure Modifier (Abr : in out T_ABR ; Cle : in Character ; Donnee : in Integer) with
		Post => La_Donnee (Abr, Cle) = Donnee;		-- donnée mise à jour

	-- Supprimer la donnée associée à la clé Clé dans l'ABR Abr.
	-- Exception : Cle_Absente_Exception si Clé n'est pas utilisée dans l'Abr
	procedure Supprimer (Abr : in out T_ABR ; Cle : in Character) with
		Post =>  Taille (Abr) = Taille (Abr)'Old - 1; -- un élément de moins

	-- Obtenir la donnée associée à la clé Cle dans l'ABR Abr.
	-- Exception : Cle_Absente_Exception si Clé n'est pas utilisée dans l'Abr
	function La_Donnee (Abr : in T_ABR ; Cle : in Character) return Integer;

	-- Supprimer tous les éléments d'un ABR.
	-- Doit être utilisée dès qu'on sait qu'un ABR ne sera plus utilisé.
	procedure Vider (Abr : in out T_ABR) with
		Post => Est_Vide (Abr);

	-- Afficher un ABR Abr dans l'ordre croissant des clés (parcours infixe)
	procedure Afficher (Abr : in T_Abr);

	-- Afficher un ABR Abr (en faisant apparaître la strucre grâce à une
	-- indendation et un signe '<', '>', '/' pour indiquer la sous-arbre
	-- gauche, '>' pour un sous arbre droit et '/' pour la racine)
	-- Exemple :
	--
	--  / Cle1 : Valeur1
	--      < Cle2 : Valeur2
	--          > Cle3 : Valeur3
	--      > Cle4 : Valeur 4
	--          < Cle5 : Valeur 5
	procedure Afficher_Debug (Abr : in T_Abr);

private

	type T_Noeud;
	type T_ABR is access T_Noeud;
	type T_Noeud is
		record
			Cle: T_Cle;
			Donnee : T_Donnee;
			Sous_Arbre_Gauche : T_ABR;
			Sous_Arbre_Droit : T_ABR;
		end record;

end ABR;


-- Spécification du module BINARY TREE.

   -- 1. Créer un arbre minimal contenant le seul nœud racine, sans père ni mère
   -- 2. Ajouter un parent (mère ou père) à un noeud donné.
   -- 3. Obtenir le nombre d’ancêtres connus (lui compris) d’un individu donné. Sur l’exemple
   -- ci-dessus, le nombre d’ancêtres connus de 2 est 5.
   -- 4. Obtenir l’ensemble des ancêtres situés à une certaine génération d’un nœud donné.
   -- Par exemple, les ancêtres de génération 2 du nœud 18 sont les noeuds 15, 26 et 33.
   -- Les ancêtres de génération 1 du nœud 33 sont les nœuds 25 et 42.
   -- 5. Afficher l’arbre à partir d’un nœud donné.
   -- 6. Supprimer, pour un arbre, un nœud et ses ancêtres. Par exemple, si on veut supprimer
   -- le noeud 15 et ses ancêtres, on supprime le nœud 5.
   -- 7. Obtenir l’ensemble des individus qui n’ont qu’un parent connu.
   -- 8. Obtenir l’ensemble des individus dont les deux parents sont connus.
   -- 9. Obtenir l’ensemble des individus dont les deux parents sont inconnus

   -- Est-ce que la pile est vide ?
   function Est_Vide (Pile : in T_Pile) return Boolean;

   -- Est-ce que la pile est pleine ?
   function Est_Pleine (Pile : in T_Pile) return Boolean;

   -- L'élément en sommet de la pile.
   function Sommet (Pile : in T_Pile) return T_Element with
     Pre => not Est_Vide (Pile);

   -- Empiler l'élément en somment de la pile.
   procedure Empiler (Pile : in out T_Pile; Element : in T_Element) with
     Pre => not Est_Pleine (Pile), Post => Sommet (Pile) = Element;

   -- Supprimer l'élément en sommet de pile
   procedure Depiler (Pile : in out T_Pile) with
     Pre => not Est_Vide (Pile);

   -- afficher.spec START DELETE
   -- afficher.ng.spec START DELETE
   -- Afficher les éléments de la pile
   -- afficher.ng.spec STOP DELETE
   generic
      with procedure Afficher_Element (Un_Element : in T_Element);
   -- afficher.ng.spec START DELETE
   procedure Afficher (Pile : in T_Pile);
   -- afficher.ng.spec STOP DELETE
   -- afficher.spec STOP DELETE

private

   type T_Tab_Elements is array (1 .. Capacite) of T_Element;

   type T_Pile is record
      Elements : T_Tab_Elements;  -- les éléments de la pile
      Taille   : Integer;        -- Nombre d'éléments dans la pile
   end record;

end Piles;
