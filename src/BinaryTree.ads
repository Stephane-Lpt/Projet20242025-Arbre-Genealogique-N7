generic
   type T_Data is private;
   type T_Key is private;

package BinaryTree is

	type T_BinaryTree is limited private;

	Key_Presente_Exception : Exception;	-- une clé est déjà présente dans un ABR
	Key_Absente_Exception  : Exception;	-- une clé est absente d'un ABR

	-- Initialiser un ABR Abr.  L'ABR est vide.
	procedure Initialiser(Abr: out T_BinaryTree) with
		Post => Est_Vide (Abr);

	-- Est-ce qu'un ABR Abr est vide ?
	function Est_Vide (Abr : T_BinaryTree) return Boolean;

	-- Obtenir le nombre d'éléments d'un ABR. 
	function Taille (Abr : in T_BinaryTree) return Integer with
		Post => Taille'Result >= 0
			and (Taille'Result = 0) = Est_Vide (Abr);

	-- Insérer la donnée Donnée associée à la clé Clé dans l'ABR Abr.
	-- Exception : Key_Presente_Exception si la clé est déjà dans l'Abr.
	procedure Inserer (Abr : in out T_BinaryTree; Key : in Character; Data : in Integer) with
		Post => La_Data (Abr, Key) = Data			-- donnée insérée
			; -- XXX and Taille (Abr) = Taille (Abr)'Old + 1; -- un élément de plus

	-- Modifier la donnée Donnée associée à la clé Clé dans l'ABR Abr.
	-- Exception : Key_Absente_Exception si Clé n'est pas utilisée dans l'Abr
	procedure Modifier (Abr : in out T_BinaryTree ; Key : in Character ; Data : in Integer) with
		Post => La_Data (Abr, Key) = Data;		-- donnée mise à jour

	-- Supprimer la donnée associée à la clé Clé dans l'ABR Abr.
	-- Exception : Key_Absente_Exception si Clé n'est pas utilisée dans l'Abr
	procedure Supprimer (Abr : in out T_BinaryTree ; Key : in Character) with
		Post =>  Taille (Abr) = Taille (Abr)'Old - 1; -- un élément de moins

	-- Obtenir la donnée associée à la clé Key dans l'ABR Abr.
	-- Exception : Key_Absente_Exception si Clé n'est pas utilisée dans l'Abr
	function La_Data (Abr : in T_BinaryTree ; Key : in Character) return Integer;

	-- Supprimer tous les éléments d'un ABR.
	-- Doit être utilisée dès qu'on sait qu'un ABR ne sera plus utilisé.
	procedure Vider (Abr : in out T_BinaryTree) with
		Post => Est_Vide (Abr);

	-- Afficher un ABR Abr dans l'ordre croissant des clés (parcours infixe)
	procedure Afficher (Abr : in T_BinaryTree);

	-- Afficher un ABR Abr (en faisant apparaître la strucre grâce à une
	-- indendation et un signe '<', '>', '/' pour indiquer la sous-arbre
	-- gauche, '>' pour un sous arbre droit et '/' pour la racine)
	-- Exemple :
	--
	--  / Key1 : Valeur1
	--      < Key2 : Valeur2
	--          > Key3 : Valeur3
	--      > Key4 : Valeur 4
	--          < Key5 : Valeur 5
	procedure Afficher_Debug (Abr : in T_BinaryTree);

private

	type T_Noeud;
	type T_BinaryTree is access T_Noeud;
	type T_Noeud is
		record
			Key: T_Key;
			Data : T_Data;
			Sous_Arbre_Gauche : T_BinaryTree;
			Sous_Arbre_Droit : T_BinaryTree;
		end record;

end BinaryTree;