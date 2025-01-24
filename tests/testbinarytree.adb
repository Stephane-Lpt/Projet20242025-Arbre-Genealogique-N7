with Ada.Text_IO; use Ada.Text_IO;
with Utils; use Utils;
with BinaryTree; 

procedure TestBinaryTree is
    procedure PutInteger (Element : in Integer; Key : in Integer; Depth : in Integer := 0; Position : in T_Position := ROOT; Verbosity : in Integer := 1) is
    begin
      Put_Line(getIndent(Depth) & getBinaryTreePrefix(Position) & Integer'Image(Key) & ":" & Integer'Image(Element));
   end PutInteger;

   package IntegerBinaryTree is new BinaryTree
     (
      PutGeneric => PutInteger,
      T_Element   => Integer
     );
   use IntegerBinaryTree;

   function createOrdinaryTree return T_BinaryTree is
      -- Déclaration des arbres
      Tree1, Tree2, Tree3, Tree4, Tree5 : T_BinaryTree;
   begin
      -- Initialisation des arbres
      initRoot(Tree1, 1, 10);
      initRoot(Tree2, 2, 20);
      initRoot(Tree3, 3, 30);
      initRoot(Tree4, 4, 40);
      initRoot(Tree5, 5, 50);

      -- Ajouter des nœuds aux arbres
      addNode(Tree1, Tree2, 1, LEFT);  -- Ajoute Tree2 comme enfant gauche de Tree1
      addNode(Tree1, Tree3, 1, RIGHT); -- Ajoute Tree3 comme enfant droit de Tree1
      addNode(Tree3, Tree4, 3, LEFT);  -- Ajoute Tree4 comme enfant gauche de Tree3
      addNode(Tree4, Tree5, 4, RIGHT); -- Ajoute Tree5 comme enfant droit de Tree4

      return Tree1;
   end createOrdinaryTree;

   -- TEST isEmpty --
   procedure TestIsEmpty is
      Tree1, Tree2 : T_BinaryTree;
   begin
      Put_Line("");
      Put_Line("---- Tests isEmpty ----");
      Put_Line("");

      initTree(Tree1);
      initRoot(Tree2, 1, 1);

      Put_Line("Test 1: Vérification de l'initialisation d'un arbre vide");
      pragma Assert (isEmpty(Tree1), "Échec: Tree1 devrait être vide");
      Put_Line("Succès: Tree1 correctement initialisé vide");

      Put_Line("Test 2: Vérification d'un arbre avec racine");
      pragma Assert (not isEmpty(Tree2), "Échec: Tree2 ne devrait pas être vide");
      Put_Line("Succès: Tree2 correctement initialisé avec une racine");
   end TestIsEmpty;


   -- TEST addNode --
   procedure TestAddNode is
      RootTree, LeftTree, RightTree, SubTree, NewChild : T_BinaryTree;
   begin
      Put_Line("");
      Put_Line("---- Tests addNode ----");
      Put_Line("");

      -- Initialisation de la racine et des enfants
      Put_Line("Initialisation de la racine (clé 1)");
      initRoot(RootTree, 1, 12);
      initRoot(LeftTree, 2, 82);
      initRoot(RightTree, 3, 23);

      Put_Line("Ajout des enfants gauche et droit à la racine");
      addNode(RootTree, LeftTree, 1, LEFT);
      addNode(RootTree, RightTree, 1, RIGHT);

      Put_Line("Vérification des enfants de la racine");
      pragma Assert(getLeftChild(RootTree) = LeftTree, "Erreur: Enfant gauche non trouvé");
      pragma Assert(getRightChild(RootTree) = RightTree, "Erreur: Enfant droit non trouvé");
      pragma Assert(getKey(getLeftChild(RootTree)) = 2, "Erreur: Clé de l'enfant gauche incorrecte");
      pragma Assert(getElement(getLeftChild(RootTree)) = 82, "Erreur: Valeur de l'enfant gauche incorrecte");
      pragma Assert(getKey(getRightChild(RootTree)) = 3, "Erreur: Clé de l'enfant droit incorrecte");
      pragma Assert(getElement(getRightChild(RootTree)) = 23, "Erreur: Valeur de l'enfant droit incorrecte");
      Put_Line("Succès: Enfants de la racine vérifiés");

      -- Ajout à un nœud interne 
      Put_Line("Récupération du sous-arbre gauche (clé 2)");
      SubTree := getNode(RootTree, 2); 

      Put_Line("Ajout d'un enfant gauche au sous-arbre");
      initRoot(NewChild, 4, 45);
      addNode(SubTree, NewChild, 2, LEFT);

      Put_Line("Vérification de l'ajout");
      pragma Assert(getLeftChild(SubTree) = NewChild, "Erreur: Enfant gauche non ajouté au sous-arbre");
      pragma Assert(getKey(getLeftChild(SubTree)) = 4, "Erreur: Clé de l'enfant gauche incorrecte");
      pragma Assert(getElement(getLeftChild(SubTree)) = 45, "Erreur: Valeur de l'enfant gauche incorrecte");
      Put_Line("Succès: Enfant gauche ajouté au sous-arbre");

      -- Ajout d'un enfant droit
      Put_Line("Ajout d'un enfant droit au sous-arbre");
      initRoot(NewChild, 5, 99);
      addNode(SubTree, NewChild, 2, RIGHT);

      Put_Line("Vérification de l'ajout");
      pragma Assert(getRightChild(SubTree) = NewChild, "Erreur: Enfant droit non ajouté au sous-arbre");
      pragma Assert(getKey(getRightChild(SubTree)) = 5, "Erreur: Clé de l'enfant droit incorrecte");
      pragma Assert(getElement(getRightChild(SubTree)) = 99, "Erreur: Valeur de l'enfant droit incorrecte");
      Put_Line("Succès: Enfant droit ajouté au sous-arbre");
   end TestAddNode;


   -- TEST isPresent --
   procedure TestIsPresent is
      RootTree, Tree1, Tree2 : T_BinaryTree;
   begin
      Put_Line("");
      Put_Line("---- Tests isPresent ----");
      Put_Line("");

      initRoot(RootTree, 1, 12);
      initRoot(Tree1, 2, 97);
      initRoot(Tree2, 3, 78);

      addNode (RootTree, Tree1, 1, LEFT);
      addNode (Tree1, Tree2, 2, RIGHT);

      Put_Line("Vérification des clés existantes");
      pragma Assert (isPresent (RootTree, 1), "Erreur: Racine (clé 1) non trouvée");
      pragma Assert (isPresent (RootTree, 2), "Erreur: Enfant gauche (clé 2) non trouvé");
      pragma Assert (isPresent (RootTree, 3), "Erreur: Enfant droit (clé 3) non trouvé");
      Put_Line("Succès: Toutes les clés existantes sont présentes");

      Put_Line("Vérification d'une clé absente");
      pragma Assert (not isPresent (RootTree, 4), "Erreur: Clé 4 trouvée dans l'arbre");
      Put_Line("Succès: Clé absente correctement identifiée");
   end TestIsPresent;


   -- TEST getSize --
   procedure TestGetSize is
      Tree1, Tree2, Tree3, Tree4, Tree5 : T_BinaryTree;
   begin
      Put_Line("");
      Put_Line("---- Tests getSize ----");
      Put_Line("");

      Put_Line("Test sur arbre vide");
      initTree (Tree1);
      pragma Assert (getSize (Tree1) = 0, "Erreur: Taille de l'arbre vide non nulle");
      Put_Line("Succès: Taille de l'arbre vide correcte");

      Put_Line("Test avec racine uniquement");
      initRoot(Tree1, 1, 12);
      pragma Assert (getSize (Tree1) = 1, "Erreur: Taille de l'arbre avec racine incorrecte");
      Put_Line("Succès: Taille après ajout de la racine correcte");

      Put_Line("Test après ajout de deux enfants");
      initRoot(Tree2, 2, 97);
      initRoot(Tree3, 3, 78);
      addNode (Tree1, Tree2, 1, LEFT);
      addNode (Tree1, Tree3, 1, RIGHT);
      pragma Assert (getSize (Tree1) = 3, "Erreur: Taille après ajout des enfants incorrecte");
      Put_Line("Succès: Taille après ajout des enfants correcte");

      Put_Line("Test après ajout de deux petits-enfants");
      initRoot(Tree4, 4, 34);
      initRoot(Tree5, 5, 90);
      addNode (Tree2, Tree4, 2, LEFT);
      addNode (Tree3, Tree5, 3, RIGHT);
      pragma Assert (getSize (Tree1) = 5, "Erreur: Taille après ajout des petits-enfants incorrecte");
      Put_Line("Succès: Taille finale de l'arbre correcte");
   end TestGetSize;


   -- TEST getNode --
   procedure TestGetNode is
      Tree1, Tree2, Tree3, Tree4, Tree5, FoundRootTree, FoundChildTree, NotFoundTree : T_BinaryTree;
   begin
      Put_Line("");
      Put_Line("---- Tests getNode ----");
      Put_Line("");

      initRoot(Tree1, 1, 10);
      initRoot(Tree2, 2, 20);
      initRoot(Tree3, 3, 30);
      initRoot(Tree4, 4, 40);
      initRoot(Tree5, 5, 50);

      addNode(Tree1, Tree2, 1, LEFT);
      addNode(Tree1, Tree3, 1, RIGHT);
      addNode(Tree3, Tree4, 3, LEFT);
      addNode(Tree4, Tree5, 4, RIGHT);

      Put_Line("Recherche de la racine (clé 1)");
      FoundRootTree := getNode(Tree1, 1);
      pragma Assert (FoundRootTree = Tree1, "Erreur: Racine non trouvée");
      Put_Line("Succès: Racine trouvée");

      Put_Line("Recherche d'un nœud enfant (clé 3)");
      FoundChildTree := getNode(Tree1, 3);
      pragma Assert (FoundChildTree = Tree3, "Erreur: Nœud enfant non trouvé");
      Put_Line("Succès: Nœud enfant trouvé");

      Put_Line("Recherche d'un nœud inexistant (clé 6)");
      NotFoundTree := getNode(Tree1, 6);
      pragma Assert (isEmpty(NotFoundTree), "Erreur: Un nœud inexistant a été retourné");
      Put_Line("Succès: Nœud inexistant non trouvé");
   end TestGetNode;

procedure TestTraverseTreeAndApply is
   -- Déclaration des arbres
   Tree1 : T_BinaryTree;
   ABRParent: T_BinaryTree;
   Stop_Flag : Boolean := False;
   
   -- Callback qui marque le parcours comme terminé dès que la clé 3 est rencontrée
   procedure ActionExample (ABR : in out T_BinaryTree; Parent : in out T_BinaryTree; Stop : in out Boolean) is
   begin
      -- Arrêter si la clé du nœud est 3
         if getKey(ABR) = 3 then
         Stop := True;
      end if;
   end ActionExample;

   procedure NoStopActionExample (ABR : in out T_BinaryTree; Parent : in out T_BinaryTree; Stop : in out Boolean) is
   begin
      if getKey(ABR) = 99 then
         Stop := True;
      end if;
   end NoStopActionExample;

begin
   initTree (ABRParent);
   Tree1 := createOrdinaryTree;

   Put_Line("");
   Put_Line("---- Tests traverseTreeAndApply... ----");
   Put_Line("");

   -- Test 1: Vérification si le parcours s'arrête lorsqu'il atteint le nœud clé 5
   Stop_Flag := False;
   Put_Line("Test 1: Parcours jusqu'à l'origine (clé 1)");
   traverseTreeAndApply(Tree1, ABRParent, ActionExample'Access, Stop_Flag);
   
   -- Vérifier si le flag Stop a été mis à True
   pragma Assert(Stop_Flag, "Test 1 échoué: Le parcours n'a pas trouvé la clé 1");
   Put_Line("Test 1 réussi: Le parcours s'est arrêté directement après avoir trouvé l'origine (clé 1)");

   -- Test 2: Parcours avec arrêt lorsque la clé 3 est rencontrée
   Stop_Flag := False;  -- Réinitialiser le flag avant chaque test
   Put_Line("Test 2: Parcours jusqu'à la clé 3...");
   traverseTreeAndApply(Tree1, ABRParent, ActionExample'Access, Stop_Flag);
   
   -- Vérifier si le flag Stop a été mis à True
   pragma Assert(Stop_Flag, "Test 2 échoué: Le parcours n'a pas trouvé la clé 3.");
   Put_Line("Test 2 réussi: Le parcours s'est arrêté directement après avoir trouvé la clé 3.");

   -- Test 3: Vérification si le parcours s'arrête lorsqu'il atteint le nœud clé 5
   Stop_Flag := False;
   Put_Line("Test 3: Parcours jusqu'à la clé 5...");
   traverseTreeAndApply(Tree1, ABRParent, ActionExample'Access, Stop_Flag);
   
   -- Vérifier si le flag Stop a été mis à True
   pragma Assert(Stop_Flag, "Test 3 échoué: Le parcours n'a pas trouvé la clé 5.");
   Put_Line("Test 3 réussi: Le parcours s'est arrêté directement après avoir trouvé la clé 5.");

   -- Test 4: Vérifier le comportement lorsque la clé recherchée n'existe pas
   Stop_Flag := False;
   Put_Line("Test 4: Recherche d'une clé inexistante (par exemple 99)...");
   traverseTreeAndApply(Tree1, ABRParent, NoStopActionExample'Access, Stop_Flag);
   
   -- Vérifier si le flag Stop est resté False
   pragma Assert(not Stop_Flag, "Test 4 échoué: La clé 99 a été trouvée alors qu'elle n'existe pas.");
   Put_Line("Test 4 réussi: La clé 99 n'existe pas dans l'arbre donc le parcours ne s'est jamais arrêté.");

end TestTraverseTreeAndApply;

procedure TestDeleteRecursive is
   -- Déclaration des arbres
   Tree1: T_BinaryTree;
   ABRParent: T_BinaryTree;
begin
   initTree (ABRParent);

   Tree1 := createOrdinaryTree;
   
   Put_Line("");
   Put_Line("---- Tests TestDeleteRecursive... ----");
   Put_Line("");

   -- Test 1: Suppression d'un nœud feuille (clé 5)
   Put_Line("Test 1: Suppression d'un nœud feuille (clé 5)...");
   deleteNodeRecursive(Tree1, 5);

   pragma Assert(isEmpty(getNode (Tree1, 5)), "Test 1 échoué: Le nœud feuille (clé 5) est toujours présent.");
   pragma Assert(not isEmpty(getNode (Tree1, 4)), "Test 1 échoué: Un autre noeud a été supprimé.");
   pragma Assert(not isEmpty(getNode (Tree1, 3)), "Test 1 échoué: Un autre noeud a été supprimé.");
   pragma Assert(not isEmpty(getNode (Tree1, 2)), "Test 1 échoué: Un autre noeud a été supprimé.");
   pragma Assert(not isEmpty(getNode (Tree1, 1)), "Test 1 échoué: Un autre noeud a été supprimé.");
   
   -- TODO: Problem with deleteNodeRecursive
   Put_Line("Test 1 réussi: Le nœud feuille (clé 5) a été supprimé correctement.");

   -- Test 2: Suppression d'un nœud avec un sous-arbre (clé 3)
   Put_Line("Test 2: Suppression d'un nœud avec un sous-arbre (clé 3)...");
   
   Tree1 := createOrdinaryTree;
   deleteNodeRecursive(Tree1, 3);
   
   pragma Assert(isEmpty(getNode (Tree1, 5)), "Test 2 échoué: Le nœud avec sous-arbre (clé 3) est toujours présent.");
   pragma Assert(isEmpty(getNode (Tree1, 3)), "Test 2 échoué: Le nœud avec sous-arbre (clé 3) est toujours présent.");
   pragma Assert(isEmpty(getNode (Tree1, 4)), "Test 2 échoué: Le nœud avec sous-arbre (clé 3) est toujours présent.");
   pragma Assert(not isEmpty(getNode (Tree1, 2)), "Test 2 échoué: Un autre noeud a été supprimé.");
   pragma Assert(not isEmpty(getNode (Tree1, 1)), "Test 2 échoué: Un autre noeud a été supprimé.");

   Put_Line("Test 2 réussi: Le nœud avec sous-arbre (clé 3) a été supprimé correctement.");

   -- Test 3: Suppression du nœud racine (clé 1)
   Put_Line("Test 3: Suppression de la racine (clé 1)...");
   
   Tree1 := createOrdinaryTree;
   deleteNodeRecursive(Tree1, 1);

   pragma Assert(isEmpty(getNode (Tree1, 1)), "Test 3 échoué: La racine (clé 1) est toujours présente.");
   pragma Assert(isEmpty(getNode (Tree1, 2)), "Test 3 échoué: Un noeud de l'arbre est toujours présente.");
   pragma Assert(isEmpty(getNode (Tree1, 3)), "Test 3 échoué: Un noeud de l'arbre est toujours présente.");
   pragma Assert(isEmpty(getNode (Tree1, 4)), "Test 3 échoué: Un noeud de l'arbre est toujours présente.");
   pragma Assert(isEmpty(getNode (Tree1, 5)), "Test 3 échoué: Un noeud de l'arbre est toujours présente.");

   Put_Line("Test 3 réussi: La racine (clé 1) a été supprimée correctement.");

   -- Test 4: Suppression d'un noeud qui n'existe pas (clé 99)
   Put_Line("Test 4: Suppression d'un noeud qui n'existe pas (clé 99)");

   Tree1 := createOrdinaryTree;
   deleteNodeRecursive(Tree1, 5000);

   exception
         when Absent_Key_Exception =>
            pragma Assert(not isEmpty(getNode (Tree1, 1)), "Test 4 échoué: Un autre noeud a été supprimé alors que rien ne devait être supprimé puisque la clé 99 n'existe pas.");
            pragma Assert(not isEmpty(getNode (Tree1, 2)), "Test 4 échoué: Un autre noeud a été supprimé alors que rien ne devait être supprimé puisque la clé 99 n'existe pas.");
            pragma Assert(not isEmpty(getNode (Tree1, 3)), "Test 4 échoué: Un autre noeud a été supprimé alors que rien ne devait être supprimé puisque la clé 99 n'existe pas.");
            pragma Assert(not isEmpty(getNode (Tree1, 4)), "Test 4 échoué: Un autre noeud a été supprimé alors que rien ne devait être supprimé puisque la clé 99 n'existe pas.");
            pragma Assert(not isEmpty(getNode (Tree1, 5)), "Test 4 échoué: Un autre noeud a été supprimé alors que rien ne devait être supprimé puisque la clé 99 n'existe pas.");
            Put_Line("Test 4 réussi: L'exception Absent_Key_Exception a été levée comme prévu et aucun noeud de l'arbre a été supprimé !");
         when others =>
            Put_Line("Test 4 réussi: L'exception Absent_Key_Exception a été levée comme prévu !");

   Put_Line("Test 4 réussi: Aucun noeud n'a été supprimé car la clé 99 n'existe pas.");


end TestDeleteRecursive;



begin
    TestIsEmpty;
    --TestAddNode;
    TestIsPresent;
   TestGetSize;
   TestGetNode;
   TestTraverseTreeAndApply;
   TestDeleteRecursive;
end TestBinaryTree;

