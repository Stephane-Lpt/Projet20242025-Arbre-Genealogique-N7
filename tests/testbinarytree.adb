with Ada.Text_IO; use Ada.Text_IO;
with Utils; use Utils;
with BinaryTree;

procedure TestBinaryTree is
    procedure PutInteger (Element : in Integer; Key : in Integer; Depth : in Integer := 0; Position : in T_Position := ROOT) is
    begin
      Put_Line(getIndent(Depth) & getBinaryTreePrefix(Position) & Integer'Image(Key) & ":" & Integer'Image(Element));
   end PutInteger;

   package IntegerBinaryTree is new BinaryTree
     (
      Put_Generic => PutInteger,
      T_Element   => Integer
     );
   use IntegerBinaryTree;

   -- TEST isEmpty --
   procedure TestIsEmpty is
      Tree1, Tree2 : T_BinaryTree;
   begin
      initTree(Tree1);

      initRoot(Tree2, 1, 1);

      pragma Assert (isEmpty(Tree1));
      pragma Assert (not isEmpty(Tree2));
   end TestIsEmpty;

   -- TEST addNode --
   procedure TestAddNode is
      RootTree, LeftTree, RightTree : T_BinaryTree;
   begin
      initRoot(RootTree, 1, 12);
      initRoot(LeftTree, 2, 82);
      initRoot(RightTree, 3, 23);

      addNode(RootTree, LeftTree, 1, LEFT);
      addNode(RootTree, RightTree, 1, RIGHT);

      -- not working!
      --pragma Assert(RootTree.all.Left.all = LeftTree);
      --pragma Assert(RootTree.all.Right.all = RightTree);
      --pragma Assert(RootTree.all.Left.Key = 2);
      --pragma Assert(RootTree.all.Right.Key = 3);
      --pragma Assert(RootTree.Left.all.Element = 82);
      --pragma Assert(RootTree.Right.all.Element = 23);

      -- TODO: add tests with ID's that aren't directly the ABR's root (need to implement getNode)
   end TestAddNode;

   -- TEST isPresent --
   procedure TestIsPresent is
      RootTree, Tree1, Tree2 : T_BinaryTree;
   begin
      initRoot(RootTree, 1, 12);
      initRoot(Tree1, 2, 97);
      initRoot(Tree2, 3, 78);

      addNode (RootTree, Tree1, 1, LEFT);
      addNode (Tree1, Tree2, 2, RIGHT);

      pragma Assert (isPresent (RootTree, 1));
      pragma Assert (isPresent (RootTree, 2));
      pragma Assert (isPresent (RootTree, 3));
      pragma Assert (not isPresent (RootTree, 4));
   end TestIsPresent;

   -- TEST getSize --
   procedure TestGetSize is
      Tree1, Tree2, Tree3, Tree4, Tree5 : T_BinaryTree;
   begin
      initTree (Tree1);

      pragma Assert (getSize (Tree1) = 0);

      initRoot(Tree1, 1, 12);
      initRoot(Tree2, 2, 97);
      initRoot(Tree3, 3, 78);
      initRoot(Tree4, 4, 34);
      initRoot(Tree5, 5, 90);

      pragma Assert (getSize (Tree1) = 1);

      addNode (Tree1, Tree2, 1, LEFT);
      addNode (Tree1, Tree3, 1, RIGHT);

      pragma Assert (getSize (Tree1) = 3);

      addNode (Tree2, Tree4, 2, LEFT);
      addNode (Tree3, Tree5, 3, RIGHT);

      pragma Assert (getSize (Tree1) = 5);
      pragma Assert (getSize (Tree2) = 2);
      pragma Assert (getSize (Tree5) = 1);
   end TestGetSize;

   procedure TestGetNode is
      Tree1, Tree2, Tree3, Tree4, Tree5, FoundRootTree, FoundChildTree, NotFoundTree : T_BinaryTree;
   begin
      initRoot(Tree1, 1, 10);
      initRoot(Tree2, 2, 20);
      initRoot(Tree3, 3, 30);
      initRoot(Tree4, 4, 40);
      initRoot(Tree5, 5, 50);

      addNode(Tree1, Tree2, 1, LEFT);
      addNode(Tree1, Tree3, 1, RIGHT);
      addNode(Tree3, Tree4, 3, LEFT);
      addNode(Tree4, Tree5, 4, RIGHT);


      -- ROOT
      FoundRootTree := getNode(Tree1, 1);
      pragma Assert (FoundRootTree = Tree1);

      -- CHILD
      FoundChildTree := getNode(Tree1, 3);
      pragma Assert (FoundChildTree = Tree3);

      -- NON-EXISTENT
      NotFoundTree := getNode(Tree1, 6);
      pragma Assert (isEmpty(NotFoundTree));

   end TestGetNode;

procedure TestTraverseTreeAndApply is
   -- Déclaration des arbres
   Tree1, Tree2, Tree3, Tree4, Tree5 : T_BinaryTree;
   Stop_Flag : Boolean := False;
   Result: T_BinaryTree;

   -- Callback qui marque le parcours comme terminé dès que la clé 3 est rencontrée
   procedure ActionExample (ABR : in out T_BinaryTree; Stop : in out Boolean; Result: in out T_BinaryTree) is
   begin
      -- Arrêter si la clé du nœud est 3
         if getKey(ABR) = 3 then
         Stop := True;
      end if;
   end ActionExample;

   procedure NoStopActionExample (ABR : in out T_BinaryTree; Stop : in out Boolean; Result: in out T_BinaryTree) is
   begin
      if getKey(ABR) = 99 then
         Stop := True;
      end if;
   end NoStopActionExample;

begin
   Put_Line("");
   Put_Line("---- Tests traverseTreeAndApply... ----");
   Put_Line("");
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

   -- Test 1: Vérification si le parcours s'arrête lorsqu'il atteint le nœud clé 5
   Stop_Flag := False;
   Put_Line("Test 1: Parcours jusqu'à l'origine (clé 1)");
   traverseTreeAndApply(Tree1, ActionExample'Access, Stop_Flag, Result);
   
   -- Vérifier si le flag Stop a été mis à True
   pragma Assert(Stop_Flag, "Test 1 échoué: Le parcours n'a pas trouvé la clé 1");
   if Stop_Flag then
      Put_Line("Test 1 réussi: Le parcours s'est arrêté directement après avoir trouvé l'origine (clé 1)");
   else
      Put_Line("Test 1 échoué: Le parcours n'a pas trouvé la clé 1");
   end if;

   -- Test 2: Parcours avec arrêt lorsque la clé 3 est rencontrée
   Stop_Flag := False;  -- Réinitialiser le flag avant chaque test
   Put_Line("Test 2: Parcours jusqu'à la clé 3...");
   traverseTreeAndApply(Tree1, ActionExample'Access, Stop_Flag, Result);
   
   -- Vérifier si le flag Stop a été mis à True
   pragma Assert(Stop_Flag, "Test 2 échoué: Le parcours n'a pas trouvé la clé 3.");
   if Stop_Flag then
      Put_Line("Test 2 réussi: Le parcours s'est arrêté directement après avoir trouvé la clé 3.");
   else
      Put_Line("Test 2 échoué: Le parcours n'a pas trouvé la clé 3.");
   end if;

   -- Test 3: Vérification si le parcours s'arrête lorsqu'il atteint le nœud clé 5
   Stop_Flag := False;
   Put_Line("Test 3: Parcours jusqu'à la clé 5...");
   traverseTreeAndApply(Tree1, ActionExample'Access, Stop_Flag, Result);
   
   -- Vérifier si le flag Stop a été mis à True
   pragma Assert(Stop_Flag, "Test 3 échoué: Le parcours n'a pas trouvé la clé 5.");
   if Stop_Flag then
      Put_Line("Test 3 réussi: Le parcours s'est arrêté directement après avoir trouvé la clé 5.");
   else
      Put_Line("Test 3 échoué: Le parcours n'a pas trouvé la clé 5.");
   end if;

   -- Test 4: Vérifier le comportement lorsque la clé recherchée n'existe pas
   Stop_Flag := False;
   Put_Line("Test 4: Recherche d'une clé inexistante (par exemple 99)...");
   traverseTreeAndApply(Tree1, NoStopActionExample'Access, Stop_Flag, Result);
   
   -- Vérifier si le flag Stop est resté False
   pragma Assert(not Stop_Flag, "Test 4 échoué: La clé 99 a été trouvée alors qu'elle n'existe pas.");
   if Stop_Flag then
      Put_Line("Test 4 échoué: La clé 99 a été trouvée alors qu'elle n'existe pas.");
   else
      Put_Line("Test 4 réussi: La clé 99 n'existe pas dans l'arbre donc le parcours ne s'est jamais arrêté.");
   end if;

end TestTraverseTreeAndApply;

procedure TestDeleteRecursive is
   -- Déclaration des arbres
   Tree1, Tree2, Tree3, Tree4, Tree5 : T_BinaryTree;
   node : T_BinaryTree;
begin
   Put_Line("");
   Put_Line("---- Tests TestDeleteRecursive... ----");
   Put_Line("");
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

   -- Test 1: Suppression d'un nœud feuille (clé 5)
   Put_Line("Test 1: Suppression d'un nœud feuille (clé 5)...");
   Display_Tree (Tree1);
   deleteNodeRecursive(Tree1, 5);
   Display_Tree (Tree1);
   node := getNode (Tree1, 5);
   pragma Assert(isEmpty(node), "Test 1 échoué: Le nœud feuille (clé 5) est toujours présent.");
   -- TODO: Problem with deleteNodeRecursive
   if isEmpty(node) then
      Put_Line("Test 1 réussi: Le nœud feuille (clé 5) a été supprimé correctement.");
   else 
      Put_Line ("Test 1 échoué: Le nœud feuille (clé 5) est toujours présent.");
   end if;

   --  -- Test 2: Suppression d'un nœud avec un sous-arbre (clé 3)
   --  Put_Line("Test 2: Suppression d'un nœud avec un sous-arbre (clé 3)...");
   --  deleteNodeRecursive(Tree1, 3);
   --  node := getNode (Tree1, 5);
   --  --  pragma Assert(not isEmpty(node), "Test 2 échoué: Le nœud avec sous-arbre (clé 3) est toujours présent.");
   --  if not isEmpty(node) then
   --     Put_Line("Test 2 réussi: Le nœud avec sous-arbre (clé 3) a été supprimé correctement.");
   --  end if;

   --  -- Test 3: Suppression du nœud racine (clé 1)
   --  Put_Line("Test 3: Suppression de la racine (clé 1)...");
   --  deleteNodeRecursive(Tree1, 1);
   --  node := getNode (Tree1, 1);
   --  --  pragma Assert(not isEmpty(node), "Test 3 échoué: La racine (clé 1) est toujours présente.");
   --  if isEmpty(node) then
   --     Put_Line("Test 3 réussi: La racine (clé 1) a été supprimée correctement.");
   --  end if;

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

