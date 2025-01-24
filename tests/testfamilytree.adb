with Ada.Text_IO; use Ada.Text_IO;
with Utils; use Utils;
with FamilyTree; use FamilyTree;
with Person; use Person;
use type FamilyTree.TreeVector.Vector;

procedure TestFamilyTree is

   -- ORDINARY FAMILY TREE : 
   --     / 1
   --        > 3
   --          < 4
   --        < 2
   --          > 5   
   function createOrdinaryFamilyTree return T_FamilyTree is
      FamilyTree : T_FamilyTree;
   begin
      -- Initialisation des arbres
      initChild(FamilyTree, 1, initPersonObj);
      addAncestor(FamilyTree, 1, LEFT, 2, initPersonObj);
      addAncestor(FamilyTree, 1, RIGHT, 3, initPersonObj);
      addAncestor(FamilyTree, 3, LEFT, 4, initPersonObj);
      addAncestor(FamilyTree, 4, RIGHT, 5, initPersonObj);

      return FamilyTree;
   end createOrdinaryFamilyTree;

   procedure TestGetGenerationsCount is
      TestTree : T_FamilyTree;
      TempTree : T_FamilyTree;


      function Create_Complex_Tree return T_FamilyTree is
         FT : T_FamilyTree;
      begin
         initChild(FT, 1, initPersonObj);
         addAncestor(FT, 1, RIGHT, 3, initPersonObj);
         addAncestor(FT, 3, LEFT, 4, initPersonObj);
         addAncestor(FT, 4, RIGHT, 5, initPersonObj);
         return FT;
      end Create_Complex_Tree;
   begin
      Put_Line("");
      Put_Line("---- Tests GetGenerationsCount... ----");
      Put_Line("");
      -- ##########################################################
      Put_Line("Test 1: Arbre vide");
      initFamilyTree (TestTree);
      pragma Assert(
         getGenerationsCount(TestTree) = 0, 
         "Échec Test 1 - Resultat: " & Integer'Image(getGenerationsCount(TestTree))
      );
      Put_Line("✓ Test 1 réussi");
      clean(TestTree);

      -- ##########################################################
      Put_Line("Test 2: Arbre à 1 nœud");s
      initChild(TestTree, 1, initPersonObj);
      pragma Assert(
         getGenerationsCount(TestTree) = 1,
         "Échec Test 2 - Resultat: " & Integer'Image(getGenerationsCount(TestTree))
      );
      Put_Line("✓ Test 2 réussi");
      clean(TestTree);
   

      -- ##########################################################
      Put_Line("Test 3: Arbre à 2 générations");
      initChild(TestTree, 1, initPersonObj);
      addAncestor(TestTree, 1, LEFT, 2, initPersonObj);
      pragma Assert(
         getGenerationsCount(TestTree) = 2,
         "Échec Test 3 - Resultat: " & Integer'Image(getGenerationsCount(TestTree))
      );
      Put_Line("✓ Test 3 réussi");
      clean(TestTree);

      -- ##########################################################
      Put_Line("Test 4: Arbre complexe (4 générations)");
      TestTree := Create_Complex_Tree;
      pragma Assert(
         getGenerationsCount(TestTree) = 4,
         "Échec Test 4 - Resultat: " & Integer'Image(getGenerationsCount(TestTree))
      );
      Put_Line("✓ Test 4 réussi");

      -- ##########################################################
      Put_Line("Test 5: Arbre déséquilibré gauche");
      initChild(TempTree, 10, initPersonObj);
      addAncestor(TempTree, 10, LEFT, 20, initPersonObj);
      addAncestor(TempTree, 20, LEFT, 30, initPersonObj);
      pragma Assert(
         getGenerationsCount(TempTree) = 3,
         "Échec Test 5 - Resultat: " & Integer'Image(getGenerationsCount(TempTree))
      );
      Put_Line("✓ Test 5 réussi");
      clean(TempTree);

   end TestGetGenerationsCount;

   procedure TestGetAncestorsCount is
      Family : T_FamilyTree;
   begin 
      Family := createOrdinaryFamilyTree;

         Put_Line("");
         Put_Line("---- Tests GetAncestorsCount... ----");
         Put_Line("");


      -- ##########################################################
      Put_Line("Test 1: Comptage ancêtres racine (clé 1)");
      pragma Assert(
         getAncestorsCount(Family, 1) = 5,
         "Échec Test 1: Devrait retourner 5 ancêtres (1+2+3+4+5)"
      );
      Put_Line("✓ Test 1 réussi");

      -- ##########################################################
      Put_Line("Test 2: Comptage sous-arbre mère (clé 3)");
      pragma Assert(
         getAncestorsCount(Family, 3) = 3,
         "Échec Test 2: Devrait retourner 3 ancêtres (3+4+5)"
      );
      Put_Line("✓ Test 2 réussi");

      -- ##########################################################
      Put_Line("Test 3: Feuille de l'arbre (clé 5)");
      pragma Assert(
         getAncestorsCount(Family, 5) = 1,
         "Échec Test 3: Devrait retourner 1 (nœud seul)"
      );
      Put_Line("✓ Test 3 réussi");

      -- ##########################################################
      Put_Line("Test 4: Branche vide (clé 2)");
      pragma Assert(
         getAncestorsCount(Family, 2) = 1,
         "Échec Test 4: Devrait retourner 1 (père sans ascendants)"
      );
      Put_Line("✓ Test 4 réussi");

      -- ##########################################################
      Put_Line("Test 5: Clé invalide");
      pragma Assert(
         getAncestorsCount(Family, 999) = 0,
         "Échec Test 5: Devrait retourner 0 pour clé inexistante"
      );
      Put_Line("✓ Test 5 réussi");
   end TestGetAncestorsCount;

   procedure TestGetAncestorsByGeneration is
      AncestorsResult : TreeVector.Vector;
      ExpectedAncestors : TreeVector.Vector;
      Family : T_FamilyTree;
   begin
      Family := createOrdinaryFamilyTree;
      Put_Line("");
      Put_Line("---- Tests GetAncestorsByGeneration... ----");
      Put_Line("");
      -- ##########################################################
      -- Test 1: Génération 1 depuis la clé 1
      -- ##########################################################
      Put_Line("Test 1: Ancêtres génération 1 (clé 1)");
      
      -- Résultat attendu : [2, 3]
      ExpectedAncestors.Append(getFamilyNode(Family, 2));
      ExpectedAncestors.Append(getFamilyNode(Family, 3));
      
      AncestorsResult := getAncestorsByGeneration(Family, 1, 1);
      
      pragma Assert(AncestorsResult = ExpectedAncestors, 
                   "Test 1 échoué : Mauvais parents directs");
      Put_Line("Test 1 réussi ✓");
      ExpectedAncestors.Clear;

      -- ##########################################################
      -- Test 2: Génération 2 depuis la clé 1
      -- ##########################################################
      Put_Line("Test 2: Ancêtres génération 2 (clé 1)");
      
      -- Résultat attendu : [4]
      ExpectedAncestors.Append(getFamilyNode(Family, 4));
      
      AncestorsResult := getAncestorsByGeneration(Family, 1, 2);
      
      pragma Assert(AncestorsResult = ExpectedAncestors and getLength(AncestorsResult) = 1, 
                   "Test 2 échoué : Mauvais grands-parents");
      Put_Line("Test 2 réussi ✓");
      ExpectedAncestors.Clear;

      -- ##########################################################
      -- Test 3: Génération 3 depuis la clé 1
      -- ##########################################################
      Put_Line("Test 3: Ancêtres génération 3 (clé 1)");
      
      -- Résultat attendu : [5]
      ExpectedAncestors.Append(getFamilyNode(Family, 5));
      
      AncestorsResult := getAncestorsByGeneration(Family, 1, 3);
      
      pragma Assert(AncestorsResult = ExpectedAncestors, 
                   "Test 3 échoué : Mauvais arrière-grand-parent");
      Put_Line("Test 3 réussi ✓");
      ExpectedAncestors.Clear;

      -- ##########################################################
      -- Test 4: Clé invalide
      -- ##########################################################
      Put_Line("Test 4: Clé inexistante (999)");
      AncestorsResult := getAncestorsByGeneration(Family, 999, 1);
      pragma Assert(AncestorsResult.Is_Empty, 
                   "Test 4 échoué : Résultat non vide");
      Put_Line("Test 4 réussi ✓");

      -- ##########################################################
      -- Test 5: Génération négative
      -- ##########################################################
      Put_Line("Test 5: Génération négative (-5)");
      AncestorsResult := getAncestorsByGeneration(Family, 1, -5);
      pragma Assert(AncestorsResult.Is_Empty, 
                   "Test 5 échoué : Résultat devrait être vide");
      Put_Line("Test 5 réussi ✓");
      
   end TestGetAncestorsByGeneration;

   procedure TestGetSingleParentIndividuals is
   begin 
      Null;
   end TestGetSingleParentIndividuals;

   procedure TestGetDualParentIndividuals is
   begin 
      Null;
   end TestGetDualParentIndividuals;

begin
   TestGetGenerationsCount;
   TestGetAncestorsCount;
   TestGetAncestorsByGeneration;
   -- TestGetSingleParentIndividuals;
   -- TestGetDualParentIndividuals;

end TestFamilyTree;

