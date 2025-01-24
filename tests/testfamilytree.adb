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
   begin 
      Null;
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
   -- TestGetGenerationsCount;
   TestGetAncestorsCount;
   TestGetAncestorsByGeneration;
   -- TestGetSingleParentIndividuals;
   -- TestGetDualParentIndividuals;

end TestFamilyTree;

