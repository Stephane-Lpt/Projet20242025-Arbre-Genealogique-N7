with Ada.Text_IO; use Ada.Text_IO;
with Utils; use Utils;
with FamilyTree; use FamilyTree;

procedure TestFamilyTree is

   procedure TestGetGenerationsCount is
   begin 
      Null;
   end TestGetGenerationsCount;

   procedure TestGetAncestorsCount is
   begin 
      Null;
   end TestGetAncestorsCount;

   
   procedure TestGetAncestorsByGeneration is
      -- Déclaration des arbres
      Tree1: T_FamilyTree;
      AncestorsResult: TreeVector.Vector;
      ExpectedAncestors: TreeVector.Vector;
   begin
      -- Initialiser l'arbre
      initTree (Tree1);

      -- Créer un arbre ordinaire pour les tests
      Tree1 := createOrdinaryTree;

      Put_Line("");
      Put_Line("---- Tests TestGetAncestorsByGeneration... ----");
      Put_Line("");

      -- Test 1: Obtenir les ancêtres de la génération 1 d'un nœud (clé 5)
      Put_Line("Test 1: Obtenir les ancêtres de la génération 1 du nœud (clé 5)...");

      -- On cherche les ancêtres du nœud avec la clé 5 à la génération 1
      AncestorsResult := getAncestorsByGeneration(Tree1, 5, 1);

      -- Définir les ancêtres attendus pour ce test
      ExpectedAncestors := (Tree1.all.Right, Tree1.all); -- Ici, clé 3 et clé 2 sont les ancêtres de génération 1

      -- Vérifier que les ancêtres obtenus sont corrects
      pragma Assert(AncestorsResult = ExpectedAncestors, "Test 1 échoué: Les ancêtres obtenus ne correspondent pas à la génération 1.");

      Put_Line("Test 1 réussi: Les ancêtres de la génération 1 ont été obtenus correctement.");

      -- Test 2: Obtenir les ancêtres de la génération 2 du nœud (clé 5)
      Put_Line("Test 2: Obtenir les ancêtres de la génération 2 du nœud (clé 5)...");

      -- On cherche les ancêtres du nœud avec la clé 5 à la génération 2
      AncestorsResult := getAncestorsByGeneration(Tree1, 5, 2);

      -- Définir les ancêtres attendus pour ce test
      ExpectedAncestors := (Tree1.all); -- Ici, clé 2 est l'ancêtre de génération 2

      -- Vérifier que les ancêtres obtenus sont corrects
      pragma Assert(AncestorsResult = ExpectedAncestors, "Test 2 échoué: Les ancêtres obtenus ne correspondent pas à la génération 2.");

      Put_Line("Test 2 réussi: Les ancêtres de la génération 2 ont été obtenus correctement.");

      -- Test 3: Vérifier un cas où il n'y a pas d'ancêtres à la génération 3 pour un nœud (clé 5)
      Put_Line("Test 3: Vérifier les ancêtres de la génération 3 pour un nœud (clé 5)...");

      -- On cherche les ancêtres du nœud avec la clé 5 à la génération 3
      AncestorsResult := getAncestorsByGeneration(Tree1, 5, 3);

      -- Définir les ancêtres attendus pour ce test (aucun dans ce cas)
      ExpectedAncestors := (others => null);

      -- Vérifier que le résultat est vide
      pragma Assert(AncestorsResult = ExpectedAncestors, "Test 3 échoué: Aucune ancêtre à la génération 3, mais des ancêtres ont été trouvés.");

      Put_Line("Test 3 réussi: Aucun ancêtre à la génération 3 n'a été trouvé.");

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
   -- TestGetAncestorsCount;
   TestGetAncestorsByGeneration;
   -- TestGetSingleParentIndividuals;
   -- TestGetDualParentIndividuals;

end TestFamilyTree;

