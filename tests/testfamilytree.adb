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

   procedure TestDeleteAncestor is
      TestTree : T_FamilyTree;
      
      function createOrdinaryFamilyTree return T_FamilyTree is
         FT : T_FamilyTree;
      begin
         initChild(FT, 1, initPersonObj);
         addAncestor(FT, 1, LEFT, 2, initPersonObj);
         addAncestor(FT, 1, RIGHT, 3, initPersonObj);
         addAncestor(FT, 3, LEFT, 4, initPersonObj);
         addAncestor(FT, 4, RIGHT, 5, initPersonObj);
         return FT;
      end createOrdinaryFamilyTree;

   begin
      New_Line;
      Put_Line("---- Tests DeleteAncestor... ----");
      New_Line;
      New_Line;
      -- ##########################################################
      Put_Line("Test 1: Suppression feuille (clé 5)");
      TestTree := createOrdinaryFamilyTree;
      deleteAncestor(TestTree, 5);
      
      pragma Assert(
         not isPresent(TestTree, 5) and 
         isPresent(TestTree, 4) and 
         isPresent(TestTree, 3),
         "Échec suppression feuille"
      );
      Put_Line("✓ Test 1 réussi");
      clean(TestTree);

      -- ##########################################################
      Put_Line("Test 2: Suppression sous-arbre (clé 3)");
      TestTree := createOrdinaryFamilyTree;
      deleteAncestor(TestTree, 3);
      
      pragma Assert(
         not isPresent(TestTree, 3) and 
         not isPresent(TestTree, 4) and 
         not isPresent(TestTree, 5) and 
         isPresent(TestTree, 1) and 
         isPresent(TestTree, 2),
         "Échec suppression sous-arbre"
      );
      Put_Line("✓ Test 2 réussi");
      clean(TestTree);

      -- ##########################################################
      Put_Line("Test 3: Suppression racine (clé 1)");
      TestTree := createOrdinaryFamilyTree;
      deleteAncestor(TestTree, 1);
      
      pragma Assert(
         isEmpty(TestTree),
         "Échec suppression racine"
      );
      Put_Line("✓ Test 3 réussi");

      -- ##########################################################
      Put_Line("Test 4: Clé inexistante (999)");
      TestTree := createOrdinaryFamilyTree;
      deleteAncestor(TestTree, 999);  -- Ne devrait rien modifier
      
      exception
         when Absent_Key_Exception =>
            pragma Assert(not isEmpty(getNode (TestTree, 1)), "Test 4 échoué: Un autre noeud a été supprimé alors que rien ne devait être supprimé puisque la clé 99 n'existe pas.");
            pragma Assert(not isEmpty(getNode (TestTree, 2)), "Test 4 échoué: Un autre noeud a été supprimé alors que rien ne devait être supprimé puisque la clé 99 n'existe pas.");
            pragma Assert(not isEmpty(getNode (TestTree, 3)), "Test 4 échoué: Un autre noeud a été supprimé alors que rien ne devait être supprimé puisque la clé 99 n'existe pas.");
            pragma Assert(not isEmpty(getNode (TestTree, 4)), "Test 4 échoué: Un autre noeud a été supprimé alors que rien ne devait être supprimé puisque la clé 99 n'existe pas.");
            pragma Assert(not isEmpty(getNode (TestTree, 5)), "Test 4 échoué: Un autre noeud a été supprimé alors que rien ne devait être supprimé puisque la clé 99 n'existe pas.");
            Put_Line("Test 4 réussi: L'exception Absent_Key_Exception a été levée comme prévu et aucun noeud de l'arbre a été supprimé !");
         when others =>
            Put_Line("Test 4 réussi: L'exception Absent_Key_Exception a été levée comme prévu !");

      Put_Line("Test 4 réussi: Aucun noeud n'a été supprimé car la clé 99 n'existe pas.");

   end TestDeleteAncestor;

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
      New_Line;
      Put_Line("---- Tests GetGenerationsCount... ----");
      New_Line;
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
      Put_Line("Test 2: Arbre à 1 nœud");
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

         New_Line;
         Put_Line("---- Tests GetAncestorsCount... ----");
         New_Line;


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
      New_Line;
      Put_Line("---- Tests GetAncestorsByGeneration... ----");
      New_Line;
      -- ##########################################################
      -- Test 1: Génération 1 depuis la clé 1
      -- ##########################################################
      Put_Line("Test 1: Ancêtres génération 1 (clé 1)");
      
      -- Résultat attendu : [2, 3]
      ExpectedAncestors.Append(getNode(Family, 2));
      ExpectedAncestors.Append(getNode(Family, 3));
      
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
      ExpectedAncestors.Append(getNode(Family, 4));
      
      AncestorsResult := getAncestorsByGeneration(Family, 1, 2);
      
      pragma Assert(AncestorsResult = ExpectedAncestors and Length(AncestorsResult) = 1, 
                   "Test 2 échoué : Mauvais grands-parents");
      Put_Line("Test 2 réussi ✓");
      ExpectedAncestors.Clear;

      -- ##########################################################
      -- Test 3: Génération 3 depuis la clé 1
      -- ##########################################################
      Put_Line("Test 3: Ancêtres génération 3 (clé 1)");
      
      -- Résultat attendu : [5]
      ExpectedAncestors.Append(getNode(Family, 5));
      
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

   procedure TestGetOrphanIndividuals is
      TestTree : T_FamilyTree;
      Result   : TreeVector.Vector;
      Expected : TreeVector.Vector;
      EmptyTree : T_FamilyTree;

      procedure Build_Test_Tree_1 is
      begin
         initChild(TestTree, 1, initPersonObj); -- [1] seul
      end Build_Test_Tree_1;

      procedure Build_Test_Tree_2 is
      begin
         initChild(TestTree, 1, initPersonObj);
         addAncestor(TestTree, 1, LEFT, 2, initPersonObj); -- 1 -> 2 (père)
      end Build_Test_Tree_2;

      procedure Build_Test_Tree_3 is
      begin
         initChild(TestTree, 1, initPersonObj); -- 1 a 2 parents
         addAncestor(TestTree, 1, LEFT, 2, initPersonObj); -- 2 a 0 parents
         addAncestor(TestTree, 1, RIGHT, 3, initPersonObj); -- 3 a 0 parents
      end Build_Test_Tree_3;

      procedure Build_Complex_Test_Tree is
      begin
         initChild(TestTree, 1, initPersonObj);
         -- Niveau 1
         addAncestor(TestTree, 1, LEFT, 2, initPersonObj);   -- Père de 1
         addAncestor(TestTree, 1, RIGHT, 3, initPersonObj);  -- Mère de 1
         
         -- Niveau 2
         addAncestor(TestTree, 2, LEFT, 4, initPersonObj);   -- Père de 2
         addAncestor(TestTree, 3, RIGHT, 5, initPersonObj);  -- Mère de 3
         
         -- Niveau 3
         addAncestor(TestTree, 4, LEFT, 6, initPersonObj);   -- Père de 4, PAS DE PARENTS
         addAncestor(TestTree, 5, RIGHT, 7, initPersonObj);  -- Mère de 5
         
         -- Niveau 4 avec mélange de cas
         addAncestor(TestTree, 7, LEFT, 8, initPersonObj);   -- Père de 7, PAS DE PARENTS
         addAncestor(TestTree, 7, RIGHT, 9, initPersonObj); -- Mère de 7
         addAncestor(TestTree, 9, RIGHT, 10, initPersonObj); -- Père de 10, PAS DE PARENTS
      end Build_Complex_Test_Tree;

      function Key_In_Result(Key : Integer) return Boolean is
      begin
         for E of Result loop
            if getKey(E) = Key then
               return True;
            end if;
         end loop;
         return False;
      end Key_In_Result;

   begin  
      New_Line;
      Put_Line("---- Tests GetOrphanIndividuals... ----");
      New_Line;
      -- ##########################################################
      Put_Line("Test 1: Arbre vide");
      initFamilyTree(EmptyTree);
      Result := getOrphanIndividuals(EmptyTree, 1);
      pragma Assert(Length(Result) = 0, "Test 1 échoué");
      Put_Line("✓ Test 1 réussi");

      -- ##########################################################
      Put_Line("Test 2: Nœud unique sans parents");
      Build_Test_Tree_1;
      Result := getOrphanIndividuals(TestTree, 1);
      pragma Assert(
         Length(Result) = 1 and 
         getKey(First_Element(Result)) = 1,
         "Test 2 échoué"
      );
      Put_Line("✓ Test 2 réussi");
      clean(TestTree);

      -- ##########################################################
      Put_Line("Test 3: Nœud avec 1 parent (gauche)");
      Build_Test_Tree_2;
      Result := getOrphanIndividuals(TestTree, 1);
      pragma Assert(
         Length(Result) = 1 and 
         getKey(First_Element(Result)) = 2,
         "Test 3 échoué"
      );
      Put_Line("✓ Test 3 réussi");
      clean(TestTree);

      -- ##########################################################
      Put_Line("Test 4: Arbre complexe avec multiples cas");
      Build_Test_Tree_3;
      Result := getOrphanIndividuals(TestTree, 1);
      
      Expected.Append(getNode(TestTree, 2)); -- 2 a 0 parents
      Expected.Append(getNode(TestTree, 3)); -- 3 a 0 parents
      
      pragma Assert(
         Length(Result) = 2 and
         Result = Expected,
         "Test 4 échoué"
      );
      Put_Line("✓ Test 4 réussi");
      clean(TestTree);

      -- ##########################################################
      Put_Line("Test 5: Clé inexistante");
      Build_Test_Tree_2;
      Result := getOrphanIndividuals(TestTree, 999);
      pragma Assert(Length(Result) = 0, "Test 5 échoué");
      Put_Line("✓ Test 5 réussi");
      clean(TestTree);

      -- ##########################################################
      Put_Line("Test 6: Arbre complexe avec multiples cas");
      Build_Complex_Test_Tree;
      Result := getOrphanIndividuals(TestTree, 1);

      -- Résultats attendus :
      -- 6, PAS DE PARENTS
      -- 8, PAS DE PARENTS
      -- 10, PAS DE PARENTS
      
      pragma Assert(
         Length(Result) = 3 and
         Key_In_Result(6) and
         Key_In_Result(8) and
         Key_In_Result(10),
         "Test 6 échoué - Résultat: " & Integer'Image(Length(Result))
      );
      
      Put_Line("✓ Test 6 réussi");
      clean(TestTree);
   end TestGetOrphanIndividuals;

   procedure TestGetSingleParentIndividuals is
      TestTree : T_FamilyTree;
      Result   : TreeVector.Vector;
      Expected : TreeVector.Vector;
      EmptyTree : T_FamilyTree;

      procedure Build_Test_Tree_1 is
      begin
         initChild(TestTree, 1, initPersonObj); -- [1] seul
      end Build_Test_Tree_1;

      procedure Build_Test_Tree_2 is
      begin
         initChild(TestTree, 1, initPersonObj);
         addAncestor(TestTree, 1, LEFT, 2, initPersonObj); -- 1 -> 2 (père)
      end Build_Test_Tree_2;

      procedure Build_Test_Tree_3 is
      begin
         initChild(TestTree, 1, initPersonObj);
         addAncestor(TestTree, 1, LEFT, 2, initPersonObj);
         addAncestor(TestTree, 1, RIGHT, 3, initPersonObj); -- 1 a 2 parents
         addAncestor(TestTree, 2, LEFT, 4, initPersonObj);   -- 2 a 1 parent
      end Build_Test_Tree_3;

      procedure Build_Complex_Test_Tree is
      begin
         initChild(TestTree, 1, initPersonObj);
         -- Niveau 1
         addAncestor(TestTree, 1, LEFT, 2, initPersonObj);   -- Père de 1
         addAncestor(TestTree, 1, RIGHT, 3, initPersonObj);  -- Mère de 1
         
         -- Niveau 2
         addAncestor(TestTree, 2, LEFT, 4, initPersonObj);   -- Père de 2 (seul parent)
         addAncestor(TestTree, 3, RIGHT, 5, initPersonObj);  -- Mère de 3 (seul parent)
         
         -- Niveau 3
         addAncestor(TestTree, 4, LEFT, 6, initPersonObj);   -- Père de 4
         addAncestor(TestTree, 5, RIGHT, 7, initPersonObj);  -- Mère de 5
         
         -- Niveau 4 avec mélange de cas
         addAncestor(TestTree, 6, LEFT, 8, initPersonObj);   -- Père de 6 (seul parent)
         addAncestor(TestTree, 7, LEFT, 9, initPersonObj);   -- Père de 7
         addAncestor(TestTree, 7, RIGHT, 10, initPersonObj); -- Mère de 7
      end Build_Complex_Test_Tree;

      function Key_In_Result(Key : Integer) return Boolean is
      begin
         for E of Result loop
            if getKey(E) = Key then
               return True;
            end if;
         end loop;
         return False;
      end Key_In_Result;


   begin  
      New_Line;
      Put_Line("---- Tests GetSingleParentIndividuals... ----");
      New_Line;
      -- ##########################################################
      Put_Line("Test 1: Arbre vide");
      initFamilyTree(EmptyTree);
      Result := getSingleParentIndividuals(EmptyTree, 1);
      pragma Assert(Length(Result) = 0, "Test 1 échoué");
      Put_Line("✓ Test 1 réussi");

      -- ##########################################################
      Put_Line("Test 2: Nœud unique sans parents");
      Build_Test_Tree_1;
      Result := getSingleParentIndividuals(TestTree, 1);
      pragma Assert(Length(Result) = 0, "Test 2 échoué");
      Put_Line("✓ Test 2 réussi");
      clean(TestTree);

      -- ##########################################################
      Put_Line("Test 3: Nœud avec 1 parent (gauche)");
      Build_Test_Tree_2;
      Result := getSingleParentIndividuals(TestTree, 1);
      pragma Assert(
         Length(Result) = 1 and 
         getKey(First_Element(Result)) = 1,
         "Test 3 échoué"
      );
      Put_Line("✓ Test 3 réussi");
      clean(TestTree);

      -- ##########################################################
      Put_Line("Test 4: Nœud avec 1 parent (droit)");
      initChild(TestTree, 1, initPersonObj);
      addAncestor(TestTree, 1, RIGHT, 5, initPersonObj); -- 1 -> 5 (mère)
      Result := getSingleParentIndividuals(TestTree, 1);
      pragma Assert(
         Length(Result) = 1 and 
         getKey(First_Element(Result)) = 1,
         "Test 4 échoué"
      );
      Put_Line("✓ Test 4 réussi");
      clean(TestTree);

      -- ##########################################################
      Put_Line("Test 5: Arbre complexe avec multiples cas");
      Build_Test_Tree_3;
      Result := getSingleParentIndividuals(TestTree, 1);
      
      Expected.Append(getNode(TestTree, 2)); -- 2 a 1 parent (4)
      
      pragma Assert(
         Length(Result) = 1 and
         Result = Expected,
         "Test 5 échoué"
      );
      Put_Line("✓ Test 5 réussi");
      clean(TestTree);

      -- ##########################################################
      Put_Line("Test 6: Clé inexistante");
      Build_Test_Tree_2;
      Result := getSingleParentIndividuals(TestTree, 999);
      pragma Assert(Length(Result) = 0, "Test 6 échoué");
      Put_Line("✓ Test 6 réussi");
      clean(TestTree);

      -- ##########################################################
      Put_Line("Test 7: Arbre complexe avec multiples cas");
      Build_Complex_Test_Tree;
      Result := getSingleParentIndividuals(TestTree, 1);

      -- Résultats attendus :
      -- 3 (n'a que le parent 5)
      -- 5 (n'a que le parent 7)
      -- 2 (n'a que le parent 4) 
      -- 4 (n'a que le parent 6)
      -- 6 (n'a que le parent 8)
      
      pragma Assert(
         Length(Result) = 5 and
         Key_In_Result(2) and
         Key_In_Result(5) and
         Key_In_Result(2) and
         Key_In_Result(4) and
         Key_In_Result(6),
         "Test 7 échoué - Résultat: " & Integer'Image(Length(Result))
      );
      
      Put_Line("✓ Test 7 réussi");
      clean(TestTree);
   end TestGetSingleParentIndividuals;

   procedure TestGetDualParentIndividuals is
      TestTree : T_FamilyTree;
      Result   : TreeVector.Vector;
      Expected : TreeVector.Vector;

      -- Arbre complexe de référence
      procedure Build_Complex_Tree is
      begin
         initFamilyTree(TestTree);
         initChild(TestTree, 1, initPersonObj);
         
         -- Niveau 1 (racine avec 2 parents)
         addAncestor(TestTree, 1, LEFT, 2, initPersonObj);
         addAncestor(TestTree, 1, RIGHT, 3, initPersonObj);
         
         -- Niveau 2 
         addAncestor(TestTree, 2, LEFT, 4, initPersonObj);  -- 2 a 1 parent
         addAncestor(TestTree, 3, LEFT, 5, initPersonObj);   -- 3 a 1 parent
         addAncestor(TestTree, 3, RIGHT, 6, initPersonObj);  -- 3 a 2 parents
         
         -- Niveau 3
         addAncestor(TestTree, 6, LEFT, 7, initPersonObj);   -- 6 a 2 parents
         addAncestor(TestTree, 6, RIGHT, 8, initPersonObj);
      end Build_Complex_Tree;

      function Contains_Key(Key : Integer) return Boolean is
      begin
         for E of Result loop
            if getKey(E) = Key then
               return True;
            end if;
         end loop;
         return False;
      end Contains_Key;

   begin
      New_Line;
      Put_Line("---- Tests GetDualParentIndividuals... ----");
      New_Line;
      -- ##########################################################
      Put_Line("Test 1: Arbre vide");
      initFamilyTree(TestTree);
      Result := getDualParentIndividuals(TestTree, 1);
      pragma Assert(Length(Result) = 0, "Test 1 échoué");
      Put_Line("✓ Test 1 réussi");
      clean(TestTree);

      -- ##########################################################
      Put_Line("Test 2: Nœud unique sans parents");
      initFamilyTree(TestTree);
      initChild(TestTree, 1, initPersonObj);
      Result := getDualParentIndividuals(TestTree, 1);
      pragma Assert(Length(Result) = 0, "Test 2 échoué");
      Put_Line("✓ Test 2 réussi");
      clean(TestTree);

      -- ##########################################################
      Put_Line("Test 3: Racine avec un seul parent");
      initFamilyTree(TestTree);
      initChild(TestTree, 1, initPersonObj);
      addAncestor(TestTree, 1, LEFT, 2, initPersonObj);
      Result := getDualParentIndividuals(TestTree, 1);
      pragma Assert(Length(Result) = 0, "Test 3 échoué");
      Put_Line("✓ Test 3 réussi");
      clean(TestTree);

      -- ##########################################################
      Put_Line("Test 4: Racine avec deux parents");
      initFamilyTree(TestTree);
      initChild(TestTree, 1, initPersonObj);
      addAncestor(TestTree, 1, LEFT, 2, initPersonObj);
      addAncestor(TestTree, 1, RIGHT, 3, initPersonObj);
      Result := getDualParentIndividuals(TestTree, 1);
      pragma Assert(Length(Result) = 1 and Contains_Key(1), "Test 4 échoué");
      Put_Line("✓ Test 4 réussi");
      clean(TestTree);

      -- ##########################################################
      Put_Line("Test 5: Arbre complexe multi-niveaux");
      Build_Complex_Tree;
      Result := getDualParentIndividuals(TestTree, 1);
      
      pragma Assert(
         Length(Result) = 3 and
         Contains_Key(1) and  -- Racine avec 2 parents
         Contains_Key(3) and  -- Nœud 3 a 2 parents
         Contains_Key(6),  -- Nœud 6 a 2 parents
         "Test 5 échoué - Résultat : " & Integer'Image(Length(Result))
      );
      Put_Line("✓ Test 5 réussi");
      clean(TestTree);

      -- ##########################################################
      Put_Line("Test 6: Clé inexistante");
      Build_Complex_Tree;
      Result := getDualParentIndividuals(TestTree, 999);
      pragma Assert(Length(Result) = 0, "Test 6 échoué");
      Put_Line("✓ Test 6 réussi");
      clean(TestTree);

      -- ##########################################################
      Put_Line("Test 7: Hiérarchie profonde");
      initFamilyTree(TestTree);
      initChild(TestTree, 1, initPersonObj);
      addAncestor(TestTree, 1, LEFT, 2, initPersonObj);
      addAncestor(TestTree, 1, RIGHT, 3, initPersonObj);
      addAncestor(TestTree, 2, LEFT, 4, initPersonObj);
      addAncestor(TestTree, 2, RIGHT, 5, initPersonObj);
      addAncestor(TestTree, 5, LEFT, 6, initPersonObj);
      addAncestor(TestTree, 5, RIGHT, 7, initPersonObj); -- 5 a 2 parents
      
      Result := getDualParentIndividuals(TestTree, 1);
      pragma Assert(
         Length(Result) = 3 and
         Contains_Key(1) and
         Contains_Key(2) and
         Contains_Key(5),
         "Test 7 échoué"
      );
      Put_Line("✓ Test 7 réussi");
      clean(TestTree);
   end TestGetDualParentIndividuals;

begin
   TestDeleteAncestor;
   TestGetGenerationsCount;
   TestGetAncestorsCount;
   TestGetAncestorsByGeneration;
   TestGetOrphanIndividuals;
   TestGetSingleParentIndividuals;
   TestGetDualParentIndividuals;

end TestFamilyTree;

