with Utils; use Utils;
with FamilyTree; use FamilyTree;
with Person; use Person;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with FamilyTree; use FamilyTree;

procedure TestPerson is
   FamilyTree : T_FamilyTree;
begin
   initChild (FamilyTree, 1, initPersonObj(
                                   FirstName => To_Unbounded_String("Stephane"),
                                   LastName => To_Unbounded_String("Loppinet"),
                                   Gender => To_Unbounded_String("Homme")
                                  ));

   addAncestor(FamilyTree, 1, LEFT, 2, initPersonObj(
                                   FirstName => To_Unbounded_String("Simon"),
                                   LastName => To_Unbounded_String("Cathala"),
                                   Gender => To_Unbounded_String("Indefini")
                                  ));

   addAncestor(FamilyTree, 1, RIGHT, 3, initPersonObj(
                                   FirstName => To_Unbounded_String("Nils"),
                                   Gender => To_Unbounded_String("LeBron James"),
                                   Birthdate => To_Unbounded_String("01/12/1945")
                                  ));

   addAncestor(FamilyTree, 2, LEFT, 4, initPersonObj(
                                   FirstName => To_Unbounded_String("Pierre"),
                                   Gender => To_Unbounded_String("Homme"),
                                   Birthdate => To_Unbounded_String("01/12/1923")
                                  ));

   showFamilyTree(ABR        => FamilyTree,
                  Verbosity  => 4);
end TestPerson;

