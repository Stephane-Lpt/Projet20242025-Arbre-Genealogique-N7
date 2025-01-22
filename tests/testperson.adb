with Ada.Text_IO; use Ada.Text_IO;
with Utils; use Utils;
with FamilyTree; use FamilyTree;
with Person; use Person;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

procedure TestPerson is
   Person1 : T_Person;
begin
   Person1 := Person.initPersonObj(
                                   To_Unbounded_String("Stephane"),
                                   To_Unbounded_String("Loppinet"),
                                   To_Unbounded_String("LeBron James"),
                                   To_Unbounded_String("01/12/2007")
                                  );

   showPerson (Person1, 10, Verbosity => 4);
end TestPerson;

