with Ada.Text_IO;         use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

package body Person is

   function GetPersonName(Person : in T_Person; Key : in Integer; OnlyReturnFullNames : Boolean := False) return String is
      NameString  : Unbounded_String;
      KeyString : Unbounded_String := To_Unbounded_String(GetTrimmedInt(Key));
   begin
      if Person.FirstName /= EmptyString and Person.LastName /= EmptyString then
         NameString := Person.FirstName & " " & Person.LastName & " (" & KeyString & ")";
      else
         if OnlyReturnFullNames then
            NameString := KeyString;
         else
            if Person.FirstName /= EmptyString and Person.LastName = EmptyString then
               NameString := Person.FirstName & "(" & KeyString & ")";
            else
               NameString := Person.LastName & "(" & KeyString & ")";
            end if;
         end if;
      end if;
      
      return To_String(NameString);
   end GetPersonName;

   function initPersonObj
     (FirstName : in Unbounded_String := EmptyString;
      LastName  : in Unbounded_String := EmptyString;
      Gender    : in Unbounded_String := EmptyString;
      BirthDate : in Unbounded_String := EmptyString)
      return T_Person
   is
      Person : T_Person;
   begin
      Person.FirstName := FirstName;
      Person.LastName  := LastName;
      Person.Gender    := Gender;
      Person.Birthdate := BirthDate;

      return Person;
   end initPersonObj;

   -- Verbosity 1 : shows 'parentX : key' pair
   -- Verbosity 2 : shows 'parentX : full name' pair
   -- Verbosity 3 : shows all known information
   -- Verbosity 4 : shows all known & unknown information
   procedure showPerson
     (Person   : in T_Person; Key : in Integer; Depth : in Integer := 0;
      Position : in T_Position := ROOT; Verbosity : in Integer := 1)
   is
      BinaryTreePrefix : String := GetBinaryTreePrefix (Position);
      Indent           : String                    := GetIndent (Depth);
      ParentString     : Unbounded_String          :=
        To_Unbounded_String
          (GetBinaryTreePrefix (Position) & " " &
           (if Position = ROOT then "Enfant"
            else (if Position = LEFT then "Parent 1" else "Parent 2")));
      UnknownValue     : constant Unbounded_String :=
        To_Unbounded_String ("Inconnu");

      PropFirstName : constant Unbounded_String :=
        To_Unbounded_String ("Pr�nom");
      PropLastName  : constant Unbounded_String :=
        To_Unbounded_String ("Nom de famille");
      PropGender : constant Unbounded_String := To_Unbounded_String ("Sexe");
      PropBirthdate : constant Unbounded_String :=
        To_Unbounded_String ("Date d'anniversaire");

      function getFormattedString
        (PropName : in Unbounded_String; PropValue : in Unbounded_String;
         PutUnknownValues : in Boolean := False) return String
      is
         FormattedString : Unbounded_String := Indent & PropName & ": ";
      begin
         if PropValue = EmptyString and PutUnknownValues then
            FormattedString :=
              FormattedString & UnknownValue & Character'Val (10);
         elsif PropValue = EmptyString and not PutUnknownValues then
            FormattedString := EmptyString;
         else
            FormattedString :=
              FormattedString & PropValue & Character'Val (10);
         end if;

         return To_String (FormattedString);
      end getFormattedString;
   begin
      case Verbosity is
         when 1 =>
            Put
              (getFormattedString
                 (ParentString, To_Unbounded_String (Integer'Image (Key))));
         when 2 =>
            Put (getFormattedString (ParentString, To_Unbounded_String(GetPersonName(Person, Key))));
         when 3 =>
            Put
              (getFormattedString
                 (ParentString, To_Unbounded_String (Integer'Image (Key))));
            Put (getFormattedString (PropFirstName, Person.FirstName));
            Put (getFormattedString (PropLastName, Person.LastName));
            Put (getFormattedString (PropGender, Person.Gender));
            Put (getFormattedString (PropBirthdate, Person.Birthdate));
            New_Line;
         when 4 =>
            Put
              (getFormattedString
                 (ParentString, To_Unbounded_String (Integer'Image (Key)),
                  True));
            Put (getFormattedString (PropFirstName, Person.FirstName, True));
            Put (getFormattedString (PropLastName, Person.LastName, True));
            Put (getFormattedString (PropGender, Person.Gender, True));
            Put (getFormattedString (PropBirthdate, Person.Birthdate, True));
            New_Line;
         when others =>
            null;
      end case;
   end showPerson;

end Person;
