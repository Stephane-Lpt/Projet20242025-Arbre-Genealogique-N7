with Ada.Text_IO;         use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;


package body Person is
   function initPersonObj(
                          FirstName : in Unbounded_String := To_Unbounded_String(""); 
                          LastName : in Unbounded_String := To_Unbounded_String("");
                          Gender : in Unbounded_String := To_Unbounded_String("");
                          BirthDate : in Unbounded_String := To_Unbounded_String("")
   ) return T_Person is
      Person : T_Person;
   begin
      Person.FirstName := FirstName;
      Person.LastName := LastName;
      Person.Gender := Gender;
      Person.Birthdate := Birthdate;
   
      return Person;
   end initPersonObj;

   -- Verbosity 1 : shows 'parentX : key' pair
   -- Verbosity 2 : shows 'parentX : full name' pair
   -- Verbosity 3 : shows all known information
   -- Verbosity 4 : shows all known & unknown information
   procedure showPerson(Person : in T_Person; Key : in Integer; Depth : in Integer := 0; Position : in T_Position := ROOT; Verbosity : in Integer := 1) is
      BinaryTreePrefix : String := getBinaryTreePrefix (Position);
      Indent : String := getIndent (Depth);
      ParentString : Unbounded_String := To_Unbounded_String(if Position = ROOT then "Enfant" else (if Position = LEFT then "Parent 1" else "Parent 2"));
      ExtraIndentation : Unbounded_String := To_Unbounded_String((Length(Indent & ParentString)));
      UnknownValue : constant String := "Inconnu";

      function getFormattedString (PropName : in Unbounded_String; PropValue : in Unbounded_String; PutUnknownValues : in Boolean := False) return String is
         FormattedString : Unbounded_String := Indent & PropName & ": ";
         EmptyString : constant Unbounded_String := To_Unbounded_String("");
      begin
         if PropValue = EmptyString and PutUnknownValues then
            FormattedString := FormattedString & UnknownValue & Character'Val(10);
         elsif PropValue = EmptyString and not PutUnknownValues then
            FormattedString := EmptyString;
         else
            FormattedString := FormattedString & PropValue & Character'Val(10);
         end if;
         
         return To_String(FormattedString);
      end getFormattedString;
   begin
      case Verbosity is
         when 1 => 
            Put (getFormattedString(ParentString, To_Unbounded_String(Integer'Image(Key))));
         when 2 =>
            declare
               NameString : Unbounded_String;
               EmptyString : Unbounded_String := To_Unbounded_String ("");
            begin
               if Person.FirstName /= EmptyString and Person.LastName = EmptyString then
                  NameString := Person.FirstName;
               elsif Person.FirstName = EmptyString and Person.LastName /= EmptyString then
                  NameString := Person.LastName;
               elsif Person.FirstName /= EmptyString and Person.LastName /= EmptyString then
                  NameString := Person.LastName & " " & Person.LastName;
               else
                  NameString := EmptyString;
               end if;
               
               Put (getFormattedString(ParentString, NameString));
            end;
         when 3 => 
            Put (getFormattedString(ParentString, To_Unbounded_String(Integer'Image(Key))));
            Put (getFormattedString(ExtraIndentation, Person.FirstName));
            Put (getFormattedString(ExtraIndentation, Person.LastName));
            Put (getFormattedString(ExtraIndentation, Person.Gender));
            Put (getFormattedString(ExtraIndentation, Person.Birthdate));
         when 4 => 
            Put (getFormattedString(ParentString, To_Unbounded_String(Integer'Image(Key)), True));
            Put (getFormattedString(ExtraIndentation, Person.FirstName, True));
            Put (getFormattedString(ExtraIndentation, Person.LastName, True));
            Put (getFormattedString(ExtraIndentation, Person.Gender, True));
            Put (getFormattedString(ExtraIndentation, Person.Birthdate, True));
         when others =>
            Null;
      end case;
   end showPerson;
 
end Person;
