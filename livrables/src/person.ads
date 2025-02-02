with Utils; use Utils;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Person is
   EmptyString      : constant Unbounded_String := To_Unbounded_String ("");
   type T_Person is private;

   function GetPersonName(Person : in T_Person; Key : in Integer; OnlyReturnFullNames : Boolean := False) return String;

   -- showPerson
   -- Verbosity 1 : shows 'parentX : key' pair
   -- Verbosity 2 : shows 'parentX : full name' pair
   -- Verbosity 3 : shows all known information
   -- Verbosity 4 : shows all known & unknown information
   procedure showPerson(Person : in T_Person; Key : in Integer; Depth : in Integer := 0; Position : in T_Position := ROOT; Verbosity : in Integer := 1) with
      Pre => 1 <= Verbosity and Verbosity <= 4;

   function initPersonObj(
                          FirstName : in Unbounded_String := EmptyString; 
                          LastName : in Unbounded_String := EmptyString;
                          Gender : in Unbounded_String := EmptyString;
                          BirthDate : in Unbounded_String := EmptyString) return T_Person;

private 
   type T_Person is
		record
			FirstName : Unbounded_String;
			LastName : Unbounded_String;
			Gender : Unbounded_String;
         Birthdate : Unbounded_String;
		end record;
end;
