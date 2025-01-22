with Utils; use Utils;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Person is
   type T_Person is private;

   -- showPerson
   -- Verbosity 1 : shows 'parentX : key' pair
   -- Verbosity 2 : shows 'parentX : full name' pair
   -- Verbosity 3 : shows all known information
   -- Verbosity 4 : shows all known & unknown information
   procedure showPerson(Person : in T_Person; Key : in Integer; Depth : in Integer := 0; Position : in T_Position := ROOT; Verbosity : in Integer := 1) with
      Pre => 1 <= Verbosity and Verbosity <= 4;

   function initPersonObj(
                          FirstName : in Unbounded_String := To_Unbounded_String(""); 
                          LastName : in Unbounded_String := To_Unbounded_String("");
                          Gender : in Unbounded_String := To_Unbounded_String("");
                          BirthDate : in Unbounded_String := To_Unbounded_String("")) return T_Person;

private 
   type T_Person is
		record
			FirstName : Unbounded_String;
			LastName : Unbounded_String;
			Gender : Unbounded_String;
         Birthdate : Unbounded_String;
		end record;
end;
