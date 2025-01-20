with Dates; use Dates;
with Utils; use Utils;

package Person is
   type T_Person is private;

   -- showPerson
   -- Verbosity 1 : shows 'parentX : key' pair
   -- Verbosity 2 : shows 'parentX : full name' pair
   -- Verbosity 3 : shows all known information
   -- Verbosity 4 : shows all known information
   procedure showPerson(Person : in T_Person; Key : in Integer; Depth : in Integer := 0; Position : in T_Position := ROOT; Verbosity : in Integer) with
      Pre => 1 <= Verbosity <= 3;

private 
   type T_Person is
		record
			FirstNamme : String;
			LastName : String;
			Gender : String;
         Birthdate : T_Date;
		end record;
end;