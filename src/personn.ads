with Dates; use Dates;

package Person is
   type T_Person is private;

private 
   type T_Person is
		record
			FirstNamme : String;
			LastName : String;
			Gender : String;
         Birthdate : T_Date;
		end record;
end;