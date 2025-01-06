package Person is
   type T_Person is private;

private 
   type T_Person is
		record
			FirstNamme : String;
			LastName : String;
			Gender : String;
         Birthdate : String;
		end record;
      
end;