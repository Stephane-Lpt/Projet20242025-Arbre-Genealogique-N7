package body utils is
   
   function getIndent(Depth : in Integer) return String is
      IndentLength : Integer := Depth * 4;
      Indent : String(1..IndentLength);
   begin
      for i in 1..IndentLength loop
         Indent(i) := ' ';
      end loop;
      
      return Indent;
   end getIndent;
   
   -- TODO : refactor bcs there's a way to do it differently & easier?
   function getEmptyString(Length : in Integer) return String is
      Indent : String(1..Length);
   begin
      for i in 1..Length loop
         Indent(i) := ' ';
      end loop;
      
      return Indent;
   end getEmptyString;

   
   function getBinaryTreePrefix(Position : in T_Position) return String is
   begin
      case Position is
            when ROOT =>
               return "/";
            when RIGHT =>
               return ">";
            when LEFT =>
               return "<";
      end case;
   end getBinaryTreePrefix;
end utils;
