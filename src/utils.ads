package utils is
   type T_Position is (ROOT, LEFT, RIGHT);
   
   -- Get indentation string based on the given depth
   function getIndent(Depth : in Integer) return String;
   
   -- Get correct prefix character depending on the position in the tree
   function getBinaryTreePrefix(Position : in T_Position) return String;

end utils;
