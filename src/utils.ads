package utils is
   type T_Position is (ROOT, LEFT, RIGHT);
   type T_PropToShow is (Keys, Elements);
   type T_ColorType is (SUCCESS, WARNING, ERROR);
   type T_InputType is (STR, INT);
   type T_OperationType is (CHOOSE, DELETE);

   Present_Key_Exception : exception;      -- une clé est déjà présente dans un ABR
   Absent_Key_Exception  : exception;      -- une clé est absente d'un ABR
   Wrong_Position_Exception : exception;   -- une position mauvaise a été saisie
   
   -- Get indentation string based on the given depth
   function getIndent(Depth : in Integer) return String;
   
   -- Get correct prefix character depending on the position in the tree
   function getBinaryTreePrefix(Position : in T_Position) return String;

   function getTrimmedInt(Int : in Integer) return String;

   function getColoredString(Text : in String; Color : in T_ColorType) return String;

   function getMenuRangeString(Length : in Integer) return String;

end utils;
