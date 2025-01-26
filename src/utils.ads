with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package utils is
   type T_Position is (ROOT, LEFT, RIGHT);
   type T_PropToShow is (Keys, Elements);
   type T_ColorType is (SUCCESS, WARNING, ERROR);
   type T_InputType is (STR, INT, KEY);
   type T_OperationType is (CHOOSE, DELETE);

   Present_Key_Exception : exception;      -- une clé est déjà présente dans un ABR
   Absent_Key_Exception     : exception;      -- une clé est absente d'un ABR
   Wrong_Position_Exception : exception;   -- une position mauvaise a été saisie

   -- Get indentation string based on the given depth
   function GetIndent (Depth : in Integer) return String;

   -- Get correct prefix character depending on the position in the tree
   function GetBinaryTreePrefix (Position : in T_Position) return String;

   function GetTrimmedInt (Int : in Integer) return String;

   function GetColoredString
     (Text : in String; Color : in T_ColorType) return String;

   function GetMenuRangeString (Length : in Integer) return String;

   function UnboundedToInteger (Str : in Unbounded_String) return Integer;

end utils;
