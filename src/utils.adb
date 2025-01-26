with Ada.Characters.Latin_1;
with Ada.Strings.Fixed;     use Ada.Strings.Fixed;
with Ada.Characters;        use Ada.Characters;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package body utils is

   function GetMenuRangeString (Length : in Integer) return String is
      RangeString : Unbounded_String;
   begin
      return "(1-" & GetTrimmedInt (Length) & " ou 'q')";
   end GetMenuRangeString;

   function GetIndent (Depth : in Integer) return String is
      IndentLength : Integer := Depth * 4;
      Indent       : String (1 .. IndentLength);
   begin
      for i in 1 .. IndentLength loop
         Indent (i) := ' ';
      end loop;

      return Indent;
   end GetIndent;

   function GetBinaryTreePrefix (Position : in T_Position) return String is
   begin
      case Position is
         when ROOT =>
            return "/";
         when RIGHT =>
            return ">";
         when LEFT =>
            return "<";
      end case;
   end GetBinaryTreePrefix;

   function GetTrimmedInt (Int : in Integer) return String is
   begin
      return Trim (Int'Image, Ada.Strings.Left);
   end GetTrimmedInt;

   function GetColoredString
     (Text : in String; Color : in T_ColorType) return String
   is
      ColorCharacter : Unbounded_String;
   begin
      case Color is
         when SUCCESS =>
            ColorCharacter := To_Unbounded_String ("[92m");
         when WARNING =>
            ColorCharacter := To_Unbounded_String ("[93m");
         when ERROR =>
            ColorCharacter := To_Unbounded_String ("[91m");
      end case;

      return
        (Latin_1.ESC & To_String (ColorCharacter) & Text & Latin_1.ESC &
         "[0m");
   end GetColoredString;

   function UnboundedToInteger (Str : in Unbounded_String) return Integer is
   begin
      return Integer'Value (To_String (Str));
   end UnboundedToInteger;

end utils;
