with Ada.Characters.Latin_1;
with Ada.Strings.Fixed;     use Ada.Strings.Fixed;
with Ada.Characters;        use Ada.Characters;

package body utils is

   function GetMenuRangeString (Length : in Integer) return String is
   begin
      if Length = 1 then
        return "(1 ou 'q')";
      else
        return "(1-" & GetTrimmedInt (Length) & " ou 'q')";
      end if;
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
   
    function IsValidDateString(DateUnboundedString : in Unbounded_String) return Boolean is
        DateString : String (1 .. 10);
        DAY : Integer;
        MONTH : Integer;
        YEAR : Integer;

        function RemoveLeadingZeros (Str : in String) return Integer is
        begin
            if Str = "" then
                return -1;
            end if;
            for chr of Str loop
                if not (chr in '0' .. '9') then
                    return -1;
                end if;
            end loop;

            for I in Str'Range loop
                if Str(I) /= '0' then
                    return Integer'Value(Str(I .. Str'Last));
                end if;
            end loop;

            return 0;
        end RemoveLeadingZeros;
	begin
        if Length(DateUnboundedString) /= 10 then
            return false;
        end if;

        DateString := To_String(DateUnboundedString);

        if DateString(3) /= '-' or DateString(6) /= '-' then
            return false;
        end if;

        DAY := RemoveLeadingZeros(DateString(1 .. 2));
        MONTH := RemoveLeadingZeros(DateString(4 .. 5));
        YEAR := RemoveLeadingZeros(DateString(7 .. 10));

            -- Validate the extracted values
        if (DAY < 1 or DAY > 31) or (MONTH < 1 or MONTH > 12) or (YEAR < 0 or YEAR > 2025) then
            return False;
        else
            return True;
        end if;
    exception
        when others => 
            return False;
    end IsValidDateString;

end utils;
