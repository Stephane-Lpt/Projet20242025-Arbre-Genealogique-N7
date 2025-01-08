with Ada.Text_IO;         use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

package body Dates is

   procedure Initialize
     (Date :    out T_Date; Day : in Integer; Month : in T_Month;
      Year : in     Integer)
   is
   begin
      Date.Day   := Day;
      Date.Month := Month;
      Date.Year  := Year;
   end Initialize;

   procedure show (Date : in T_Date) is
   begin
      if Date.Day < 10 then
         Put ('0');
      end if;
      Put (Date.Day, Width => 0);
      Put ('/');
      if Integer'Pos (Date.Month) + 1 < 10 then
         Put ('0');
      end if;
      Put (Integer'Pos (Date.Month) + 1);
      Put (Date.Year, Width => 0);
      New_Line;
   end show;

   function getDay (Date : in T_Date) return Integer is
   begin
      return Date.Day;
   end getDay;

   function getMonth (Date : in T_Date) return T_Month is
   begin
      return Date.Month;
   end getMonth;

   function getYear (Date : in T_Date) return Integer is
   begin
      return Date.Year;
   end getYear;

end Dates;
