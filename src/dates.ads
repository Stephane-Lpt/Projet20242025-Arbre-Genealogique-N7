package Dates is

   type T_Month is
     (JANUARY, FEBRUARY, MARCH, APRIL, MAY, JUNE, JULY, AUGUST, SEPTEMBER,
      OCTOBER, NOVEMBER, DECEMBER);

   type T_Date is private;
   
   procedure initialize(Date : out T_Date; Day : in Integer; Month : in T_Month; Year : in Integer) with
     Pre  => Year >= 0 and Day >= 1 and Day <= 31,
     Post => Get_Day (Date) = Day and Get_Month (Date) = Month and Get_Year (Date) = Year;

   procedure show(Date : in T_Date);

   function getMonth(Date : in T_Date) return T_Month;

   function getDay(Date : in T_Date) return Integer;

   function getYear(Date : in T_Date) return Integer;

private
   type T_Date is record
      Day   : Integer;
      Month : T_Month;
      Year  : Integer;
   end record;
end Dates;
