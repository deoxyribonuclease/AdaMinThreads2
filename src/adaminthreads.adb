with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Numerics.Discrete_Random;

procedure Adaminthreads is

   arrLength : constant Integer := 100000;
   thread_num : constant Integer := 10;

   type My_Array is array (1..arrLength) of Integer;
   arr : My_Array;

   subtype Random_Range is Integer range 1 .. 1000000;
   package Random_Int is new Ada.Numerics.Discrete_Random(Random_Range);
   use Random_Int;
   G : Generator;

   task type Min_Finder is
      entry Starts(Start_Index, End_Index : in Integer);
      entry Get_Min(Min : out Integer);
   end Min_Finder;

   task body Min_Finder is
      Min_Local : Integer;
      Start, Finish : Integer;
   begin
      accept Starts(Start_Index, End_Index : in Integer) do
         Start := Start_Index;
         Finish := End_Index;
      end Starts;

      Min_Local := arr(Start);
      for I in Start..Finish loop
         if arr(I) < Min_Local then
            Min_Local := arr(I);
         end if;
      end loop;

      accept Get_Min(Min : out Integer) do
         Min := Min_Local;
      end Get_Min;
   end Min_Finder;

   Min_Finders : array(1..thread_num) of Min_Finder;
   Min_Global : Integer;

begin
   -- generate array
   Reset(G);
   for I in arr'Range loop
      arr(I) := abs Random(G);
   end loop;

   -- replace random element
   declare
      Random_Index : Integer := Random(G) mod arrLength + 1;
   begin
   arr(Random_Index) := -8;
      Put_Line("Element at index" & Random_Index'Img & " is now negative: -8");
   end;

   -- cut and start
   for I in 1..thread_num loop
      Min_Finders(I).Starts((I-1) * arrLength / thread_num + 1, I * arrLength / thread_num);
   end loop;

   -- get min from each thread, also print local min
   Min_Global := arr(1);
   for I in 1..thread_num loop
      declare
         Min_Local : Integer;
      begin
         Min_Finders(I).Get_Min(Min_Local);
          for J in ((I-1) * arrLength / thread_num + 1).. (I * arrLength / thread_num) loop
            if arr(J) = Min_Local then
            Put_Line("Thread"& I'Img & ": min element is - " & Min_Local'Img & " at index " & J'Img);
            exit;
           end if;
          end loop;
         if Min_Local < Min_Global then
            Min_Global := Min_Local;
         end if;
      end;
   end loop;

   -- print final min value
   for I in arr'Range loop
      if arr(I) = Min_Global then
         Put_Line("Minimum element is " & Min_Global'Img & " at index " & I'Img);
         exit;
      end if;
   end loop;
end Adaminthreads;
