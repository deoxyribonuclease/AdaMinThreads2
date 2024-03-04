with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Numerics.Discrete_Random;

procedure Adaminthreads is

   arrLength : constant Integer := 100000;
   thread_num : constant Integer := 10;

   type My_Array is array (1..arrLength) of Integer;

   subtype Random_Range is Integer range 1 .. 1000000;
   package Random_Int is new Ada.Numerics.Discrete_Random(Random_Range);
   use Random_Int;
   G : Generator;

   protected type Shared_Array is
      procedure Initialize;
      function Get_Min return Integer;
      function Get_Min_Index return Integer;
      function Get_Element(Index : Integer) return Integer;
      procedure Set_Element(Index : Integer; Value : Integer);
   private
      arr : My_Array;
      Min_Global : Integer;
      Indx_Global : Integer;
   end Shared_Array;

   protected body Shared_Array is
      procedure Initialize is
      begin
         Reset(G);
         for I in arr'Range loop
            arr(I) := abs Random(G);
         end loop;
         Min_Global := arr(1);
         Indx_Global := 1;
         for I in arr'Range loop
            if arr(I) < Min_Global then
               Min_Global := arr(I);
               Indx_Global := I;
            end if;
         end loop;
      end Initialize;

      function Get_Min return Integer is
      begin
         return Min_Global;
      end Get_Min;

      function Get_Min_Index return Integer is
      begin
         return Indx_Global;
      end Get_Min_Index;

      function Get_Element(Index : Integer) return Integer is
      begin
         return arr(Index);
      end Get_Element;

      procedure Set_Element(Index : Integer; Value : Integer) is
      begin
         arr(Index) := Value;
         if Value < Min_Global then
            Min_Global := Value;
            Indx_Global := Index;
         end if;
      end Set_Element;
   end Shared_Array;

   SA : Shared_Array;

   task type Min_Finder is
      entry Starts(Start_Index, End_Index, Thread_Number : in Integer);
   end Min_Finder;

   task body Min_Finder is
      Start, Finish, Thread : Integer;
   begin
      accept Starts(Start_Index, End_Index, Thread_Number: in Integer) do
         Start := Start_Index;
         Finish := End_Index;
         Thread := Thread_Number;
      end Starts;

      declare
         Min_Local : Integer := SA.Get_Element(Start);
         Indx_Local : Integer := 0;
      begin
         for I in Start..Finish loop
            if SA.Get_Element(I) < Min_Local then
               Min_Local := SA.Get_Element(I);
               Indx_Local := I;
            end if;
         end loop;
         SA.Set_Element(Start, Min_Local);
         Put_Line("Thread"& Thread'Img & ": min element is -" & Min_Local'Img & " at index" & Indx_Local'Img);
      end;
   end Min_Finder;
   Min_Finders : array(1..thread_num) of Min_Finder;

begin
   SA.Initialize;

   -- replace random element
   declare
      Random_Index : Integer := Random(G) mod arrLength + 1;
   begin
      SA.Set_Element(Random_Index, -8);
      Put_Line("Element at index" & Random_Index'Img & " is now negative: -8");
   end;

   -- cut and start
   for I in 1..thread_num loop
      Min_Finders(I).Starts((I-1) * arrLength / thread_num + 1, I * arrLength / thread_num, I);
   end loop;

   -- print final min value
   for I in 1..arrLength loop
      if SA.Get_Element(I) = SA.Get_Min then
         Put_Line("Minimum element is " & SA.Get_Min'Img & " at index " & SA.Get_Min_Index'Img);
         exit;
      end if;
   end loop;
end Adaminthreads;
