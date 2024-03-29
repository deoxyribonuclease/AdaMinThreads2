with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Numerics.Discrete_Random;

procedure Adaminthreads is

   arrLength : constant Integer := 100000;
   thread_num : constant Integer := 100;
   Result_Index : Integer := 0;
   Result_Value : Integer := 0;

   type My_Array is array (1..arrLength) of Integer;

   subtype Random_Range is Integer range 1 .. 1000000;
   package Random_Int is new Ada.Numerics.Discrete_Random(Random_Range);
   use Random_Int;
   G : Generator;

   protected type Shared_Array is
      procedure Initialize;
      entry Get_Final_Min (Value : out Integer;Index : out Integer );
      function Get_Min return Integer;
      function Get_Min_Index return Integer;
      function Get_Element(Index : Integer) return Integer;
      procedure Set_Element(Index : Integer; Value : Integer);
   private
      Thread_Count : Integer := 0;
      arr : My_Array;
      Min_Global : Integer;
      Indx_Global : Integer;
   end Shared_Array;

   protected body Shared_Array is
      procedure Initialize is
      Random_Index : Integer := Random(G) mod arrLength;
      begin
         for I in arr'Range loop
            arr(I) := abs Random(G);
         end loop;

         arr(Random_Index) := -8;
         Put_Line("Element at index" & Random_Index'Img & " is now negative: -8");

         Min_Global := Integer'Last;
         Indx_Global := -1;
      end Initialize;

      entry Get_Final_Min (Value : out Integer;Index : out Integer ) when Thread_Count = thread_num is
      begin
         Value := Min_Global;
         Index := Indx_Global;
         --Put_Line("Final Minimum element is " & Min_Global'Img & " at index " & Indx_Global'Img);
      end Get_Final_Min;

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
          if arr(Index) < Min_Global then
               Min_Global := arr(Index);
               Indx_Global := Index;
            end if;
         Thread_Count := Thread_Count + 1;

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
         Put_Line("Thread"& Thread'Img & ": min element is -" & Min_Local'Img & " at index" & Indx_Local'Img);
         SA.Set_Element(Indx_Local, Min_Local);
      end;
   end Min_Finder;
   Min_Finders : array(1..thread_num) of Min_Finder;

begin
   Reset(G);
   SA.Initialize;

   -- cut and start
   for I in 1..thread_num loop
      Min_Finders(I).Starts((I-1) * arrLength / thread_num + 1, I * arrLength / thread_num, I);
   end loop;

  SA.Get_Final_Min(Result_Value, Result_Index);
   Put_Line("Final Minimum element is " & Result_Value'Img & " at index " & Result_Index'Img);

end Adaminthreads;
