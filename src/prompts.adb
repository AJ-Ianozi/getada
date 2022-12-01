with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Strings.Fixed;       use Ada.Strings.Fixed;
with Ada.Text_IO;             use Ada.Text_IO;

package body Prompts is

   function Get_Answer
     (Prompt : String; Default_Answer : Yes_or_No := NA) return Yes_or_No
   is
      Question : constant String :=
        Prompt & " [" &
        (case Default_Answer is when Yes => "Y/n", when No => "y/N",
           when others                   => "y/n") &
        "]  >";
   begin
      loop
         Put (Question);
         declare
            Response : constant String :=
              Trim (To_Lower (Get_Line), Ada.Strings.Both);
         begin
            if Response'Length = 0 and then Default_Answer /= NA then
               return Default_Answer;
            elsif Response = "y" or else Response = "yes" then
               return Yes;
            elsif Response = "n" or else Response = "no" then
               return No;
            else
               Put_Line ("Invalid response.");
            end if;
         end;
      end loop;
   end Get_Answer;

   function Get_Answer
     (Prompt : String; Default_Answer : String := "") return String
   is
      Question : constant String :=
        Prompt &
        (if Default_Answer'Length > 0 then " [" & Default_Answer & "]"
         else "") &
        " >";
   begin
      Put (Question);
      declare
         Response : constant String :=
           Trim (To_Lower (Get_Line), Ada.Strings.Both);
      begin
         if Response'Length = 0 and then Default_Answer'Length > 0 then
            return Default_Answer;
         else
            return Response;
         end if;
      end;
   end Get_Answer;

end Prompts;
