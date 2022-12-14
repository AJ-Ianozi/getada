--    Copyright (C) 2022 A.J. Ianozi <aj@ianozi.com>
--
--    This file is part of GetAda: the Unofficial Alire Installer
--
--    This program is free software: you can redistribute it and/or modify
--    it under the terms of the GNU General Public License as published by
--    the Free Software Foundation, either version 3 of the License, or
--    (at your option) any later version.
--
--    This program is distributed in the hope that it will be useful,
--    but WITHOUT ANY WARRANTY; without even the implied warranty of
--    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--    GNU General Public License for more details.
--
--    You should have received a copy of the GNU General Public License
--    along with this program.  If not, see <https://www.gnu.org/licenses/>.

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Strings.Fixed;       use Ada.Strings.Fixed;
with Ada.Text_IO;             use Ada.Text_IO;

package body Prompts is

   function Get_Answer
     (Prompt        : String; Default_Answer : Answer := DisableDefault;
      Provided_Text : String := "") return Answer
   is
      Question : constant String :=
        Prompt & " [" &
        (case Default_Answer is when Yes => "Y/n", when No => "y/N",
           when others                   => "y/n") &
        "] " &
        (if Provided_Text'Length > 0 then " (" & Provided_Text & ")" else "") &
        " >";
   begin
      loop
         Put (Question);
         declare
            Response : constant String :=
              Trim (To_Lower (Get_Line), Ada.Strings.Both);
         begin
            if Response'Length = 0 and then Default_Answer /= DisableDefault
            then
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
     (Prompt        : String; Default_Answer : String := "";
      Provided_Text : String := "") return String
   is
      Question : constant String :=
        Prompt &
        (if Default_Answer'Length > 0 then " [" & Default_Answer & "]"
         else "") &
        (if Provided_Text'Length > 0 then " (" & Provided_Text & ")" else "") &
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
