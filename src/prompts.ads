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

package Prompts is
   type Answer is
     (No, --  Answer is "No"
      Yes, --  Answer is "Yes"
      Other,  --  "other" - useful for providing 3rd option
      DisableDefault);

   --  Prompts a yes or no question, with optional default.
   function Get_Answer
     (Prompt        : String; Default_Answer : Answer := DisableDefault;
      Provided_Text : String := "") return Answer;

   --  Prompts a question and accepts a string, with optional default answer.
   function Get_Answer
     (Prompt        : String; Default_Answer : String := "";
      Provided_Text : String := "") return String;
end Prompts;
