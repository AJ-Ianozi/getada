--    Copyright (C) 2022-2023 A.J. Ianozi <aj@ianozi.com>
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
with Ada.Directories;
package Files is
   --  Returns if a line exists in a given file.  If the file doesn't exist,
   --  it will return False.
   function Line_Exists (Full_Path : String; Line : String) return Boolean;
   --  Creates a random directory, returning that directory as a string.
   --  Set "Ask" to true if you want to return a directory.
   function Unique_Dir
      (Parent : String; No_Prompt : Boolean := True)
   return String;
   --  Returns true if "Dir_Path" contains "File"
   function Directory_Contains (Dir_Path : String;
       File : String;
       Which_Kind : Ada.Directories.File_Kind)
      return Boolean;
private
   function Random_String (Str_Len : Natural) return String;
end Files;
