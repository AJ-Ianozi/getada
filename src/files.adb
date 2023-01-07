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

with Ada.Directories;   use Ada.Directories;
with Ada.Text_IO;       use Ada.Text_IO;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
package body Files is

   function Line_Exists (Full_Path : String; Line : String) return Boolean is
      File_To_Check : File_Type;
      Result        : Boolean := False;
   begin
      if Ada.Directories.Exists (Full_Path) then
         Open (File_To_Check, In_File, Full_Path);
         --  Iterate through the file, looking for a PATH export.
         Check_File :
         while not End_Of_File (File_To_Check) loop
            declare
               Next_Line : constant String := Get_Line (File_To_Check);
            begin
               if Index (Next_Line, Line) > 0 then
                  --  Contains our env file
                  Result := True;
                  exit Check_File;
               end if;
            end;
         end loop Check_File;
         Close (File_To_Check);
      end if;
      return Result;
   end Line_Exists;
end Files;
