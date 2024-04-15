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

with Ada.Directories;   use Ada.Directories;
with Ada.Text_IO;       use Ada.Text_IO;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Numerics.Discrete_Random;
with Prompts; use Prompts;
with Defaults;
package body Files is

   --  Returns true if "Dir_Path" contains "File"
   function Directory_Contains
      (Dir_Path : String;
       File : String;
       Which_Kind : Ada.Directories.File_Kind)
      return Boolean
   is
      Next_Item     : Directory_Entry_Type;
      Directory_Search : Search_Type;
   begin
      Start_Search (Directory_Search, Dir_Path, "");
      while More_Entries (Directory_Search) loop
         Get_Next_Entry (Directory_Search, Next_Item);
         if Kind (Next_Item) = Which_Kind and then
            Simple_Name (Next_Item) = File
         then
            return True;
         end if;
      end loop;
      return False;
   end Directory_Contains;

   --  Creates a random string.
   function Random_String (Str_Len : Natural) return String is
      Alpha_Num : constant array (1 .. 62) of Character :=
         ('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd',
          'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r',
          's', 't', 'u', 'v', 'w', 'x', 'y', 'z', 'A', 'B', 'C', 'D', 'E', 'F',
          'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T',
          'U', 'V', 'W', 'X', 'Y', 'Z');
      subtype Alpha_Range is Integer range 1 .. 62;
      package Rand_Gen is new Ada.Numerics.Discrete_Random (Alpha_Range);
      use Rand_Gen;
      Gen : Generator;
   begin
      Reset (Gen);
      return Result : String (1 .. Str_Len) do
         for I in 1 .. Str_Len loop
            Result (I) := Alpha_Num (Random (Gen));
         end loop;
      end return;
   end Random_String;

   function Line_Exists (Full_Path : String; Line : String) return Boolean is
      File_To_Check : File_Type;
      Result        : Boolean := False;
   begin
      if Exists (Full_Path) then
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

   --  Creates a temporary directory in Tmp_Dir directory
   --  It will try 1000 times.
   --  Raises an exception if unable to create directory.
   function Unique_Dir
      (Parent : String; No_Prompt : Boolean := True)
   return String is
   begin
      if not Ada.Directories.Exists (Parent) then
         raise Defaults.Invalid_File with Parent & " does not exist!";
      end if;
      loop
         for I in 1 .. 1000 loop
            declare
               New_Directory : constant String :=
                  Ada.Directories.Full_Name
                     (Parent & "/" & "tmp." & Random_String (16));
            begin
               if not Ada.Directories.Exists (New_Directory) then
                  Ada.Directories.Create_Path (New_Directory);
                  return New_Directory;
               end if;
            end;
         end loop;
         if No_Prompt or else
            Get_Answer ("Cannot create unique folder in " & Parent &
                        "... Keep searching?", Default_Answer => Yes) =
            No
         then
            raise Defaults.Invalid_File with
               "Unable to find unique dir in " & Parent;
         end if;
      end loop;
   end Unique_Dir;

end Files;
