--    Copyright (C) 2022-2024 A.J. Ianozi <aj@ianozi.com>
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

with Ada.Strings.Fixed;       use Ada.Strings.Fixed;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Platform;                use Platform;
with Ada.Environment_Variables;
with Ada.Directories;

package body Shells is
   --  Get the resulting configuration file to edit, based on the shell.
   function Get_Shell_Config (Shell_Name : Supported_Shells) return String is
   begin
      case Shell_Name is
         when sh | bash =>
            return ".profile";
         when zsh =>
            return ".zshenv";
         when others =>
            raise Unsupported_Shell with Shell_Name'Image & " not supported";
      end case;
   end Get_Shell_Config;
   --  I'm doing it this way in case I want to add support for fish, e.g.
   --  "env.fish" etc.
   function Get_Shell_Env (Shell_Name : Supported_Shells) return String is
   begin
      case Shell_Name is
         when sh | bash | zsh =>
            return "env.sh";
         when others =>
            raise Unsupported_Shell with Shell_Name'Image & " not supported";
      end case;
   end Get_Shell_Env;
   procedure Write_Env_File
     (Shell_Name : Supported_Shells; File : File_Type; Dir : String)
   is
   begin
      case Shell_Name is
         when sh | bash | zsh =>
            Put_Line (File, "#!/bin/sh");
            Put_Line (File, "# Add alire's dir to path");
            Put_Line (File, "case "":$PATH:"" in");
            Put_Line (File, "*"":" & Dir & ":""*)");
            Put_Line (File, "  : ;; # already exists, do nothing.");
            Put_Line (File, "* )");
            Put_Line (File, "  # Add to path");
            Put_Line (File, "  export PATH=""$PATH:" & Dir & """");
            Put_Line (File, "esac");
         when others =>
            raise Unsupported_Shell with Shell_Name'Image & " not supported";
      end case;
   end Write_Env_File;
   function Get_Env_Command
     (Shell_Name : Supported_Shells; Env_File : String) return String
   is
   begin
      case Shell_Name is
         when sh | bash | zsh =>
            return ". """ & Env_File & """";
         when null_shell =>
            raise Unsupported_Shell with Shell_Name'Image & " not supported";
      end case;
   end Get_Env_Command;
   --  Checks if this shell is compatible with the bourne shell
   function Sh_Compatible (Shell_Name : Supported_Shells) return Boolean is
   begin
      case Shell_Name is
         when sh | bash =>
            return True;
         when others =>
            return False;
      end case;
   end Sh_Compatible;

   function Available_Shells return Shell_Array
   is
      --  Env variable for current Shell
      Shell_Env : constant String := "SHELL";

      --  General location of available shells.
      Shell_Path : constant String := "/etc/shells";

      --  Truth table of avalaible shells
      Shell_Amount : Natural := 0; -- Amount of shells discovered
      Shell_List   : array (Supported_Shells'Range) of Boolean :=
        (others => False);

      --  This processes each shell using the established variables above
      procedure Process_Shell (Shell_Str : String) is
         Prefix : constant String := "/bin/"; -- All lines start with this?
      begin
         --  If this line is /bin/something
         if Shell_Str'Length > Prefix'Length
           and then Index (Shell_Str, Prefix) = 1
         then
            declare
               Check_Shell : constant String :=
                 To_Upper
                   (Shell_Str
                      (Shell_Str'First - 1 + Prefix'Length + 1 ..
                           Shell_Str'Last));
            begin
               for S in Shell_List'Range loop
                  if not Shell_List (S) --  the current item is false
                    and then S'Image = Check_Shell
                  then
                     if not Sh_Compatible (S) then
                        Shell_List (S) := True;
                        Shell_Amount   := Shell_Amount + 1;
                     elsif not Shell_List (sh) then
                        Shell_List (sh) := True;
                        Shell_Amount    := Shell_Amount + 1;
                     end if;
                  end if;
               end loop;
            end;
         end if;
      end Process_Shell;
   begin
      --  TODO: a better way to do this.
      if OS = Windows then
         declare
            Result : constant Shell_Array (1 .. 1) :=
              (1 => (Null_Unbounded_String, null_shell));
         begin
            return Result;
         end;
      end if;
      --  First log current shell.
      if Ada.Environment_Variables.Exists (Shell_Env) then
         Process_Shell (Ada.Environment_Variables.Value (Shell_Env));
      end if;
      --  Next, check the available shells
      if Ada.Directories.Exists (Shell_Path) then
         declare
            Shell_File : File_Type; -- To store the file
         begin
            Open (Shell_File, In_File, Shell_Path);
            --  Iterate through the file, indexing which shells we support.
            while not End_Of_File (Shell_File) loop
               declare
                  Next_Line : constant String := Get_Line (Shell_File);
               begin
                  Process_Shell (Next_Line);
               end;
            end loop;
            Close (Shell_File);
         end;
      end if;
      --  Verify that we have at least 1 found shell.
      if Shell_Amount = 0 then
         raise No_Shells_Found
           with "Cannot locate /etc/shells or $SHELL does not exist." &
           " Please pass ""--no-path""";
      end if;
      --  Create the array, fill it with resulting shells, return the result.
      declare
         Result  : Shell_Array (1 .. Shell_Amount);
         Counter : Positive := 1;
      begin
         for S in Shell_List'Range loop
            if Shell_List (S) then
               Result (Counter).Config_File :=
                 To_Unbounded_String (Get_Shell_Config (S));
               Result (Counter).Shell := S;
               Counter                := Counter + 1;
            end if;
         end loop;
         return Result;
      end;
   end Available_Shells;
end Shells;
