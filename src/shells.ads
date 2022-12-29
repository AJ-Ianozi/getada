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

pragma Assertion_Policy (Check);
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Local_Settings;        use Local_Settings;
with Ada.Text_IO;           use Ada.Text_IO;

package Shells is
   No_Shells_Found   : exception;
   Unsupported_Shell : exception;
   --  Set the supported shells here.
   type Supported_Shells is
     (null_shell,
      sh,
      bash, --  bash will use sh's .profile
      zsh);
   --  This is for returning a list of path files.
   type Shell_Config is record
      Config_File : Unbounded_String;
      Shell       : Supported_Shells;
   end record;
   type Shell_Array is array (Positive range <>) of Shell_Config;
   function Available_Shells (Current_Platform : Platform) return Shell_Array;
   function Get_Shell_Env (Shell_Name : Supported_Shells) return String;
   procedure Write_Env_File
     (Shell_Name : Supported_Shells; File : File_Type; Dir : String);
   function Get_Env_Command
     (Shell_Name : Supported_Shells; Env_File : String) return String;

private
   --  Gets the config file for each shell.
   function Get_Shell_Config (Shell_Name : Supported_Shells) return String;
   function Sh_Compatible (Shell_Name : Supported_Shells) return Boolean;
end Shells;
