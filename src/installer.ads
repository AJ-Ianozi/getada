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

pragma Assertion_Policy (Check);
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Directories;
with Settings;              use Settings;
with Defaults;
package Installer is

   Invalid_Version, Invalid_Download, Invalid_File, No_Environment_Variable,
   Platform_Not_Yet_Supported, User_Aborted,
   Missing_Dependency : exception;

   --  Just to verify we're using theh correct version format.
   subtype Valid_Version is Character with
        Static_Predicate => Valid_Version in 'A' .. 'Z' | 'a' .. 'z' |
            '0' .. '9' | '-' | '.';

   procedure Install (Our_Settings : Program_Settings) with
      Pre =>
      (for all I in 1 .. Length (Our_Settings.Version) =>
         Element (Our_Settings.Version, I) in Valid_Version
         or else raise Invalid_Version
           with "ERROR: Invalid version: " & To_String (Our_Settings.Version));
private
   --  Generate a random string of size Str_Len
   function Random_String (Str_Len : Natural) return String;
   procedure Extract_Alire (File : String) with
      Pre => Ada.Directories.Exists (File)
      or else raise Invalid_File with "Unable to load file: " & File,
      Post => Ada.Directories.Exists (Defaults.Alire)
      or else raise Invalid_File
        with "Unable to find """ & Defaults.Alire & """ in: " & File;
end Installer;
