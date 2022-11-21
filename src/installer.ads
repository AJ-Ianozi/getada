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
with Ada.Directories;
with Options;               use Options;
package Installer is
   package Defaults is
      --  Directories
      Tmp_Dir : constant String := "/.cache/getada";
      Cfg_Dir : constant String := "/.getada";
      Bin_Dir : constant String := "/bin";

      --  Environmental Variables
      Tmp_Env : constant String := "GETADA_TMP";
      Cfg_Env : constant String := "GETADA_CFG";
      Bin_Env : constant String := "GETADA_BIN";
      Ver_Env : constant String := "GETADA_ALIRE_VERSION";

   end Defaults;

   --  The binary for Alire.
   Alire : constant String := "alr";

   Invalid_Version, Invalid_Download, Invalid_File, No_Environment_Variable,
   OS_Not_Yet_Supported, User_Aborted : exception;
   type Yes_or_No is (No, Yes, NA);
   --  Just to verify we're using theh correct version format.
   subtype Valid_Version is Character with
        Static_Predicate => Valid_Version in 'A' .. 'Z' | 'a' .. 'z' |
            '0' .. '9' | '-' | '.';

   procedure Install (Options : Program_Options) with
      Pre =>
      (for all I in 1 .. Length (Options.Version) =>
         Element (Options.Version, I) in Valid_Version
         or else raise Invalid_Version
           with "ERROR: Invalid version: " & To_String (Options.Version));
private
   function Correct_Path (Home_Dir : String; Path : String) return String;
   function Get_Answer
     (Prompt : String; Default_Answer : Yes_or_No := NA) return Yes_or_No;
   function Get_Answer
     (Prompt : String; Default_Answer : String := "") return String;
   --  Will add this back in when we have AWS
   --  procedure Download (URL : String; Destination_File : String);
   procedure Download (URL : String);
   procedure Extract_Alire (File : String) with
      Pre => Ada.Directories.Exists (File)
      or else raise Invalid_File with "Unable to load file: " & File,
      Post => Ada.Directories.Exists ("alr")
      or else raise Invalid_File with "Unable to find ""alr"" in: " & File;
end Installer;
