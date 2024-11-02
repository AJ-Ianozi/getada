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

with Ada.Characters.Latin_1;
package Defaults is
   --  Exceptions
   Invalid_Version, Invalid_Download, Invalid_File, No_Environment_Variable,
   Platform_Not_Yet_Supported, User_Aborted, Graceful_Exit,
   Missing_Dependency : exception;

   --  About GetAda
   Getada_Version : constant String := "1.0.1"; --  Must match alire.toml

   --  Directories, starting at root.
   Tmp_Dir  : constant String := "/tmp"; --  Location in temporary directory

   --  Directories, starting at $HOME
   Cfg_Dir  : constant String := "/.getada"; --  Config, scripts, bin
   Bin_Dir  : constant String := "/bin";     --  Located in Cfg_Dir
   Log_File : constant String := "/log.dat"; --  Located in Cfg_Dir

   --  Environmental Variables
   Tmp_Env : constant String := "TMPDIR";               --  Override Tmp_Dir
   Cfg_Env : constant String := "GETADA_CFG";           --  Override Cfg_Dir
   Bin_Env : constant String := "GETADA_BIN";           --  Override Bin_Dir
   Ver_Env : constant String := "GETADA_ALIRE_VERSION"; --  Override latest ver

   --  Alire binary file
   Alire_Command : constant String := "alr";
   --  Getada binary file
   Getada_Command : constant String := "getada";

   --  Messages
   NL : constant String :=
     Ada.Characters.Latin_1.CR & Ada.Characters.Latin_1.LF;

   Welcome_Message : constant String :=
     "Welcome to the unofficial Alire Installer (""GetAda"") v" &
     Getada_Version & "!" & NL &
     "Alire is the official Ada Package Manager. For more information" & NL &
     "please visit https://ada-lang.io or https://alire.ada.dev" & NL &
     "Copyright (C) 2022-2024 A.J. Ianozi licensed GPL3.";

   Help_Message : constant String :=
     "Options: " & NL &
     "-h --help: Print this message and exit." & NL &
     "-v --version: Print the version of this binary and exit." & NL &
     "-p --no-path: Don't update path." & NL &
     "-n --non-interactive: Suppress prompts; answer with defaults." & NL &
     "-q --quiet: Be quiet (does not suppress propmts)" & NL &
     "-t /directory --tmp=/directory: Location of tmp directory " & NL &
     "-c /directory --cfg=/directory: Set config directory " & NL &
     "-b /directory --bin=/directory: Set binary directory " & NL &
     "-a x.y.z --alire-version=x.y.z: Download version x.y.z of alire." & NL &
     "-u --uninstall: Uninstall Alire. This only works if Alire was" & NL &
     "                installed with GetAda.  Works out of the box if" & NL &
     "                default directory was used, otherwise you must" & NL &
     "                pass --cfg= so the uninstaller can find the log." & NL &
     "You can also set the version and tmp/cfg/binary directories by " & NL &
     "setting the following environment variables:" & NL &
     " * """ & Ver_Env & """ for Alire's version" & NL &
     " * """ & Tmp_Env & """ for location of temp directory" & NL &
     " * """ & Cfg_Env & """ for config directory" & NL &
     " * """ & Bin_Env & """ for binary directory" & NL &
     "That's it for right now!";

end Defaults;
