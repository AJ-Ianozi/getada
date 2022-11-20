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
with Installer; use Installer;
with Options;   use Options;
with Ada.Text_IO; use Ada.Text_IO;

with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;

procedure Getada is

   procedure Show_Welcome is
      Welcome_Message : constant String :=
        "Welcome to the unofficial Alire Installer (""GetAda"")!" & CR & LF &
        "Alire is the official Ada Package Manager. For more information" &
        CR & LF & "please visit https://ada-lang.io or https://alire.ada.dev" &
        CR & LF & "This program is Copyright (C) 2022 A.J. Ianozi";
   begin
      Put_Line (Welcome_Message);
   end Show_Welcome;

   procedure Show_Help is
      Help_Message : constant String :=
        "Options: " & CR & LF & "-h --help: Print this message and exit." &
        CR & LF & "-p --no-path: Don't update path." & CR & LF &
        "-m /directory --meta=/directory: Set tmp/metadata " & CR &
        LF & "-c /directory --cfg=/directory: Set config directory " & CR &
        LF & "-b /directory --bin=/directory: Set binary directory " & CR &
        LF & "-v x.y.z --version=x.y.z: Download a specific version." & CR &
        LF &
        "You can also set the version and metadata / binary directory by " &
        "setting the following environment variables:" & CR & LF &
        " * ""GETADA_ALIRE_VERSION"" for Alire's version" & CR & LF &
        " * ""GETADA_TMP"" for metadata directory" & CR & LF &
        " * ""GETADA_CFG"" for config directory" & CR & LF &
        " * ""GETADA_BIN"" for binary directory" & CR & LF &
        "That's it for right now!";
   begin
      Put_Line (Help_Message);
   end Show_Help;

   --  This will hold our options for running this program.
   Options : constant Program_Options := Process_Arguments;
begin

   --  Welcome the user to our program :)
   Show_Welcome;

   if Options.Show_Help then
      --  Just show the help message and exit.
      Show_Help;
      return;
   else

      --  Start our installer.
      Install (Options);
   end if;

end Getada;
