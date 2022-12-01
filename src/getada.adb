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
with Defaults;
with Installer;   use Installer;
with Options;     use Options;
with Settings;    use Settings;
with Ada.Text_IO; use Ada.Text_IO;

procedure Getada is

   --  This will hold our options for running this program.
   Options : constant Program_Options := Process_Arguments;

begin

   --  Welcome the user to our program :)
   if not Options.Quiet then
      Put_Line (Defaults.Welcome_Message);
   end if;

   if Options.Show_Help then
      --  Just show the help message and exit. -q won't count.
      Put_Line (Defaults.Help_Message);
      return;
   end if;

   declare
      Settings : constant Program_Settings := Init_Settings (Options);
   begin
      if Options.Uninstall then
         --  Start our uninstaller.
         --  Uninstall (Settings);
         null;
      else
         --  Start our installer.
         Install (Settings);
      end if;
   end;

end Getada;
