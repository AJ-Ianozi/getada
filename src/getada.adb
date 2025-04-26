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
with Defaults;    use Defaults;
with Platform;    use Platform;
with Installer;   use Installer;
with Uninstaller; use Uninstaller;
with Options;     use Options;
with Settings;    use Settings;
with Ada.Text_IO; use Ada.Text_IO;

procedure Getada is

   --  This will hold our options for running this program.
   Options : constant Program_Options := Process_Arguments;

begin

   --  Welcome the user to our program :)
   if not Options.Quiet and then not Options.Show_Version then
      Put_Line (Welcome_Message);
   end if;

   if Options.Show_Help then
      --  Just show the help message and exit. -q won't count.
      Put_Line (Help_Message);
   elsif Options.Show_Version then
      --  Just show the version and exit.  -q won't count.
      Put_Line (Getada_Command & " " & Getada_Version);
   else
      --  Check if the platform is currently supported by alire.
      --  If etiher alire or I start building/distributing binaries for alr
      --  then this can be updated.
      case OS is
         when MacOS =>
            if Arch not in x86_64 | aarch64 then
               raise Platform_Not_Yet_Supported with NL &
               "----------------------------------------" &
               "----------------------------------------" & NL &
               "Currently only x86_64/aarch64 is supported on MacOS" & NL &
               "Alire may be built from source code from " & NL &
               "https://github.com/alire-project/alire" & NL &
               "----------------------------------------" &
               "----------------------------------------";
            end if;
         when Linux =>
            if Arch not in x86_64 | aarch64 then
               raise Platform_Not_Yet_Supported with NL &
               "----------------------------------------" &
               "----------------------------------------" & NL &
               "Currently only x86_64/aarch64 is supported on MacOS" & NL &
               "Alire may be built from source code from " & NL &
               "https://github.com/alire-project/alire" & NL &
               "----------------------------------------" &
               "----------------------------------------";
            end if;
         when Windows =>
            raise Platform_Not_Yet_Supported with NL &
            "----------------------------------------" &
            "----------------------------------------" & NL &
            "NOTE: Windows installation is not supported by this tool!" & NL &
            " Please use alire's installer on https://alire.ada.dev/" &
            NL & "----------------------------------------" &
            "----------------------------------------";
         when FreeBSD =>
            raise Platform_Not_Yet_Supported with NL &
            "----------------------------------------" &
            "----------------------------------------" & NL &
            "NOTE: FreeBSD installation is not ready yet!" & NL &
            "Please install Alire via ports: " & NL &
            "https://cgit.freebsd.org/ports/log/devel/alire" & NL &
            "----------------------------------------" &
            "----------------------------------------";
      end case;
      --  Take our settings and start installer or uninstaller
      declare
         Settings : constant Program_Settings := Init_Settings (Options);
      begin
         if Options.Uninstall then
            --  Start our uninstaller.
            Uninstall (Settings);
         else
            --  Start our installer.
            Install (Settings);
         end if;
      end;
   end if;
exception
   when User_Aborted =>
      Put_Line ("Aborted... Closing program.");
   when Graceful_Exit => null;
end Getada;
