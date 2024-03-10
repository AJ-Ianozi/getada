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
with Ada.Environment_Variables;
with Ada.Directories;
with Platform;
with Defaults;
package body Settings is

   function Correct_Path (Home_Dir : String; Path : String) return String is
      Corrected_Path : constant String :=
        (if Path (Path'First) = '~' then
           Home_Dir & "/" &
           (if Path'Length > 1 then Path (Path'First + 1 .. Path'Last) else "")
         else
           Path);
   begin
      return Ada.Directories.Full_Name (Corrected_Path);
   end Correct_Path;

   function Init_Settings (Options : Program_Options) return Program_Settings
   is
      use Platform;
      --  On windows it's "HOMEPATH", but unix is "HOME".
      Home_Env : constant String :=
        (case OS is when Windows => "HOMEPATH",
           when others                    => "HOME");

      Home_Dir : constant String :=
        (if Ada.Environment_Variables.Exists (Home_Env) then
           Ada.Environment_Variables.Value (Home_Env)
         else raise No_Environment_Variable
             with "Cannot find the $HOME environment variable. " &
             "No home directory can be found.");

      Version : constant String :=
        (if Options.Version /= Null_Unbounded_String then
           To_String (Options.Version)
         elsif Ada.Environment_Variables.Exists (Defaults.Ver_Env) then
           Ada.Environment_Variables.Value (Defaults.Ver_Env)
         else "");

      Tmp_Dir : constant String :=
        Correct_Path
          (Home_Dir,
           (if Options.Tmp_Dir /= Null_Unbounded_String then
              To_String (Options.Tmp_Dir)
            else
              (if Ada.Environment_Variables.Exists (Defaults.Tmp_Env) then
                 Ada.Environment_Variables.Value (Defaults.Tmp_Env)
               else Defaults.Tmp_Dir)));

      --  TODO: Need to decide on a location...
      --  since alire uses ~/.config/alire for everything maybe
      --  we have ~/.config/getada/env.sh instead of ~/.getada/env.sh ?
      Cfg_Dir : constant String :=
        Correct_Path
          (Home_Dir,
           (if Options.Cfg_Dir /= Null_Unbounded_String then
              To_String (Options.Cfg_Dir)
            else
              (if Ada.Environment_Variables.Exists (Defaults.Cfg_Env) then
                 Ada.Environment_Variables.Value (Defaults.Cfg_Env)
               else Home_Dir & Defaults.Cfg_Dir)));
      --  TODO: Put this in Program Files on Windows +
      --       and Home_Dir/Applications/bin on MacOS?
      --       do something like:
      --       Local_Apps: constant String := Local_Settings.App_Dir;
      Bin_Dir : constant String :=
        Correct_Path
          (Home_Dir,
           (if Options.Bin_Dir /= Null_Unbounded_String then
              To_String (Options.Bin_Dir)
            else
              (if Ada.Environment_Variables.Exists (Defaults.Bin_Env) then
                 Ada.Environment_Variables.Value (Defaults.Bin_Env)
               else Cfg_Dir & Defaults.Bin_Dir)));

      Path_Env : constant String :=
        (if Ada.Environment_Variables.Exists ("PATH") then
           Ada.Environment_Variables.Value ("PATH")
         else "");

      Our_Settings : constant Program_Settings :=
        (Version          => To_Unbounded_String (Version),
         Tmp_Dir          => To_Unbounded_String (Tmp_Dir),
         Cfg_Dir          => To_Unbounded_String (Cfg_Dir),
         Bin_Dir          => To_Unbounded_String (Bin_Dir),
         Home_Dir         => To_Unbounded_String (Home_Dir),
         Path_Env         => To_Unbounded_String (Path_Env),
         No_Update_Path   => Options.No_Update_Path,
         Non_Interactive  => Options.Non_Interactive, Quiet => Options.Quiet);
   begin
      return Our_Settings;
   end Init_Settings;

end Settings;
