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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Options;               use Options;
with Local_Settings;        use Local_Settings;
package Settings is

   No_Environment_Variable : exception;

   type Program_Settings is record
      Current_Platform : Platform;
      Version          : Unbounded_String;
      Tmp_Dir          : Unbounded_String;
      Cfg_Dir          : Unbounded_String;
      Bin_Dir          : Unbounded_String;
      Home_Dir         : Unbounded_String;
      Path_Env         : Unbounded_String;
      No_Update_Path   : Boolean;
      Non_Interactive  : Boolean;
      Quiet            : Boolean;
   end record;

   --  Takes program options and initalizes the settings we'll install.
   function Init_Settings (Options : Program_Options) return Program_Settings;

private
   function Correct_Path (Home_Dir : String; Path : String) return String;

end Settings;
