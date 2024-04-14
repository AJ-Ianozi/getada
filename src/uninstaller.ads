--    Copyright (C) 2023 A.J. Ianozi <aj@ianozi.com>
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
with Settings; use Settings;
with Logger;   use Logger;

package Uninstaller is
   No_Log_Found : exception;
   Invalid_Log  : exception;
   --  Starts uninstaller for getada+alire based on provided settings.
   procedure Uninstall (Our_Settings : Program_Settings);

   --  Starts uninstaller for getada+alire, potentially with prompts.
   procedure Uninstall
     (Log          : Install_Log; Our_Settings : Program_Settings;
      Never_Prompt : Boolean := False);
private
   --  Uninstalls getada+alire.
   procedure Process_Uninstall
     (Log     : Install_Log; Our_Settings : Program_Settings;
      Pretend : Boolean := False);
end Uninstaller;
