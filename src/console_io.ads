--    Copyright (C) 2022-2023 A.J. Ianozi <aj@ianozi.com>
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

with Settings; use Settings;

package Console_IO is
   type C_IO is tagged private;
   --  Initiate the console IO
   function Init (Our_Settings : Program_Settings) return C_IO;

   --  Prints the line if quiet is not enabled
   procedure Say_Line (This : C_IO; Item : String);

   --  Returns string if quiet is not enabled
   function Say (This : C_IO; Item : String) return String;

   --  Prints even if quiet is enabled
   procedure Must_Say (This : C_IO; Item : String);
private
   type C_IO is tagged record
      Quiet           : Boolean;
      Non_Interactive : Boolean;
   end record;

end Console_IO;
