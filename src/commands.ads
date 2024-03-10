--    Copyright (C) 2024 A.J. Ianozi <aj@ianozi.com>
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

package Commands is
   --  Possible commands that are supported.
   --  We need at least curl or wget.
   type Possible_Commands is (curl, chmod, wget);
   --  Runs "Cmd --version" and prints results.
   function Test_Command (Cmd : String) return Boolean;
   --  Available_Command (curl) = True if curl is supported.
   --  Truth table
   type Command_Supported is array (Possible_Commands'Range) of Boolean;
   --  Generated commands supported function
   function Test_Commands return Command_Supported;
end Commands;