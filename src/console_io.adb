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

with Ada.Text_IO; use Ada.Text_IO;
package body Console_IO is
   function Init (Our_Settings : Program_Settings) return C_IO is
      Result : constant C_IO :=
        (Quiet           => Our_Settings.Quiet,
         Non_Interactive => Our_Settings.Non_Interactive);
   begin
      return Result;
   end Init;
   --  Posts a message, but only if messages aren't suppressed.
   procedure Say_Line (This : C_IO; Item : String) is
   begin
      if not This.Quiet then
         Put_Line (Item);
      end if;
   end Say_Line;
   --  Like Say_Line but suppresses if quiet AND non-interactive.
   procedure Must_Say (This : C_IO; Item : String) is
   begin
      if not (This.Quiet and then This.Non_Interactive) then
         Put_Line (Item);
      end if;
   end Must_Say;

   function Say (This : C_IO; Item : String) return String is
   begin
      return (if not This.Quiet then Item else "");
   end Say;
end Console_IO;
