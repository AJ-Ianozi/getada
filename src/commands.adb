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

with Ada.Characters.Handling; use Ada.Characters.Handling;
with GNAT.Expect;             use GNAT.Expect;
with Ada.Text_IO;             use Ada.Text_IO;
with GNAT.OS_Lib;
package body Commands is

   function Test_Command (Cmd : String) return Boolean
   is
      Test_Args : constant GNAT.OS_Lib.Argument_List (1 .. 1) :=
         (1 => new String'("--version"));
      Successfully_Executed : Boolean;
   begin
      GNAT.OS_Lib.Spawn
         (Program_Name => Cmd,
          Args         => Test_Args,
          Success      => Successfully_Executed);
      return Successfully_Executed;
   end Test_Command;

   function Test_Commands return Command_Supported is
      Test_Args : constant GNAT.OS_Lib.Argument_List (1 .. 1) :=
         (1 => new String'("--version"));
   begin
      return Result : Command_Supported do
         --  TODO: A better way of doing this....
         for I in Possible_Commands'Range loop
            begin
               declare
                  Status   : aliased Integer := 0;
                  Response : constant String :=
                     Get_Command_Output
                     (Command => To_Lower (Possible_Commands'Image (I)),
                     Arguments => Test_Args,
                     Input => "",
                     Status  => Status'Access);
               begin
                  Result (I) := True;
               end;
            exception
               when GNAT.Expect.Invalid_Process =>
                  Result (I) := False;
            end;
         end loop;
      end return;
   end Test_Commands;

   function Available_Command (Cmd : Possible_Commands) return Boolean is
      (Supported_Commands (Cmd));

end Commands;