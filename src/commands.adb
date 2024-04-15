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
with Ada.Strings.Fixed;
with GNAT.Expect;             use GNAT.Expect;
with GNAT.OS_Lib;
with Platform; use Platform;
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

   function Is_Executable (Path : String) return Boolean is
      Test_Arg : constant GNAT.OS_Lib.Argument_List (1 .. 1) :=
         (1 => new String'(Path));
   begin
      declare
         Status   : aliased Integer := 0;
         Response : constant String :=
                     Get_Command_Output
                     (Command => "file",
                     Arguments => Test_Arg,
                     Input => "",
                     Status  => Status'Access);
      begin
         return Ada.Strings.Fixed.Index (Response, "executable") > 0;
      exception
         when others => return False;
      end;
   end Is_Executable;

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

   function Test_Binary
       (Binary_To_Test : String; IO : C_IO; Ignore_IO : Boolean := False)
   return Boolean is
      type Checks is (Chmod, Macos_xattr);
      Tested : array (Checks'Range) of Boolean :=
      (Macos_xattr => (if OS = MacOS then False else True),
         others => False);
   begin
      Test_Alire :
      loop
         if not Ignore_IO then
            IO.Say_Line
            ("Testing binary by running """ & Binary_To_Test & " --version""");
         end if;
         if not Test_Command (Binary_To_Test) then
            if not Ignore_IO then
               IO.Say_Line
               ("Unable to run binary... Attempting to troubleshoot.");
            end if;
            if not Tested (Chmod) then
               declare
                  Chmod_Success : Boolean;
                  Chmod_Args : constant GNAT.OS_Lib.Argument_List
                  (1 .. 2) :=
                  (1 => new String'("+x"),
                     2 => new String'(Binary_To_Test));
               begin
                  --  TODO: Handle if they don't have chmod
                  --    or if it's not stored at /bin/chmod
                  if not Ignore_IO then
                     IO.Say_Line
                     ("Attempting ""/bin/chmod +x " & Binary_To_Test & """");
                  end if;
                  GNAT.OS_Lib.Spawn
                  (Program_Name => "/bin/chmod", Args => Chmod_Args,
                     Success      => Chmod_Success);
                  Tested (Chmod) := True;
               end;
            elsif not Tested (Macos_xattr) then
               declare
                  xattr_Success : Boolean;
                  xattr_Args : constant GNAT.OS_Lib.Argument_List
                  (1 .. 3) :=
                  (1 => new String'("-d"),
                     2 => new String'("com.apple.quarantine"),
                     3 => new String'(Binary_To_Test));
               begin
                  if not Ignore_IO then
                     IO.Say_Line
                     ("Attempting ""/usr/bin/xattr -d " &
                        "com.apple.quarantine " & Binary_To_Test & """");
                  end if;
                  GNAT.OS_Lib.Spawn
                  (Program_Name => "/usr/bin/xattr", Args => xattr_Args,
                     Success      => xattr_Success);
               end;
               Tested (Macos_xattr) := True;
            else -- not chmod_tried
               return False;
            end if;
         else
            if not Ignore_IO then
               IO.Say_Line ("Sucessfully able to run binary.");
            end if;
            exit Test_Alire;
         end if;
      end loop Test_Alire;
      return True;
   end Test_Binary;

end Commands;