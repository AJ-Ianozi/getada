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

with Ada.Directories;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Fixed;     use Ada.Strings.Fixed;
with Ada.Text_IO;           use Ada.Text_IO;
with Ada.IO_Exceptions;
with GNAT.OS_Lib;

with Console_IO; use Console_IO;
with Commands;   use Commands;
with Files;      use Files;
with Prompts;    use Prompts;
with Defaults;

package body Uninstaller is

   procedure Process_Uninstall
     (Log     : Install_Log; Our_Settings : Program_Settings;
      Pretend : Boolean := False)
   is
      IO           : constant C_IO             := Init (Our_Settings);
      Old_Settings : constant Program_Settings := Log.Get_Settings;

      Cur_Getada : constant String := To_String (Our_Settings.Exec_Path);

      --  Used to check if Getada has been copied to the binary file or not
      Getada_Copied : Boolean := False;

      --  Removes the env file entry from config files.
      procedure Remove_Env_Files (Data : String; Pretend : Boolean := False) is
         SubStr    : Natural := Data'First;
         Entry_Sep : Natural;
      begin
         --  Read up to the next '@' or end of string.
         loop
            Entry_Sep := Index (Data (SubStr .. Data'Last), Entry_Seperator);
            declare
               Our_Line : constant String :=
                 Data
                   (SubStr ..
                        (if Entry_Sep > 0 then Entry_Sep - 1 else Data'Last));
               --  Split by '%' (it MUST contain a '%')
               Item_Sep : constant Natural := Index (Our_Line, Item_Seperator);
               File     : constant String  :=
                 Our_Line
                   (Our_Line'First ..
                        (if Item_Sep > 0 then Item_Sep - 1
                         else raise Invalid_Log
                             with "Log entry missing item seperator"));
               Command : constant String :=
                 Our_Line (Item_Sep + 1 .. Our_Line'Last);
            begin
               --  Go through the command, removing the file.
               if Line_Exists (File, Command) then
                  declare
                     Tmp_Name : constant String := File & ".tmp"; --  Temp file
                     Orig     : File_Type; --  to load original file
                     Temp     : File_Type; --  to load temporary file
                  begin
                     if Pretend then
                        IO.Must_Say
                          ("Remove line `" & Command & "` from: " & File);
                     else
                        IO.Say_Line
                          ("Removing line `" & Command & "` from: " & File);
                        Open (Orig, In_File, File);
                        Create (Temp, Out_File, Tmp_Name);
                        while not End_Of_File (Orig) loop
                           declare
                              Next_Line : constant String := Get_Line (Orig);
                           begin
                              --  Only write the line if it isn't the command.
                              if Next_Line /= Command then
                                 Put_Line (Temp, Next_Line);
                              end if;
                           end;
                        end loop;
                        Close (Orig);
                        Close (Temp);
                        --  Move the temp file over the original
                        Ada.Directories.Delete_File (File);
                        Ada.Directories.Rename (Tmp_Name, File);
                     end if;
                  end;
               end if;
               exit when Entry_Sep = 0; --  This is the last entry
               SubStr := Entry_Sep + 1; --  Move to next entry if not.
            end;
         end loop;
      end Remove_Env_Files;

      --  Removes a file (if it exists) is
      procedure Remove_File (Data : String; Pretend : Boolean := False) is
      begin
         if Ada.Directories.Exists (Data) then
            if Pretend then
               IO.Must_Say ("Remove: " & Data);
            else
               IO.Say_Line ("Removing: " & Data);
               Ada.Directories.Delete_File (Data);
            end if;
         end if;
      exception
         when Ada.IO_Exceptions.Use_Error =>
            Put_Line (Standard_Error, "Unable to remove file: " & Data);
            Put_Line (Standard_Error, "Reason: In might be in use.");
         when others =>
            Put_Line (Standard_Error, "Unable to remove file: " & Data);
            Put_Line (Standard_Error, "Other error detected.");
      end Remove_File;

      --  Removes a directory (if it exists) is
      procedure Remove_Dir (Data : String; Pretend : Boolean := False) is
      begin
         if Ada.Directories.Exists (Data) then
            if Pretend then
               IO.Must_Say ("Remove: " & Data);
            else
               IO.Say_Line ("Removing: " & Data);
               Ada.Directories.Delete_Directory (Data);
            end if;
         end if;
      exception
         when Ada.IO_Exceptions.Use_Error =>
            Put_Line (Standard_Error, "Unable to remove directory: " & Data);
            Put_Line (Standard_Error, "Reason: Not empty.");
         when others =>
            Put_Line (Standard_Error, "Unable to remove file: " & Data);
            Put_Line (Standard_Error, "Other error detected.");
      end Remove_Dir;

   begin
      --  If not pretending check if current getada binary we're running is
      --  In a path to be deleted.
      --  If it is, we need to move it to a temporary directory and re-run
      --  the uninstaller.
      Ada.Directories.Set_Directory (To_String (Our_Settings.Tmp_Dir));
      if not Pretend and then
         Ada.Directories.Containing_Directory (Cur_Getada) =
            Old_Settings.Bin_Dir
      then
         declare
            use GNAT.OS_Lib;
            Exit_Code     : Process_Id;
            Tmp_Path      : constant String :=
                             Unique_Dir (To_String (Our_Settings.Tmp_Dir));
            New_Getada    : constant String := Tmp_Path &
                              "/" & Defaults.Getada_Command;
            Arg_List : constant Argument_List (1 .. 5) :=
                        (1 => new String'("--uninstall"),
                         2 => new String'("--non-interactive"),
                         3 => new String'("--tmp=" & To_String
                                                      (Our_Settings.Tmp_Dir)),
                         4 => new String'("--cfg=" & To_String
                                                      (Our_Settings.Cfg_Dir)),
                         5 => new String'("--bin=" & To_String
                                                      (Our_Settings.Bin_Dir)));

         begin
            --  Set current working directory to new temp path
            IO.Must_Say
               ("Copying getada from " & Cur_Getada & " to " & New_Getada);
            --  Copy current binary to temp path
            Ada.Directories.Copy_File (Cur_Getada, New_Getada);
            if not Test_Binary (New_Getada, IO, True) then
               raise Defaults.Invalid_File with
                  "Unable to copy uninstaller to temp directory " & New_Getada;
            end if;
            --  Run the new binary and exit.
            IO.Must_Say ("Restarting Uninstaller.");
            Exit_Code := Non_Blocking_Spawn
                           (Program_Name => New_Getada,
                            Args => Arg_List);
            if Exit_Code = Invalid_Pid then
               raise Defaults.Invalid_File with
                  "Unable to run installer from " & New_Getada;
            else
               raise Defaults.Graceful_Exit;
            end if;
         end;
      else
         --  If pretending, show what will happens when uninstalled
         if Pretend then
            IO.Must_Say
            ("The following will be preformed to uninstall Alire and Getada:");
         else
            IO.Must_Say ("Uninstalling...");
         end if;

         for S in reverse Stage'Range loop
            --  Check if work was done on this entry (ignore NA/Failed)
            if Log.Get_Status (S) = Success then
               case S is
                  when Added_Env_File =>
                     --  Remove the env file command from files.
                     Remove_Env_Files (Log.Get_Data (S), Pretend);
                  when Created_Env_File =>
                     --  Remove the env file itself
                     Remove_File (Log.Get_Data (S), Pretend);
                  when Extracted =>
                     --  Remove the extracted binary file
                     Remove_File (Log.Get_Data (S), Pretend);
                  when Copied_Getada =>
                     Getada_Copied := True;
                  when others =>
                     null;
               end case;
            end if;
         end loop;
         --  Remove Getada binary file if it exists
         if Getada_Copied then
            Remove_File (Log.Get_Data (Copied_Getada), Pretend);
         end if;
         --  Remove the getada bin folder if we can.
         Remove_Dir (To_String (Old_Settings.Bin_Dir), Pretend);
         --  Remove log file if we can.
         Remove_File (To_String (Old_Settings.Cfg_Dir & Defaults.Log_File),
                      Pretend);
         --  Remove config directory if we can
         Remove_Dir (To_String (Old_Settings.Cfg_Dir), Pretend);
         if not Pretend then
            IO.Must_Say ("Alire has (hopefully) been uninstalled.");
            IO.Must_Say ("To reinstall, please visit https://www.getada.dev/");
            IO.Must_Say ("Please press any key to continue.");
         end if;
      end if;
   end Process_Uninstall;

   --  Uninstaller utilizing log and program settings. Prompts user if needed.
   procedure Uninstall
     (Log          : Install_Log; Our_Settings : Program_Settings;
      Never_Prompt : Boolean := False)
   is
   begin
      --  If we haven't specified never to prompt and are interactive:
      if not Never_Prompt and then not Our_Settings.Non_Interactive then
         --  Print what the uninstaller will do based on the log.
         Process_Uninstall (Log, Our_Settings, Pretend => True);
         if Get_Answer ("Do you wish to continue?", Default_Answer => No) = No
         then --  Thanks to Donna!  No longer a case statement.
            raise Defaults.User_Aborted;
         end if;
      end if;
      Process_Uninstall (Log, Our_Settings);
   end Uninstall;

   --  General uninstaller just utilizing program settings.
   procedure Uninstall (Our_Settings : Program_Settings) is
      Log_File : constant String :=
        To_String (Our_Settings.Cfg_Dir) & Defaults.Log_File;
   begin
      declare
         Log : constant Install_Log := Load (Log_File);
      begin
         Uninstall (Log, Our_Settings);
      end;
   exception
      when Invalid_File =>
         raise No_Log_Found
           with """" & Log_File & "..." & Defaults.NL &
           "If alire is installed with Getada then please pass:" &
           " --cfg=/directory" & Defaults.NL &
           "where ""/directory"" contains a log.dat file.";
   end Uninstall;
end Uninstaller;