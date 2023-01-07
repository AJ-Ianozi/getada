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

with Console_IO; use Console_IO;
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
            Put_Line (Standard_Error, "Reason: In Use.");
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
            Put_Line (Standard_Error, "Reason: In Use");
         when others =>
            Put_Line (Standard_Error, "Unable to remove file: " & Data);
            Put_Line (Standard_Error, "Other error detected.");
      end Remove_Dir;

   begin
      --  Show what will happen when uninstalled.
      if Pretend then
         IO.Must_Say
           ("The following will be preformed to uninstall Alire and Getada:");
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
               when Downloaded =>
                  --  Remove the downloaded file
                  Remove_File (Log.Get_Data (S), Pretend);
               when others =>
                  null;
            end case;
         end if;
      end loop;
      --  Remove metadata, bin dir, log, and config directory.
      Remove_Dir (To_String (Old_Settings.Tmp_Dir), Pretend);
      Remove_Dir (To_String (Old_Settings.Bin_Dir), Pretend);
      Remove_File
        (To_String (Old_Settings.Cfg_Dir & Defaults.Log_File), Pretend);
      Remove_Dir (To_String (Old_Settings.Cfg_Dir), Pretend);
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
            raise User_Aborted;
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
         Put_Line ("crap");
         raise No_Log_Found
           with """" & Log_File & "..." & Defaults.NL &
           "If alire is installed with Getada then please pass:" &
           " --cfg=/directory" & Defaults.NL &
           "where ""/directory"" contains a log.dat file.";
   end Uninstall;
end Uninstaller;
