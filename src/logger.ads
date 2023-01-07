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

pragma Assertion_Policy (Check);
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Directories;

with Settings; use Settings;

package Logger is

   Invalid_File : exception;
   --  Entries in data log are separated by @, e.g. "Entry1@Entry2@Entry3"
   Entry_Seperator : constant String := "@";
   --  If more than one item per entry, e.g. "Entry1@Entry2A%Entry2B@Entry3"
   Item_Seperator : constant String := "%";
   --  This holds the stages of our installer, in order of process.
   type Stage is
     (No_Stage,
      Created_Metadata,
      Downloaded,
      Created_Cfg,
      Created_Bin,
      Extracted,
      Alr_Tested,
      Created_Env_File,
      Added_Env_File);
   --  The status of each step
   type Statuses is (Failed, Success, NA);

   --  The logger structure itself.
   type Install_Log is tagged private;

   --  Initiates the logger structure.
   function Init (Our_Settings : Program_Settings) return Install_Log;

   --  Loads the logger from a file
   function Load (File_Name : String) return Install_Log with
      Pre => Ada.Directories.Exists (File_Name)
      or else raise Invalid_File with "File not found: " & File_Name;

   --  Saves the logger to a file
   procedure Save (Log : Install_Log; File_Name : String) with
      Post => Ada.Directories.Exists (File_Name)
      or else raise Invalid_File with "Unable to create file " & File_Name;

   --  Logs the data in the actual log structure
   procedure Logit
     (Log            : in out Install_Log; Current_Stage : Stage;
      Current_Status :        Statuses; Current_Data : String := "");

   --  Retrives the latest stage this log got to.
   function Get_Recent_Stage (Log : Install_Log) return Stage;

   --  Retrives the status of a log entry of x stage.
   function Get_Status (Log : Install_Log; Of_Stage : Stage) return Statuses;

   --  Retrives the data.
   function Get_Data (Log : Install_Log; Of_Stage : Stage) return String;

   --  Retrives the settings from the log
   function Get_Settings (Log : Install_Log) return Program_Settings;
private

   --  The individual step.
   type Step is record
      Status : Statuses         := NA;
      Data   : Unbounded_String := Null_Unbounded_String;
   end record;
   type Step_Array is array (Stage'Range) of Step;

   --  The logger structure itself.
   type Install_Log is tagged record
      Current_Settings : Program_Settings; --  Settings used to install
      Recent_Stage     : Stage;            --  The furthest stage it got to
      Steps            : Step_Array;       --  Each step of process
   end record;

end Logger;
