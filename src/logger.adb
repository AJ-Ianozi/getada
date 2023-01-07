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

with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;
package body Logger is
   function Init (Our_Settings : Program_Settings) return Install_Log is
      Result : constant Install_Log :=
        (Current_Settings => Our_Settings, Recent_Stage => No_Stage,
         Steps => (others => (Status => NA, Data => Null_Unbounded_String)));
   begin
      return Result;
   end Init;

   procedure Logit
     (Log            : in out Install_Log; Current_Stage : Stage;
      Current_Status :        Statuses; Current_Data : String := "")
   is
   begin
      Log.Recent_Stage                 := Current_Stage;
      Log.Steps (Current_Stage).Status := Current_Status;
      Append (Log.Steps (Current_Stage).Data, Current_Data & Entry_Seperator);
   end Logit;

   function Get_Recent_Stage (Log : Install_Log) return Stage is
   begin
      return Log.Recent_Stage;
   end Get_Recent_Stage;

   function Get_Status (Log : Install_Log; Of_Stage : Stage) return Statuses is
   begin
      return Log.Steps (Of_Stage).Status;
   end Get_Status;

   function Get_Data (Log : Install_Log; Of_Stage : Stage) return String is
      Data : constant String := To_String (Log.Steps (Of_Stage).Data);
   begin
      --  The data ends with "@" since tha's used as a seperator.
      --  So remove the trailing "@" to make things easier later.
      if Data = "@" or else Data'Length = 0 then
         return "";
      elsif Data (Data'Last) = '@' then
         return Data (Data'First .. Data'Last - 1);
      else
         return Data;
      end if;
   end Get_Data;

   function Get_Settings (Log : Install_Log) return Program_Settings is
   begin
      return Log.Current_Settings;
   end Get_Settings;

   procedure Save (Log : Install_Log; File_Name : String) is
      Log_File   : File_Type;
      Stream_Ptr : Stream_Access;
   begin
      Create (Log_File, Out_File, File_Name);
      Stream_Ptr := Stream (Log_File);
      --  First write out the current settings
      Program_Settings'Write (Stream_Ptr, Log.Current_Settings);
      --  Next, write the recent stage as an unbounded string.
      Unbounded_String'Write
        (Stream_Ptr, To_Unbounded_String (Log.Recent_Stage'Image));
      --  Then, write out the size of the array.
      Natural'Write (Stream_Ptr, Log.Steps'Length);
      --  Finally, write out each array step.
      for I in Log.Steps'Range loop
         --  Specify which step this is
         Unbounded_String'Write (Stream_Ptr, To_Unbounded_String (I'Image));
         --  Write out said step
         Step'Write (Stream_Ptr, Log.Steps (I));
      end loop;
      Close (Log_File);
   end Save;

   function Load (File_Name : String) return Install_Log is
      Log_File   : File_Type;
      Stream_Ptr : Stream_Access;

      --  Temporary variables to know what we're reading.
      Cur_Stage      : Unbounded_String;
      Cur_Array_Size : Natural;

      --  Our result.
      Log : Install_Log;

   begin
      Open (Log_File, In_File, File_Name);
      Stream_Ptr := Stream (Log_File);
      --  First read in the current settings
      Program_Settings'Read (Stream_Ptr, Log.Current_Settings);
      --  Next, read the recent stage.
      Unbounded_String'Read (Stream_Ptr, Cur_Stage);
      --  And set it.
      Log.Recent_Stage := Stage'Value (To_String (Cur_Stage));
      --  Next, the array size
      Natural'Read (Stream_Ptr, Cur_Array_Size);
      --  And load it in.
      declare
         Tmp_Stage : Stage;
      begin
         for I in 1 .. Cur_Array_Size loop
            --  Find out which step this is
            Unbounded_String'Read (Stream_Ptr, Cur_Stage);
            Tmp_Stage := Stage'Value (To_String (Cur_Stage));
            --  Read in the step
            Step'Read (Stream_Ptr, Log.Steps (Tmp_Stage));
         end loop;
      end;
      Close (Log_File);

      return Log;
   end Load;

end Logger;
