with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;

package body Logger is
   function Init (Our_Settings : Program_Settings) return Install_Log_Entry is
      Result : constant Install_Log_Entry :=
        (Current_Settings => Our_Settings,
         Recent_Stage => No_Stage,
         Steps => (others => (Status => NA, Data => Null_Unbounded_String)));
   begin
      return Result;
   end Init;

   procedure Logit
     (Log            : in out Install_Log_Entry;
      Current_Stage  :        Stage;
      Current_Status :        Statuses;
      Current_Data   :        String   := "")
   is
   begin
      Log.Recent_Stage                 := Current_Stage;
      Log.Steps (Current_Stage).Status := Current_Status;
      Append (Log.Steps (Current_Stage).Data, Current_Data & Entry_Seperator);
   end Logit;

   procedure Save (Log       : Install_Log_Entry;
                   File_Name : String)
   is
      Log_File   : File_Type;
      Stream_Ptr : Stream_Access;
   begin
      Create (Log_File, Out_File, File_Name);
      Stream_Ptr := Stream (Log_File);
      Install_Log_Entry'Write (Stream_Ptr, Log);
      Close (Log_File);
   end Save;

   function Load (File_Name : String) return Install_Log_Entry is
      Log_File   : File_Type;
      Stream_Ptr : Stream_Access;
      Log        : Install_Log_Entry;
   begin
      Open (Log_File, In_File, File_Name);
      Stream_Ptr := Stream (Log_File);
      Install_Log_Entry'Read (Stream_Ptr, Log);
      return Log;
   end Load;

end Logger;
