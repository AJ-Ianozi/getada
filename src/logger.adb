package body Logger is


   procedure Create_Log(File_Name : String; Log : Install_Log_Entry);

   function Read_Log(File_Name: String) return Install_Log_Entry is
      Log : Install_Log_Entry;
   begin
      
   end;

end Logger;