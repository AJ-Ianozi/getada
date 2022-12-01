with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Streams.Stream_IO;
with Ada.Directories;

package Logger is

   Invalid_File : exception;
   type Install_Log_Entry is record
      Version, Cfg_Dir, Bin_Dir : Unbounded_String := Null_Unbounded_String;
   end record;

   procedure Create_Log (File_Name : String; Log : Install_Log_Entry) with
      --  Pre => not Ada.Directories.Exists (File_Name)
      --  or else raise Invalid_File with "File exists: " & File_Name,
      Post => Ada.Directories.Exists (File_Name)
      or else raise Invalid_File with "Unable to create file " & File_Name;

   function Read_Log (File_Name : String) return Install_Log_Entry with
      Pre => Ada.Directories.Exists (File_Name)
      or else raise Invalid_File with "File not found: " & File_Name;
end Logger;
