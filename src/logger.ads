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
   type Install_Log_Entry is tagged private;

   function Init
     (Our_Settings : Program_Settings)
      return Install_Log_Entry;

   function Load (File_Name : String) return Install_Log_Entry with
      Pre => Ada.Directories.Exists (File_Name)
      or else raise Invalid_File with "File not found: " & File_Name;

   procedure Save (Log       : Install_Log_Entry;
                   File_Name : String) with
      Post => Ada.Directories.Exists (File_Name)
      or else raise Invalid_File with "Unable to create file " & File_Name;

   --  Logs the data in the actual log structure
   procedure Logit
     (Log            : in out Install_Log_Entry;
      Current_Stage  :        Stage;
      Current_Status :        Statuses;
      Current_Data   :        String   := "");
private

   --  The individual step.
   type Step is record
      Status : Statuses         := NA;
      Data   : Unbounded_String := Null_Unbounded_String;
   end record;
   type Step_Array is array (Stage'Range) of Step;

   --  The logger structure itself.
   type Install_Log_Entry is tagged record
      Current_Settings : Program_Settings; --  Settings used to install
      Recent_Stage     : Stage;            --  The furthest stage it got to
      Steps            : Step_Array;       --  Each step of process
   end record;

end Logger;
