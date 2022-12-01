with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Options;               use Options;
with Local_Settings;        use Local_Settings;
package Settings is

   No_Environment_Variable : exception;

   type Program_Settings is record
      Current_Platform : Platform;
      Version          : Unbounded_String;
      Tmp_Dir          : Unbounded_String;
      Cfg_Dir          : Unbounded_String;
      Bin_Dir          : Unbounded_String;
      Home_Dir         : Unbounded_String;
      Path_Env         : Unbounded_String;
      No_Update_Path   : Boolean;
      Non_Interactive  : Boolean;
      Quiet            : Boolean;
   end record;

   --  Takes program options and initalizes the settings we'll install.
   function Init_Settings (Options : Program_Options) return Program_Settings;

private
   function Correct_Path (Home_Dir : String; Path : String) return String;

end Settings;
