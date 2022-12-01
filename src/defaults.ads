with Ada.Characters.Latin_1;
package Defaults is
   --  Directories
   Tmp_Dir : constant String := "/.cache/getada";
   Cfg_Dir : constant String := "/.getada";
   Bin_Dir : constant String := "/bin";

   --  Environmental Variables
   Tmp_Env : constant String := "GETADA_TMP";
   Cfg_Env : constant String := "GETADA_CFG";
   Bin_Env : constant String := "GETADA_BIN";
   Ver_Env : constant String := "GETADA_ALIRE_VERSION";

   --  Alire binary file
   Alire : constant String := "alr";

   --  Messages
   NL : constant String :=
     Ada.Characters.Latin_1.CR & Ada.Characters.Latin_1.LF;

   Welcome_Message : constant String :=
     "Welcome to the unofficial Alire Installer (""GetAda"")!" & NL &
     "Alire is the official Ada Package Manager. For more information" & NL &
     "please visit https://ada-lang.io or https://alire.ada.dev" & NL &
     "Copyright (C) 2022 A.J. Ianozi licensed GPL3.";

   Help_Message : constant String :=
     "Options: " & NL & "-h --help: Print this message and exit." & NL &
     "-p --no-path: Don't update path." & NL &
     "-n --non-interactive: Suppress prompts; answer with defaults." & NL &
     "-q --quiet: Be quiet (does not suppress propmts)" & NL &
     "-m /directory --tmp=/directory: Set tmp/metadata " & NL &
     "-c /directory --cfg=/directory: Set config directory " & NL &
     "-b /directory --bin=/directory: Set binary directory " & NL &
     "-v x.y.z --version=x.y.z: Download a specific version." & NL &
     "-u --uninstall: Uninstall Alire. This only works if Alire was" & NL &
     "                installed in a default directory or --cfg is passed." &
     NL & "You can also set the version and metadata / binary directory by " &
     NL & "setting the following environment variables:" & NL & " * """ &
     Ver_Env & """  for Alire's version" & NL & " * """ & Tmp_Env &
     """ for metadata directory" & NL & " * """ & Cfg_Env &
     """ for config directory" & NL & " * """ & Bin_Env &
     """ for binary directory" & NL & "That's it for right now!";

end Defaults;
