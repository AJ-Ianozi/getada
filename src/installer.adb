--    Copyright (C) 2022-2024 A.J. Ianozi <aj@ianozi.com>
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

--  for downloader:
--  TODO: Add this when AWS can support https in alire
--    with AWS.Client;
--    with AWS.Response;
--    with AWS.Resources;
--    with Ada.Streams.Stream_IO;
with GNAT.Expect;    use GNAT.Expect;
with GNAT.OS_Lib;
--  Required for reading the json versions and getting the download URL.
with JSON.Parsers;
with JSON.Types;

with Console_IO;  use Console_IO;
with Shells;      use Shells;
with Prompts;     use Prompts;
with Logger;      use Logger;
with Files;       use Files;
with Uninstaller; use Uninstaller;
with Platform;    use Platform;
with Commands;    use Commands;
with Options;

with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Text_IO;       use Ada.Text_IO;

with Zip;
with UnZip;

--  For random string generator
with Ada.Numerics.Discrete_Random;

package body Installer is

--  This will be used once alire's AWS supports https.
--  For the moment: just using curl.
--  procedure Download (URL : String; Destination_File : String) is
--      use AWS;
--      use Ada.Streams;
--      Result : constant Response.Data := Client.Get(URL => URL);
   procedure Download (URL : String) is
      Cmd  : constant String := "curl";
               --(if Available_Command (curl) then "curl"
               -- elsif Available_Command (wget) then "wget"
               -- else raise Missing_Dependency);
      Arg : constant String := "-OJL";
               --(if Available_Command (curl) then "-OJL"
               -- elsif Available_Command (wget) then "-q"
               -- else raise Missing_Dependency);
      Args : constant GNAT.OS_Lib.Argument_List (1 .. 2) :=
         (1 => new String'(Arg),
          2 => new String'(URL));
      Status   : aliased Integer := 0;
      Response : constant String :=
        Get_Command_Output
          (Command => Cmd, Arguments => Args, Input => "",
           Status  => Status'Access);
   begin
--  This will be used once alire's AWS supports https.
--  For the moment: just using curl.
--      if Response.Content_Type (Result) = "application/zip" then
--         declare
--            Message_Stream : Resources.File_Type;
--            Buffer         : Stream_Element_Array (1 .. 4096);
--            Last           : Stream_Element_Offset;
--            File           : Stream_IO.File_Type;
--         begin
--            Response.Message_Body (Result, Message_Stream);
--            Stream_IO.Create (File, Stream_IO.Out_File, Destination_File);
--            loop
--               Resources.Read (Message_Stream, Buffer, Last);
--               Stream_IO.Write (File, Buffer (1 .. Last));
--               exit when Last < Buffer'Last;
--            end loop;
--            Stream_IO.Close (File);
--         end;
--      else
--         raise Invalid_Download with
--            "Unable to download: File is not of type zip.";
--      end if;
      null;
   end Download;

   --  Creates a random string.
   function Random_String (Str_Len : Natural) return String is
      Alpha_Num : constant array (1 .. 62) of Character :=
         ('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd',
          'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r',
          's', 't', 'u', 'v', 'w', 'x', 'y', 'z', 'A', 'B', 'C', 'D', 'E', 'F',
          'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T',
          'U', 'V', 'W', 'X', 'Y', 'Z');
      subtype Alpha_Range is Integer range 1 .. 62;
      package Rand_Gen is new Ada.Numerics.Discrete_Random (Alpha_Range);
      use Rand_Gen;
      Gen : Generator;
   begin
      Reset (Gen);
      return Result : String (1 .. Str_Len) do
         for I in 1 .. Str_Len loop
            Result (I) := Alpha_Num (Random (Gen));
         end loop;
      end return;
   end Random_String;

   procedure Extract_Alire (File : String) is
      use Zip, UnZip;
      Zip_File : Zip_info;
   begin
      Load (Zip_File, File);
      --  If alire decides to start packing alr somewhere besides bin/alr
      --  then this will break.
      --  Also this won't work on Windows :)
      if Exists (Zip_File, "bin/alr") then
         Extract (from => File, what => "bin/alr", rename => Defaults.Alire);
      elsif Exists (Zip_File, "bin/alr.exe") then
         Extract
           (from   => File, what => "bin/alr",
            rename => Defaults.Alire & ".exe");
      else
         raise Invalid_File
           with "Archive does not contain bin/alr." &
           " If Alire has changed their zip structure, file a bug report.";
      end if;
   end Extract_Alire;

   procedure Install (Our_Settings : Program_Settings) is
      --  For easier newlines.
      NL : constant String := Defaults.NL;

      IO : constant C_IO := Init (Our_Settings);

      Home_Dir : constant String := To_String (Our_Settings.Home_Dir);
      Tmp_Dir  : constant String := To_String (Our_Settings.Tmp_Dir);
      Cfg_Dir  : constant String := To_String (Our_Settings.Cfg_Dir);
      --  TODO: Put this in Program Files on Windows +
      --       and Home_Dir/Applications/bin on MacOS?
      --       do something like:
      --       Local_Apps: constant String := Local_Settings.App_Dir;
      Bin_Dir : constant String := To_String (Our_Settings.Bin_Dir);
      Version : constant String := To_String (Our_Settings.Version);

      --  The final resting place of the alire binary.
      Alire_Binary : constant String :=
        Bin_Dir & "/" & Defaults.Alire &
        (if Platform.OS = Windows then ".exe" else "");

      Our_Shells : constant Shell_Array :=
        (if not Our_Settings.No_Update_Path then
            Available_Shells
         else (1 => (Null_Unbounded_String, null_shell)));

      --  For logging as we move through.
      Log : Install_Log := Init (Our_Settings);

      Settings_Message : constant String :=
        NL &
        (if Version /= "" then
           "I will attempt to fetch version """ & Version & """"
         else "No version has been specified. Will attempt to install the " &
           "latest version of Alire." &
           IO.Say (NL & "(To specify a version, pass --version=x.y.z)")) &
        NL & "Temporary files will be stored in a folder in: " &
        NL & Tmp_Dir & NL &
        IO.Say
          (NL & "(This can be changed with the """ & Defaults.Tmp_Env & """ " &
           "environment variable or passing --tmp=/directory/here)" & NL) &
        NL & "Any of alire's scripts or helper files will store in " &
        "the following location:" & NL & Cfg_Dir & NL &
        IO.Say
          (NL & "(This can be changed either by setting the """ &
           Defaults.Cfg_Env &
           """ environment variable or passing --cfg=/directory/here)" & NL) &
        NL & "Alire's binary will be installed as """ & Defaults.Alire &
        """ in " & "the following location:" & NL & Bin_Dir & NL &
        IO.Say
          (NL & "(This can be changed either by setting the """ &
           Defaults.Bin_Env & """ " &
           "environment variable or passing --bin=/directory/here)" & NL);

   begin

      if OS = Windows then
         raise OS_Not_Yet_Supported
           with NL & "----------------------------------------" &
           "----------------------------------------" & NL &
           "NOTE: Windows installation is not ready yet!" & NL &
           " I recommend using alire's installer on https://alire.ada.dev/" &
           NL & "----------------------------------------" &
           "----------------------------------------";
      end if;

      --  TODO: Add unix type for linux/macos/freebsd/etc --
      case OS is
         when MacOS | Linux =>
            null;
         when others =>
            raise OS_Not_Yet_Supported
              with "The current OS is not yet supported. " &
              "Should never get here! ";
      end case;

      if False then --not Available_Command (curl) and then Available_Command (wget) then 
         raise Missing_Dependency
           with NL & "----------------------------------------" &
           "----------------------------------------" & NL &
           "You must have curl or wget to use GetAda." & NL &
           " Please at least install curl and then re-run." &
           NL & "----------------------------------------" &
           "----------------------------------------";
      end if;

      IO.Must_Say (Settings_Message);

      if not Our_Settings.No_Update_Path then

         IO.Must_Say
           ("This path will be added to your local PATH variable by " &
            "modifying the following files:");

         for Shell of Our_Shells loop
            IO.Must_Say (Home_Dir & "/" & To_String (Shell.Config_File));
         end loop;

         IO.Must_Say ("(This can be changed by passing --no-path)" & NL);

         IO.Must_Say ("You can revert everything that was done by re-running" &
                      " GetAda with the --uninstall option." & NL);

      end if;
      --  Ask user if they want to install the propgram, or offer interactive
      if not Our_Settings.Non_Interactive then
         case Get_Answer
           (Prompt => "Continue with installation?", Default_Answer => Other,
            Provided_Text =>
              "press ""enter"" with no input for interactive mode")
         is
            when No =>
               raise User_Aborted;
            when Other =>
               New_Line;
               Put_Line ("Switching to interactive mode...");
               New_Line;
               declare
                  use Options;
                  New_Version : constant String :=
                    Get_Answer
                      ("Which Version would you like to install?",
                       Provided_Text => "leave blank for latest version");
                  New_Tmp_Dir : constant String :=
                    Get_Answer
                      ("Where do you want to store temporary files?",
                       Default_Answer => To_String (Our_Settings.Tmp_Dir));
                  New_Cfg_Dir : constant String :=
                    Get_Answer
                      ("Where do you want the configuration directory?",
                       Default_Answer => To_String (Our_Settings.Cfg_Dir));
                  New_Bin_Dir : constant String :=
                    Get_Answer
                      ("Where do you want the binary file to go?",
                       Default_Answer => To_String (Our_Settings.Bin_Dir));
                  New_Update_Path : constant Answer :=
                    Get_Answer
                      ("Add the binary diretory to PATH if it isn't already?",
                       Default_Answer => Yes);
                  New_Options : constant Program_Options :=
                    (Version =>
                       (if New_Version'Length = 0 then Null_Unbounded_String
                        else To_Unbounded_String (New_Version)),
                     Tmp_Dir =>
                       (if New_Tmp_Dir'Length = 0 then Null_Unbounded_String
                        else To_Unbounded_String (New_Tmp_Dir)),
                     Cfg_Dir =>
                       (if New_Cfg_Dir'Length = 0 then Null_Unbounded_String
                        else To_Unbounded_String (New_Cfg_Dir)),
                     Bin_Dir =>
                       (if New_Bin_Dir'Length = 0 then Null_Unbounded_String
                        else To_Unbounded_String (New_Bin_Dir)),
                     No_Update_Path =>
                       (if New_Update_Path = No then True else False),
                     others => False);
                  New_Settings : constant Program_Settings :=
                    Init_Settings (New_Options);
               begin
                  Install (New_Settings);
                  return;
               end;
            when others =>
               null;
         end case;
      end if;
      --  Download / extract Alire
      declare
         function Download_URL return String is
   --  TODO: Have additional platforms, obviously this will NOT
   --  work on aarch64 linux atm.
   --  If alire provides other arches, then we can also just do:
   --  "bin-" & To_Lower(Plat.Arch'Image) &
   --  "-" & To_Lower(Plat.OS'Image) & ".zip";
   --  So macos would be "bin-aarch64-macos.zip"
   --  (obviously MacOS supports x86_86 but that's an exception)
   --  TODO: Also maybe download gnat and build from source for unknown archs?
   --  Also, some linux distros don't use glibc, so we may need to get a
   --  version of Alire that is not built against libc. Bootstrap?
            Alire_Base_API : constant String :=
              "https://api.github.com/repos/alire-project/alire/releases";
            --  Decide whether we're getting latest or e.g /tags/v1.2.3
            URL : constant String :=
              Alire_Base_API &
              (if Length (Our_Settings.Version) > 0 then
                 "/tags/v" & To_String (Our_Settings.Version)
               else "/latest");

            --  Result : AWS.Response.Data;
            --  Result := AWS.Client.Get(URL => URL);

            --  just use `curl -s URL` or wget :D
            Cmd  : constant String := "curl";
               --(if Available_Command (curl) then "curl"
               -- elsif Available_Command (wget) then "wget"
               -- else raise Missing_Dependency);
            Arg : constant String := "-q";
               --(if Available_Command (curl) then "-q"
               -- elsif Available_Command (wget) then "-qO-"
               -- else raise Missing_Dependency);
            Args : constant GNAT.OS_Lib.Argument_List (1 .. 2) :=
              (1 => new String'(Arg), 2 => new String'(URL));
            Status   : aliased Integer := 0;
            Response : constant String :=
              Get_Command_Output
                (Command => Cmd, Arguments => Args, Input => "",
                 Status  => Status'Access);
            Suffex : constant String :=
              (case OS is
                 when MacOS   => "bin-x86_64-macos.zip",
                 when Linux   => "bin-x86_64-linux.zip",
                 when FreeBSD => "", -- coming soon
                 when Windows => "bin-x86_64-windows.zip");
            --  the json parser stuff
            package Types is new JSON.Types (Long_Integer, Long_Float);
            package Parsers is new JSON.Parsers (Types);

            Parser : Parsers.Parser :=
              Parsers.Create
                ((if Response (Response'First) = '{' then Response
                  else raise Invalid_Download
                      with "Unable to download from the following URL: '" &
                      URL & "'... Expecting JSON but got: " & Response));
            Value : constant Types.JSON_Value := Parser.Parse;

            use Types;
         begin
            if Value.Kind = Object_Kind and then Value.Contains ("assets")
              and then Value ("assets").Kind = Array_Kind
            then
               for Element of Value ("assets") loop
                  declare
                     Download_URL : constant String :=
                       Element ("browser_download_url").Image;
                  begin
                     if Index (Download_URL, Suffex) > 0 then
                        return
                          (if
                             Download_URL (Download_URL'First) = '"'
                             and then Download_URL (Download_URL'Last) = '"'
                             and then Download_URL'First /= Download_URL'Last
                           then
                             Download_URL
                               (Download_URL'First + 1 ..
                                    Download_URL'Last - 1)
                           else Download_URL);
                     end if;
                  end;
               end loop;
            end if;
            raise Invalid_Version
              with "Unable to find alire download of version: " &
              To_String (Our_Settings.Version);
         end Download_URL;

         --  Creates a temporary directory in Tmp_Dir directory
         --  It will try 1000 times.
         --  Raises an exception if unable to create directory.
         function Unique_Dir return String is
         begin
            if not Ada.Directories.Exists (Tmp_Dir) then
               raise Invalid_File with Tmp_Dir & " does not exist!";
            end if;
            loop
               for I in 1 .. 1000 loop
                  declare
                     New_Directory : constant String :=
                        Ada.Directories.Full_Name
                           (Tmp_Dir & "/" & "tmp." & Random_String (16));
                  begin
                     if not Ada.Directories.Exists (New_Directory) then
                        Ada.Directories.Create_Path (New_Directory);
                        return New_Directory;
                     end if;
                  end;
               end loop;
               if
                  Our_Settings.Non_Interactive or else
                  Get_Answer ("Cannot create unique folder in " & Tmp_Dir &
                              "... Keep searching?", Default_Answer => Yes) =
                  No
               then
                  raise Invalid_File with
                     "Unable to find unique dir in " & Tmp_Dir;
               end if;
            end loop;
         end Unique_Dir;

         --  Fetch the download URL for Alire from github.
         URL : constant String := Download_URL;
         --  This contains the full .zip name based on the download URL.
         File_Name : constant String :=
           URL ((Index (URL, "/", Ada.Strings.Backward) + 1) .. URL'Last);
         --  This is the full path to save the file.
         Tmp_Path  : constant String := Unique_Dir;
         Save_Path : constant String := Tmp_Path & "/" & File_Name;
      begin
         --  Create the metadata directory.
         Log.Logit (Created_Metadata, Success);
         --  Metadata directory is current working directory.
         Ada.Directories.Set_Directory (Tmp_Path);
         --  Download the zip if it doesn't already exist.
         if not Ada.Directories.Exists (Save_Path) then
            IO.Say_Line ("Downloading " & URL & " to " & Save_Path);
            --  Will add this back in once we have AWS
            --  Download (URL, Save_Path);
            Download (URL);
         else
            IO.Say_Line
              ("file " & Save_Path & " already exists, skipping download.");
         end if;
         Log.Logit (Downloaded, Success, Save_Path);
         --  Create the config and directories if they don't exist.
         if not Ada.Directories.Exists (Cfg_Dir) then
            IO.Say_Line ("Creating Directory: " & Cfg_Dir);
            Ada.Directories.Create_Path (Cfg_Dir);
         else
            IO.Say_Line ("Directory " & Cfg_Dir & " detected.");
         end if;
         Log.Logit (Created_Cfg, Success);
         if not Ada.Directories.Exists (Bin_Dir) then
            IO.Say_Line ("Creating Directory: " & Bin_Dir);
            Ada.Directories.Create_Path (Bin_Dir);
         else
            IO.Say_Line ("Directory " & Bin_Dir & " detected.");
         end if;
         Log.Logit (Created_Bin, Success);
         --  Binary directory is current working directory.
         Ada.Directories.Set_Directory (Bin_Dir);
      --  Check if alr already exists.  If it does, confirm that they want to
         --   overwrite it.
         if Ada.Directories.Exists (Bin_Dir & "/" & Defaults.Alire)
           and then
             (if Our_Settings.Non_Interactive then Yes
              else Get_Answer
                  ("The following file already exists:" & Bin_Dir & "/" &
                   Defaults.Alire & " ... Replace it?",
                   Default_Answer => Yes)) =
             No
         then
            raise User_Aborted;
         end if;
         IO.Say_Line ("Extracting: " & Save_Path & " to " & Bin_Dir);
         Extract_Alire (Save_Path);
         Log.Logit (Extracted, Success, Alire_Binary);
      end;
      --  Verify alire is there and executable (we may have to set +x if not)
      declare
         type Checks is (Chmod, Macos_xattr);
         Tested : array (Checks'Range) of Boolean :=
           (Macos_xattr => (if OS = MacOS then False else True),
            others => False);

      begin
         Test_Alire :
         loop
            IO.Say_Line
              ("Testing Alire by running """ & Alire_Binary & " --version""");
            if not Test_Command (Alire_Binary) then
               IO.Say_Line
                 ("Unable to run binary... Attempting to troubleshoot.");
               if not Tested (Chmod) then
                  declare
                     Chmod_Success : Boolean;
                     Chmod_Args : constant GNAT.OS_Lib.Argument_List
                       (1 .. 2) :=
                       (1 => new String'("+x"),
                        2 => new String'(Alire_Binary));
                  begin
                     --  TODO: Handle if they don't have chmod
                     --    or if it's not stored at /bin/chmod
                     IO.Say_Line
                       ("Attempting ""/bin/chmod +x " & Alire_Binary & """");
                     GNAT.OS_Lib.Spawn
                       (Program_Name => "/bin/chmod", Args => Chmod_Args,
                        Success      => Chmod_Success);
                     Tested (Chmod) := True;
                  end;
               elsif not Tested (Macos_xattr) then
                  declare
                     xattr_Success : Boolean;
                     xattr_Args : constant GNAT.OS_Lib.Argument_List
                       (1 .. 3) :=
                       (1 => new String'("-d"),
                        2 => new String'("com.apple.quarantine"),
                        3 => new String'(Alire_Binary));
                  begin
                     IO.Say_Line
                       ("Attempting ""/usr/bin/xattr -d " &
                        "com.apple.quarantine " & Alire_Binary & """");
                     GNAT.OS_Lib.Spawn
                       (Program_Name => "/usr/bin/xattr", Args => xattr_Args,
                        Success      => xattr_Success);
                  end;
                  Tested (Macos_xattr) := True;
               else -- not chmod_tried
                  raise Invalid_File
                    with Alire_Binary &
                    " is not a valid executible by this system.";
               end if;
            else
               IO.Say_Line ("Sucessfully able to run binary.");
               exit Test_Alire;
            end if;
         end loop Test_Alire;
         Log.Logit (Alr_Tested, Success, Alire_Binary);
      end;
      --  At this point alr works, time to add it to path if requested.
      if not Our_Settings.No_Update_Path then
         declare
            procedure Add_Env_To_Config (Config : Shell_Config) is
               Full_Path : constant String :=
                 Home_Dir & "/" & To_String (Config.Config_File);
               Shell_File : File_Type;
               Env_Path   : constant String :=
                 Cfg_Dir & "/" & Get_Shell_Env (Config.Shell);
               Command : constant String :=
                 Get_Env_Command (Config.Shell, Env_Path);
            begin
               --  If the env for this shell does not exist
               if not Ada.Directories.Exists (Env_Path) then
                  declare
                     Env_File : File_Type;
                  begin
                     Create (Env_File, Out_File, Env_Path);
                     Write_Env_File (Config.Shell, Env_File, Bin_Dir);
                     Close (Env_File);
                     Log.Logit (Created_Env_File, Success, Env_Path);
                  end;
               end if;
            --  If the current profile exists, read through it to check for our
            --  command to add.
               if not Line_Exists (Full_Path, Command) then
                  --  Open or amend the file
                  if Ada.Directories.Exists (Full_Path) then
                     Open (Shell_File, Append_File, Full_Path);
                  else
                     Create (Shell_File, Out_File, Full_Path);
                  end if;
                  IO.Say_Line ("Adding '" & Command & "' to " & Full_Path);
                  Put_Line (Shell_File, Command);
                  Close (Shell_File);
               else
                  IO.Say_Line ("Env source already detected in " & Full_Path);
                  IO.Say_Line
                    ("If you believe this is an error, please report it.");
               end if;
               Log.Logit
                 (Added_Env_File, Success,
                  Full_Path & Logger.Item_Seperator & Command);
            end Add_Env_To_Config;

            Current_Path : constant String :=
              To_String (Our_Settings.Path_Env);
         begin
            for Shell of Our_Shells loop
               Add_Env_To_Config (Shell);
            end loop;
            if Index (":" & Bin_Dir & ":", ":" & Current_Path & ":") = 0 then
               IO.Say_Line
                 (Bin_Dir &
                  " not detected in Path.  You may need to reinitate " &
                  "your session.");
            else
               IO.Say_Line (Bin_Dir & " already detected in $PATH.");
            end if;
         end;
      end if;
      Log.Save (Cfg_Dir & Defaults.Log_File);
   exception
      when User_Aborted | OS_Not_Yet_Supported =>
         raise;
      when others =>
         IO.Must_Say ("Something went wrong... Aborting installation...");
         IO.Must_Say
           ("Attempting to save log in HOME directory:" & Home_Dir &
            Defaults.Log_File);
         Log.Save (Home_Dir & Defaults.Log_File);
         IO.Must_Say ("Attempting to roll back to state before installer ran");
         --  Uninstall what was done so far.
         Uninstall (Log, Our_Settings, Never_Prompt => True);
         IO.Must_Say ("If filing a bug report, please attach the log.");
         raise;
   end Install;

end Installer;
