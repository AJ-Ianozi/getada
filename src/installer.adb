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

pragma Ada_2022;
--  for downloader:
--  TODO: Add this when AWS can support https in alire
--    with AWS.Client;
--    with AWS.Response;
--    with AWS.Resources;
--    with Ada.Streams.Stream_IO;
with GNAT.Expect;    use GNAT.Expect;
with GNAT.OS_Lib;
--  Required for reading the json versions and getting the download URL.
with GNATCOLL.JSON;

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

package body Installer is

   procedure Extract_Alire (File : String) is
      use Zip, UnZip;
      Zip_File : Zip_info;
   begin
      Load (Zip_File, File);
      --  If alire decides to start packing alr somewhere besides bin/alr
      --  then this will break.
      --  Also this won't work on Windows :)
      if Exists (Zip_File, "bin/alr") then
         Extract (from => File, what => "bin/alr",
                  rename => Defaults.Alire_Command);
      elsif Exists (Zip_File, "bin/alr.exe") then
         Extract
           (from   => File, what => "bin/alr",
            rename => Defaults.Alire_Command & ".exe");
      else
         raise Defaults.Invalid_File
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
        Bin_Dir & "/" & Defaults.Alire_Command &
        (if Platform.OS = Windows then ".exe" else "");

      --  The final resting place of the getada binary
      Getada_Binary : constant String :=
         Bin_Dir & "/" & Defaults.Getada_Command &
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
           IO.Say (NL &
            "(To specify a version, pass --alire-version=x.y.z)")) &
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
            NL & "Alire's binary will be installed as """ &
            Defaults.Alire_Command &
            """ in " & "the following location:" & NL & Bin_Dir & NL &
        IO.Say
          (NL & "(This can be changed either by setting the """ &
           Defaults.Bin_Env & """ " &
           "environment variable or passing --bin=/directory/here)" & NL);

      --  Our available commands
      Available_Command : constant Command_Supported := Test_Commands;

   begin

      if not Available_Command (curl) and then Available_Command (wget) then
         raise Defaults.Missing_Dependency
           with NL &
           "----------------------------------------" &
           "----------------------------------------" & NL &
           "You must have curl or wget to use GetAda." & NL &
           " Please at least install curl and then re-run." & NL &
           "----------------------------------------" &
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
               raise Defaults.User_Aborted;
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
            use GNATCOLL.JSON;
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

            --  just use `curl -s URL` or wget :D
            Cmd  : constant String := --  "curl";
               (if Available_Command (curl) then "curl"
                elsif Available_Command (wget) then "wget"
                else raise Defaults.Missing_Dependency);
            Arg : constant String := --  "-q";
               (if Available_Command (curl) then "-q"
                elsif Available_Command (wget) then "-qO-"
                else raise Defaults.Missing_Dependency);
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
                 when FreeBSD => "", -- Need self hosted runners to build this
                 when Windows => "bin-x86_64-windows.zip");
            --  the json parser stuff
            Result : constant JSON_Value :=
               Read ((if Response (Response'First) = '{' then Response
                      else raise Defaults.Invalid_Download
                      with "Unable to download from the following URL: '" &
                      URL & "'... Expecting JSON but got: " & Response));
         begin
            if Result.Kind = JSON_Object_Type and then
               Result.Has_Field ("assets") and then
               Result.Get ("assets").Kind = JSON_Array_Type
            then
               declare
                  Value : constant JSON_Array := Result.Get ("assets");
               begin
                  for Element of Value
                     when Element.Has_Field ("browser_download_url")
                  loop
                     declare
                        Download_URL : constant String :=
                        Element.Get ("browser_download_url");
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
               end;
            end if;
            raise Defaults.Invalid_Version
              with "Unable to find alire download of version: " &
              To_String (Our_Settings.Version);
         end Download_URL;

         procedure Download (URL : String) is
            Cmd  : constant String := --  "curl";
                     (if Available_Command (curl) then "curl"
                      elsif Available_Command (wget) then "wget"
                      else raise Defaults.Missing_Dependency);
            Arg : constant String := --  "-OJL";
                     (if Available_Command (curl) then "-OJL"
                      elsif Available_Command (wget) then "-q"
                      else raise Defaults.Missing_Dependency);
            Args : constant GNAT.OS_Lib.Argument_List (1 .. 2) :=
               (1 => new String'(Arg),
                2 => new String'(URL));
            Status   : aliased Integer := 0;
            Response : constant String :=
            Get_Command_Output
               (Command => Cmd, Arguments => Args, Input => "",
               Status  => Status'Access, Err_To_Out => True);
         begin
            if Status /= 0 then
               raise Defaults.Invalid_Download with
                  "The following occurred when trying to download " & URL &
                  NL & Response;
            end if;
         end Download;

         --  Fetch the download URL for Alire from github.
         URL : constant String := Download_URL;
         --  This contains the full .zip name based on the download URL.
         File_Name : constant String :=
           URL ((Index (URL, "/", Ada.Strings.Backward) + 1) .. URL'Last);
         --  This is the full path to save the file.
         Tmp_Path  : constant String :=
            Unique_Dir (Tmp_Dir, Our_Settings.Non_Interactive);
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
         if Ada.Directories.Exists (Bin_Dir & "/" & Defaults.Alire_Command)
           and then
             (if Our_Settings.Non_Interactive then Yes
              else Get_Answer
                  ("The following file already exists:" & Bin_Dir & "/" &
                   Defaults.Alire_Command & " ... Replace it?",
                   Default_Answer => Yes)) =
             No
         then
            raise Defaults.User_Aborted;
         end if;
         IO.Say_Line ("Extracting: " & Save_Path & " to " & Bin_Dir);
         Extract_Alire (Save_Path);
         Log.Logit (Extracted, Success, Alire_Binary);
      end;
      --  If getada isn't in our binary path, copy that over too.
      if Our_Settings.Exec_Path /= Getada_Binary then
         declare
            Cur_Getada : constant String := To_String (Our_Settings.Exec_Path);
         begin
            Put_Line ("Copying " & Cur_Getada & " to " & Getada_Binary);
            Ada.Directories.Copy_File (Cur_Getada, Getada_Binary);
            Log.Logit (Copied_Getada, Success, Getada_Binary);
         end;
      end if;
      --  Verify alr and getada are there and executable
      --  (we may have to set +x if not)
      if not Test_Binary (Alire_Binary, IO) then
         raise Defaults.Invalid_File
               with Getada_Binary &
               " is not a valid executible by this system.";
      end if;
      if not Test_Binary (Getada_Binary, IO) then
         raise Defaults.Invalid_File
               with Getada_Binary &
               " is not a valid executible by this system.";
      end if;
      Log.Logit (Alr_Tested, Success, Alire_Binary);
      --  At this point alr works, time to add it to path if requested.
      if not Our_Settings.No_Update_Path then
         declare
            Already_Created : Boolean := False;
            procedure Add_Env_To_Config (Config : Shell_Config) is
               Full_Path : constant String :=
                 Home_Dir & "/" & To_String (Config.Config_File);
               Shell_File : File_Type;
               Env_Path   : constant String :=
                 Cfg_Dir & "/" & Get_Shell_Env (Config.Shell);
               Command : constant String :=
                 Get_Env_Command (Config.Shell, Env_Path);
               Env_File : File_Type;
            begin
               --  If the env for this shell does not exist
               if not Already_Created then
                  if Ada.Directories.Exists (Env_Path) then
                     Open (Env_File, Out_File, Env_Path);
                  else
                     Create (Env_File, Out_File, Env_Path);
                  end if;
                  Write_Env_File (Config.Shell, Env_File, Bin_Dir);
                  Close (Env_File);
                  Already_Created := True;
                  Log.Logit (Created_Env_File, Success, Env_Path);
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
      IO.Say_Line ("");
      IO.Say_Line ("Alire is now installed.  It can be called via ""alr""");
      IO.Say_Line ("");
      IO.Say_Line ("To uninstall alire, simply run:");
      IO.Say_Line ("""getada --uninstall""");
      IO.Say_Line ("");
      IO.Say_Line ("To create a new ada project, simply run:");
      IO.Say_Line ("""alr init --bin new_project""");
      IO.Say_Line ("to have a new ada project in the folder ""new_project""");
      IO.Say_Line ("To build an ada project, run ""alr build""");
      IO.Say_Line ("Have a tutorial to start on your first project:");
      IO.Say_Line ("https://ada-lang.io/docs/learn/tutorial/hello-world");
      IO.Say_Line ("");
      if Our_Settings.No_Update_Path then
         IO.Say_Line
            (Bin_Dir & " was not added to PATH and you may want to do that.");
      else
         IO.Say_Line ("You may need to restart your shell.");
         IO.Say_Line ("Doing so should add """ & Bin_Dir & """ to $PATH.");
         IO.Say_Line ("To configure your current shell without restarting, " &
                      "please run the following");
         IO.Say_Line ("command in your terminal (note the leading DOT):");
         IO.Say_Line (". """ & Cfg_Dir & "/" & Get_Shell_Env (sh) & """");
      end if;
      IO.Say_Line ("");
      Log.Save (Cfg_Dir & Defaults.Log_File);
   exception
      when Defaults.User_Aborted | Defaults.Platform_Not_Yet_Supported =>
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
