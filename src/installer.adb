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

--  for downloader:
--  TODO: Add this when AWS can support https in alire
--    with AWS.Client;
--    with AWS.Response;
--    with AWS.Resources;
--    with Ada.Streams.Stream_IO;
with Local_Settings; use Local_Settings;
with GNAT.Expect;    use GNAT.Expect;
with GNAT.OS_Lib;
--  Required for reading the json versions and getting the download URL.
with JSON.Parsers;
with JSON.Types;

with Shells;  use Shells;
with Prompts; use Prompts;

with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Text_IO;       use Ada.Text_IO;

with Zip;
with UnZip;

package body Installer is

--  This will be used once alire's AWS supports https.
--  For the moment: just using curl.
--  procedure Download (URL : String; Destination_File : String) is
--      use AWS;
--      use Ada.Streams;
--      Result : constant Response.Data := Client.Get(URL => URL);
   procedure Download (URL : String) is
      Cmd  : constant String                             := "curl";
      Args : constant GNAT.OS_Lib.Argument_List (1 .. 4) :=
        (1 => new String'("-O"), 2 => new String'("-J"),
         3 => new String'("-L"), 4 => new String'(URL));
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
      else
         raise Invalid_File
           with "Archive does not contain bin/alr." &
           " If Alire has changed their zip structure, file a bug report.";
      end if;
   end Extract_Alire;

   procedure Install (Our_Settings : Program_Settings) is
      --  For easier newlines.
      NL : constant String := Defaults.NL;

      --  Posts a message, but only if messages aren't suppressed.
      procedure Say_Line (Item : String) is
      begin
         if not Our_Settings.Quiet then
            Put_Line (Item);
         end if;
      end Say_Line;

      Home_Dir : constant String := To_String (Our_Settings.Home_Dir);
      Tmp_Dir  : constant String := To_String (Our_Settings.Tmp_Dir);
      Cfg_Dir  : constant String := To_String (Our_Settings.Cfg_Dir);
      --  TODO: Put this in Program Files on Windows +
      --       and Home_Dir/Applications/bin on MacOS?
      --       do something like:
      --       Local_Apps: constant String := Local_Settings.App_Dir;
      Bin_Dir : constant String := To_String (Our_Settings.Bin_Dir);
      Version : constant String := To_String (Our_Settings.Version);

      Our_Shells : constant Shell_Array :=
        (if
           not Our_Settings.No_Update_Path
           and then Our_Settings.Current_Platform.OS /= Windows
         then Available_Shells (Our_Settings.Current_Platform)
         else (1 => (Null_Unbounded_String, null_shell)));

      Settings_Message : constant String :=
        (if Version /= "" then
           "I will attempt to fetch version """ & Version & """"
         else "No version has been specified. Will attempt to install the " &
           "latest version of Alire." & NL &
           "(To specify a version, pass --version=x.y.z)") &
        NL & "Temporary files will be stored in the following directory: " &
        NL & Tmp_Dir & NL & NL & "(This can be changed with the """ &
        Defaults.Tmp_Env & """ " &
        "environment variable or passing --tmp=/directory/here)" & NL & NL &
        "Any of alire's scripts or helper files will store in " &
        "the following location:" & NL & Cfg_Dir & NL & NL &
        "(This can be changed either by setting the """ & Defaults.Cfg_Env &
        """ environment variable or passing --cfg=/directory/here)" & NL & NL &
        "Alire's binary will be installed as """ & Defaults.Alire & """ in " &
        "the following location:" & NL & Bin_Dir & NL & NL &
        "(This can be changed either by setting the """ & Defaults.Bin_Env &
        """ " & "environment variable or passing --bin=/directory/here)" & NL;
   begin

      if Our_Settings.Current_Platform.OS = Windows then
         raise OS_Not_Yet_Supported
           with NL & "----------------------------------------" &
           "----------------------------------------" & NL &
           "NOTE: Windows installation is not ready yet!" & NL &
           " I recommend using alire's installer on https://alire.ada.dev/" &
           NL & "----------------------------------------" &
           "----------------------------------------";
      end if;

      --  TODO: Add unix type for linux/macos/freebsd/etc --
      case Our_Settings.Current_Platform.OS is
         when MacOS | Linux =>
            null;
         when others =>
            raise OS_Not_Yet_Supported
              with "The current OS is not yet supported. " &
              "Should never get here! ";
      end case;

      Say_Line (Settings_Message);

      if not Our_Settings.No_Update_Path then

         Say_Line
           ("This path will be added to your local PATH variable by " &
            "modifying the following files:");

         for Shell of Our_Shells loop
            Say_Line (Home_Dir & "/" & To_String (Shell.Config_File));
         end loop;

         Say_Line (NL & "(This can be changed by passing --no-path)" & NL);

      end if;
      --  Ask user if they want to install the propgram.
      if
        (if Our_Settings.Non_Interactive then Yes
         else Get_Answer
             ("Continue with installation?", Default_Answer => Yes)) =
        No
      then
         raise User_Aborted;
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

            --  just use `curl -s URL` :D
            Cmd  : constant String                             := "curl";
            Args : constant GNAT.OS_Lib.Argument_List (1 .. 2) :=
              (1 => new String'("-s"), 2 => new String'(URL));
            Status   : aliased Integer := 0;
            Response : constant String :=
              Get_Command_Output
                (Command => Cmd, Arguments => Args, Input => "",
                 Status  => Status'Access);
            Suffex : constant String :=
              (case Our_Settings.Current_Platform.OS is
                 when MacOS   => "bin-x86_64-macos.zip",
                 when Linux   => "bin-x86_64-linux.zip",
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
         --  Fetch the download URL for Alire from github.
         URL : constant String := Download_URL;
         --  This contains the full .zip name based on the download URL.
         File_Name : constant String :=
           URL ((Index (URL, "/", Ada.Strings.Backward) + 1) .. URL'Last);
         --  This is the full path to save the file.
         Save_Path : constant String := Tmp_Dir & "/" & File_Name;
      begin
         --  Create the metadata directory if it doesn't alerady exist.
         if not Ada.Directories.Exists (Tmp_Dir) then
            Say_Line ("Creating Directory: " & Tmp_Dir);
            Ada.Directories.Create_Path (Tmp_Dir);
         else
            Say_Line ("Directory " & Tmp_Dir & " detected.");
         end if;
         --  Metadata directory is current working directory.
         Ada.Directories.Set_Directory (Tmp_Dir);
         --  Download the zip if it doesn't already exist.
         if not Ada.Directories.Exists (Save_Path) then
            Say_Line ("Downloading " & URL & " to " & Save_Path);
            --  Will add this back in once we have AWS
            --  Download (URL, Save_Path);
            Download (URL);
         else
            Say_Line
              ("file " & Save_Path & " already exists, skipping download.");
         end if;
         --  Create the config and directories if they don't exist.
         if not Ada.Directories.Exists (Cfg_Dir) then
            Say_Line ("Creating Directory: " & Cfg_Dir);
            Ada.Directories.Create_Path (Cfg_Dir);
         else
            Say_Line ("Directory " & Cfg_Dir & " detected.");
         end if;
         if not Ada.Directories.Exists (Bin_Dir) then
            Say_Line ("Creating Directory: " & Bin_Dir);
            Ada.Directories.Create_Path (Bin_Dir);
         else
            Say_Line ("Directory " & Bin_Dir & " detected.");
         end if;
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
         Say_Line ("Extracting: " & Save_Path & " to " & Bin_Dir);
         Extract_Alire (Save_Path);
      end;
      --  Verify alire is there and executable (we may have to set +x if not)
      declare
         type Checks is (Chmod, Macos_xattr);
         Tested : array (Checks'Range) of Boolean :=
           (Macos_xattr =>
              (if Our_Settings.Current_Platform.OS = MacOS then False
               else True),
            others => False);
         Successfully_Executed : Boolean;
         Alire_Binary : constant String := Bin_Dir & "/" & Defaults.Alire;
         Alire_Args            : constant GNAT.OS_Lib.Argument_List (1 .. 1) :=
           (1 => new String'("--version"));
      begin
         Test_Alire :
         loop
            Say_Line
              ("Testing Alire by running """ & Alire_Binary & " --version""");
            GNAT.OS_Lib.Spawn
              (Program_Name => Alire_Binary, Args => Alire_Args,
               Success      => Successfully_Executed);
            if not Successfully_Executed then
               Say_Line
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
                     Say_Line
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
                     Say_Line
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
               Say_Line ("Sucessfully able to run binary.");
               exit Test_Alire;
            end if;
         end loop Test_Alire;
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
               No_Source_In_Env : Boolean := True;
            begin
               --  If the env for this shell does not exist
               if not Ada.Directories.Exists (Env_Path) then
                  declare
                     Env_File : File_Type;
                  begin
                     Create (Env_File, Out_File, Env_Path);
                     Write_Env_File (Config.Shell, Env_File, Bin_Dir);
                     Close (Env_File);
                  end;
               end if;
            --  If the current profile exists, read through it to check for our
            --  command to add.
               if Ada.Directories.Exists (Full_Path) then
                  Open (Shell_File, In_File, Full_Path);
                  --  Iterate through the file, looking for a PATH export.
                  Check_File :
                  while not End_Of_File (Shell_File) loop
                     declare
                        Next_Line : constant String := Get_Line (Shell_File);
                     begin
                        if Index (Next_Line, Command) > 0 then
                           --  Contains our env file
                           No_Source_In_Env := False;
                           exit Check_File;
                        end if;
                     end;
                  end loop Check_File;
                  Close (Shell_File);
               end if;
               if No_Source_In_Env then
                  --  Open or amend the file
                  if Ada.Directories.Exists (Full_Path) then
                     Open (Shell_File, Append_File, Full_Path);
                  else
                     Create (Shell_File, Out_File, Full_Path);
                  end if;
                  Say_Line ("Adding '" & Command & "' to " & Full_Path);
                  Put_Line (Shell_File, "# Added by getada");
                  Put_Line (Shell_File, Command);
                  Close (Shell_File);
               else
                  Say_Line ("Env source already detected in " & Full_Path);
                  Say_Line
                    ("If you believe this is an error, please report it.");
               end if;
            end Add_Env_To_Config;

            Current_Path : constant String :=
              To_String (Our_Settings.Path_Env);
         begin
            for Shell of Our_Shells loop
               Add_Env_To_Config (Shell);
            end loop;
            if Index (":" & Bin_Dir & ":", ":" & Current_Path & ":") = 0 then
               Say_Line
                 (Bin_Dir &
                  " not detected in Path.  You may need to reinitate " &
                  "your session.");
            else
               Say_Line (Bin_Dir & " already detected in $PATH.");
            end if;
         end;
      end if;
   exception
      when User_Aborted =>
         Say_Line ("Aborting installation...");
   end Install;

end Installer;
