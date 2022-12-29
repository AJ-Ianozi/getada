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

with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Ada.Command_Line;       use Ada.Command_Line;
with Ada.Strings.Fixed;      use Ada.Strings.Fixed;

package body Options is

   --  To quickly parse arguments: setting strings
   function Check_Argument
     (Update_String      : in out Unbounded_String; Short_String : String;
      Long_String :    String; Current_Argument : String; Arg_Index : Natural;
      Skip_Next_Argument :    out Boolean) return Boolean
   is
   begin
      if Current_Argument = Short_String then
         if Update_String /= Null_Unbounded_String then
            raise Invalid_Argument
              with Current_Argument & " already set, cannot be set twice";
         elsif Arg_Index + 1 > Argument_Count then
            raise Invalid_Argument
              with "Argument not provided for " & Current_Argument;
         end if;
         Update_String      := To_Unbounded_String (Argument (Arg_Index + 1));
         Skip_Next_Argument := True;
         return True;
      elsif Current_Argument'Length > Long_String'Length
        and then Index (Current_Argument, Long_String) = 1
      then
         if Update_String /= Null_Unbounded_String then
            raise Invalid_Argument
              with Current_Argument & " already set, cannot be set twice";
         end if;
         Update_String :=
           To_Unbounded_String
             (Current_Argument
                (Long_String'Length + 1 .. Current_Argument'Last));
         Skip_Next_Argument := False;
         return True;
      else
         Skip_Next_Argument := False;
         return False;
      end if;
   end Check_Argument;

   --  To quickly parse arguments: setting Booleans
   function Check_Argument
     (Update_Flag : in out Boolean; Short_String : String;
      Long_String :        String; Current_Argument : String) return Boolean
   is
   begin
      if Current_Argument = Short_String or else Current_Argument = Long_String
      then
         if Update_Flag then
            raise Invalid_Argument
              with Current_Argument & " already set, cannot be set twice";
         end if;
         Update_Flag := True;
         return True;
      else
         return False;
      end if;
   end Check_Argument;

   function Process_Arguments return Program_Options is

      --  The settings object we'll return.
      Options : Program_Options;

      --  This is used to skip arguments
      Skip : Boolean := False;

   begin
      for I in 1 .. Argument_Count loop
         if not Skip then
            declare
               Arg : constant String := Argument (I);
            begin
               if Check_Argument (Options.Show_Help, "-h", "--help", Arg)
                 or else Check_Argument
                   (Options.No_Update_Path, "-p", "--no-path", Arg)
                 or else Check_Argument
                   (Options.Non_Interactive, "-n", "--non-interactive", Arg)
                 or else Check_Argument (Options.Quiet, "-q", "--quiet", Arg)
                 or else Check_Argument
                   (Options.Uninstall, "-u", "--uninstall", Arg)
                 or else Check_Argument
                   (Options.Version, "-v", "--version=", Arg, I, Skip)
                 or else Check_Argument
                   (Options.Tmp_Dir, "-t", "--tmp=", Arg, I, Skip)
                 or else Check_Argument
                   (Options.Bin_Dir, "-b", "--bin=", Arg, I, Skip)
                 or else Check_Argument
                   (Options.Cfg_Dir, "-c", "--cfg=", Arg, I, Skip)
                 or else raise Unknown_Argument
                   with "Unknown Argument: " & Arg & CR & LF &
                   "Run app with arguments -h or --help to show help."
               then
                  null;
               end if;
            end;
         else -- not Skip_This_Argument
            Skip := False;
         end if;
      end loop;

      return Options;
   end Process_Arguments;
end Options;
