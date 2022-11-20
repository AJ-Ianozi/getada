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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Options is

   --  This will be set if we detect a version
   type Program_Options is record
      Version        : Unbounded_String := Null_Unbounded_String;
      Metadata_Dir   : Unbounded_String := Null_Unbounded_String;
      Alire_Cfg      : Unbounded_String := Null_Unbounded_String;
      Alire_Bin      : Unbounded_String := Null_Unbounded_String;
      Show_Help      : Boolean          := False;
      No_Update_Path : Boolean          := False;
   end record;

   --  Exceptions
   Invalid_Argument : exception;
   Unknown_Argument : exception;

   function Process_Arguments return Program_Options;
private
   function Check_Argument
     (Update_Flag : in out Boolean; Short_String : String;
      Long_String :        String; Current_Argument : String) return Boolean;
   function Check_Argument
     (Update_String      : in out Unbounded_String; Short_String : String;
      Long_String :    String; Current_Argument : String; Arg_Index : Natural;
      Skip_Next_Argument :    out Boolean) return Boolean;
end Options;
