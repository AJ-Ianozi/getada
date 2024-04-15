--    Copyright (C) 2024 A.J. Ianozi <aj@ianozi.com>
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
with Ada.Strings.Fixed;

package body Common is
   use Ada.Strings.Fixed;
   function Split
      (Item : String; Delimiter : String)
   return String_Vectors.Vector is
      Result : String_Vectors.Vector;
   begin
      if Delimiter'Length > 0 and then Item'Length > 0 then
         declare
            Next_Begin     : Natural := Item'First;
            Next_Delimiter : Natural := Index (Item, Delimiter);
         begin
            loop
               if Next_Delimiter > 0 then
                  if Next_Delimiter = 1 then
                     Result.Append ("");
                  else
                     Result.Append (Item (Next_Begin .. Next_Delimiter - 1));
                  end if;
                  Next_Begin     := Next_Delimiter + Delimiter'Length;
                  Next_Delimiter := Index (Item (Next_Begin .. Item'Last),
                                           Delimiter);
               else
                  Result.Append (Item (Next_Begin .. Item'Last));
                  exit;
               end if;
               exit when Next_Delimiter > Item'Last;
            end loop;
         end;
      end if;
      return Result;
   end Split;
end Common;