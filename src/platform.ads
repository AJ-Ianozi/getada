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

--  Local settings specific to this platform.
package Platform is
   --  Potential Operating Systems
   type OSs is (Linux, Windows, MacOS, FreeBSD);
   --  Archs: Can add to this as more are supported.
   type Archs is
     (x86,     --  i386/i686
      x86_64,  --  64-bit x86-64
      aarch64, --  64-bit arm, e.g. Apple and rasperry pi
      armv7l); --  32-bit arm

   function OS return OSs;
   function Arch return Archs;
end Platform;
