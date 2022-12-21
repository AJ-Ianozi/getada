puts "GetAda -- The Homebrew Ada installer for macOS                           "
puts "--                                                                       "
puts "tl;dr;                                                                   "
puts "----                                                                     "
puts " > getada install GNAT        -- Install the GNAT compiler and tools     "
puts " > getada install GPRBuild    -- Install the GPRBuild GNAT Project tools "
puts " > getada install GNATStudio  -- Install the GNAT Studio IDE             "
puts " > getada install Alire       -- Install the `alr` Ada packaging tools   "
puts "                                                                         "
puts "Usage                                                                    "
puts "----                                                                     "
puts "   getada install [ GNAT | GPRBuild | GNATStudio | Alire ]               "

name = gets.chop
puts "The name #{name} was entered"


