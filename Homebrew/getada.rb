
#!env_rb ruby
#
# license: MIT

display_app     = "GetAdaNow"
app             = display_app.downcase
display_install = "Install"
install         = display_install.downcase

puts ""
puts "#{display_app} -- The Homebrew Ada installer for macOS                      "
puts "--                                                                          "
puts "tl;dr;                                                                      "
puts "----                                                                        "
puts " > :app :env setup"
puts " > :app :install .all"
puts "Usage 1"
puts "----"
puts " > #{app} #{install} GNAT        -- #{display_install} the GNAT compiler and tools     "
puts " > #{app} #{install} GPRBuild    -- #{display_install} the GPRBuild GNAT Project tools "
puts " > #{app} #{install} GNATStudio  -- #{display_install} the GNAT Studio IDE             "
puts " > #{app} #{install} Alire       -- #{display_install} the Alire packaging tools   "
puts " > #app #environment check"
puts " > #app #environment setup"
puts "                                                                            "
puts "Usage                                                                       "
puts "----                                                                        "
puts "   #{app} #{install} [ GNAT | GPRBuild | GNATStudio | Alire ]               "



