
#!env_rb ruby
#
# license: MIT

App     = "GetAdaNow"
app     = App.downcase
Install = "Install"
install = Install.downcase
Env     = "Environment"
env     = Env.downcase

environment = env

puts ""
puts "#{App} -- The Homebrew Ada installer for macOS                      "
puts "--                                                                          "
puts ""
puts "tl;dr;                                                                      "
puts "----                                                                        "
puts " > #{app} #{environment} setup"
puts " > #{app} #{install} .all"
puts ""
puts "The not so short version"
puts "----"
puts " > #{app} #{install} GNAT        -- #{Install} the GNAT compiler and tools     "
puts " > #{app} #{install} GPRBuild    -- #{Install} the GPRBuild GNAT Project tools "
puts " > #{app} #{install} GNATStudio  -- #{Install} the GNAT Studio IDE             "
puts " > #{app} #{install} Alire       -- #{Install} the Alire packaging tools   "
puts " > #{app} #{environment} check"
puts " > #{app} #{environment} setup"
puts "                                                                            "
puts "Usage                                                                       "
puts "----                                                                        "
puts "   #{app} #{install} [ GNAT | GPRBuild | GNATStudio | Alire ]               "



