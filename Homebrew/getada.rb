
#!env_rb ruby
#
# license: MIT

App     = "GetAdaNow"
app     = App.downcase
Install = "Install"
install = Install.downcase
Env     = "Environment"
env     = Env.downcase

CMD = [ :env, :inst ]
Cmd = [ :env => "Environment", :inst => "Install" ]

cmd = Cmd.each do | k, v |
   k.to_s.downcase
end

puts cmd

# environment = env

puts ""
puts "#{App} -- The Homebrew Ada installer for macOS                      "
puts "--                                                                          "
puts ""
puts "tl;dr;                                                                      "
puts "----                                                                        "
puts " > #{app} #{env} setup"
puts " > #{app} #{install} .all"
puts "or simply"
puts " > #{app} setup.all"
puts
puts "The not so short version"
puts "----"
puts " > #{app} #{env} check"
puts " > #{app} #{env} setup"
puts " > #{app} #{install} GNAT        -- #{Install} the GNAT compiler and tools     "
puts " > #{app} #{install} GPRBuild    -- #{Install} the GPRBuild GNAT Project tools "
puts " > #{app} #{install} GNATStudio  -- #{Install} the GNAT Studio IDE             "
puts " > #{app} #{install} Alire       -- #{Install} the Alire packaging tools   "
puts
puts "Usage                                                                       "
puts "----                                                                        "
puts "   #{app} #{install} [ GNAT | GPRBuild | GNATStudio | Alire ]               "
