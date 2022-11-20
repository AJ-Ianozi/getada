# GetAda: An unofficial Ada installer.

This is my attempt at an installer for Alire.  It currently supports all non-windows platforms that Alire has an official release for, which at present is:
* Linux, built against glibc
* MacOS

For Windows, please use the current windows installer exe located on [Alire's Releases Page](https://github.com/alire-project/alire/releases).

It is in very early beta, so please report any bugs you find!

## Requirements
Right now `curl` and `chmod` are required to run this.  I plan on utilizing AWS as a backend once the version available in Alire supports SSL, and that should hopefully remove the `curl` requirement, as well as open up the installer for Windows.

## How to get it?
Easiest way is to download the latest .zip, extract it, and run it.

If you want to build it, you can also clone this repository, `cd` to the directory, then build it with `alr update && alr build` in Alire.

### On MacOS
If you're running on MacOS, and you get a message about the program not being verified, then run the following command on the extracted `getada` binary before attempting to run it.  E.g.:
```
xattr -d com.apple.quarantine ./getada
```

Hopefully this can be solved in due time.

## How does it work?
Once downloaded, simply run the application as-is, and it will prompt you to install Alire.  For instructions on how to use it, run `getada --help` or `getada -h`.

It will download the latest version of Alire for your platform as a zip file to a metadata directory and then extract it to a binary directory.  By default the metadata directory is `~/.cache/getada`, the the alire config directory is `~/.alire`, and the `alr` binary itself goes in `~/.alire/bin`.

After extracting, it will then add the binary directory to your path, by creating a `env.sh` file (for sh/zsh/bash; other shells like `fish` are possible in the future) that adds the directory to $PATH if it doesn't already exist.  That file will be sourced in the shell's default env file (e.g. `.profile`).

## How to remove what it's done?
I am planning on giving `getada` the ability to *uninstall* but it presently cannot.  For that, I would have to store what it's doing in the metadata directory, and I haven't added that feature yet.  For now, you would have to manually remove `alr` from the binary directory as well as all of the files in the `alr config`.  You'll also need to remove the source in the shell's profiles.

### NOTE: Early beta with more on the way
This software is experimental software that is still in the early stages. The default directories and other methods are subject to change. For example, Alire uses `~/.cache/alire/` for most of its files, and GetAda may follow.  I also want to bring in the bash autocomplete (and create a zsh autocomplete) and include that in the env.sh.

There's plenty of TODOs in the code, specifically in the `installer.adb` file, and I'm sure this could be better optimized and reworked.

Once things look stable, I also want to have a shell script that can be ran in the command prompt that will automatically download GetAda and run it.
