# GetAda: An unofficial Ada installer.

This is my attempt at an installer for Alire.  It currently supports all non-windows platforms that Alire has an official release for, which at present is:
* Linux, built against glibc
* MacOS

For Windows, please use the current windows installer exe located on [Alire's Releases Page](https://github.com/alire-project/alire/releases).

It is in very early beta, so please report any bugs you find!

## Requirements
Right now `chmod` and `curl` are required to run this.  On Windows I recommend using the installer on alire.ada.dev.

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

It will download the latest version of Alire for your platform as a zip file to a metadata directory and then extract it to a binary directory.  By default the metadata directory is `~/.cache/getada`, the config directory is `~/.getada`, and the `alr` binary goes in `~/.getada/bin`.

After extracting, it will then add the binary directory to your path, by creating a `env.sh` file (for sh/zsh/bash; other shells like `fish` are possible in the future) that adds the directory to $PATH if it doesn't already exist.  That file will be sourced in the shell's default env file (e.g. `.profile`).

## How to remove what it's done?
If you want to undo everything that GetAda did, simply run `getada --uninstall`.  If you want to do this manually, `alr` from the binary directory as well as all of the files in the `alr config`.  You'll also need to remove the source in the shell's profiles.

### NOTE: Early beta with more on the way
This software is experimental software that is still in the early stages. The default directories and other methods are subject to change. For example, Alire uses `~/.config/alire/` for most of its files, and GetAda may follow.  I also want to bring in the bash autocomplete (and create a zsh autocomplete) and include that in the env.sh.

There's plenty of TODOs in the code, specifically in the `installer.adb` file, and I'm sure this could be better optimized and reworked.

However, things do look stable, so I'm working to have a shell script that can be ran in the command prompt that will automatically download GetAda and run it.
