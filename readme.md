# GetAda: An unofficial Ada installer.

This is my attempt at an installer for Alire.  It currently supports all non-windows platforms that Alire has an official release for, which at present is:
* Linux, built against glibc
* MacOS

For Windows, please use the current windows installer exe located on [Alire's Releases Page](https://github.com/alire-project/alire/releases).

## Requirements
Right now `chmod` and (`curl` or `wget`) are required to run this.

## How to get it?
On Mac and Linux with `curl` installed, the easiest way is to copy and paste the following command in your terminal:
```sh
curl --proto '=https' -sSf https://www.getada.dev/init.sh | sh
```

You can also download the latest zip release, extract it, and run it.

If you want to build it, you can also clone this repository, `cd` to the directory, then build it with `alr update && alr build` in Alire.

### On MacOS
If you're installing without he above curl command and you get a message about the program not being verified on MacOS, then run the following command on the extracted `getada` binary before attempting to run it.  E.g.:
```
xattr -d com.apple.quarantine ./getada
```

## How does it work?
Once downloaded, simply run the application as-is, and it will prompt you to install Alire.  For instructions on how to use it, run `getada --help` or `getada -h`.

It will download the latest version of Alire for your platform as a zip file to a temporary directory and then extract it to a binary directory.  By default the temporary directory is whatever `mktmp` normally puts out.  The config directory is `~/.getada`, and the `alr` and `getada` binaries go in `~/.getada/bin`.

After extracting, it will then add the binary directory to your path, by creating a `env.sh` file (for sh/zsh/bash; other shells like `fish` are possible in the future) that adds the directory to $PATH if it doesn't already exist.  That file will be sourced in the shell's default env file (e.g. `.profile`).

If you don't want to restart your shell, you'll have to `source` that file manually.  The installer tells you how to do this once successfully installed.

To create a new ada project, simply run:
```sh
alr init --bin new_project
```
This will create a new ada project in the folder "new_project".

To add, for example, json support to your project, run
```sh
alr with json
```

Check out all of the available alire crates on https://alire.ada.dev/crates.html

To build an ada project, run:
```sh
alr build
```

For more information, check out [alire.ada.dev](https://alire.ada.dev) and [Ada-lang.io's tutorial](https://https://ada-lang.io/docs/learn/tutorial/hello-world#starting-a-new-project).

## How to remove what it's done?
If you want to undo everything that GetAda did, simply run `getada --uninstall`.  If you want to do this manually, remove `alr` from the binary directory as well as all of the files in the config directory.  You'll also need to remove the source in the shell's profiles.

### Contributing
I also want to bring in the bash autocomplete (and create a zsh autocomplete) and include that in the env.sh, but that's later.  Someday I would like to remove the `curl`/`wget` dependency and Ada Web Server, but that involves moving off github for the releases.

There's plenty of TODOs in the code, specifically in the `installer.adb` file, and I'm sure this could be better optimized and reworked.

### License
Getada is copyright A.J. Ianozi and Getada contributors.

`getada-download.sh` and `www/getada.js` is copyright A.J. Ianozi, Diggory Blake, the Mozilla Corporation, and Rustup contributors, and was originally forked from [Rustup](https://github.com/rust-lang/rustup) as [getada-download](https://github.com/AJ-Ianozi/getada-download).

All code in this repository is licensed under the GNU GENERAL PUBLIC LICENSE 3.0, which an exception to changes made to `getada-download.sh` and `www/getada.js` before forking, which are licensed under MIT and APACHE.