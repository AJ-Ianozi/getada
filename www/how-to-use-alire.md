-- All lines which starts with double minus sign are comments and ignored by program. Unless they have colon sign. Then they are tags definition.
-- Ada Web Server template which will be used as HTML template for this file. Required for each file
-- layout: default
-- You may add as many tags as you want, and they can be in any place in file, not only at beginning. Tags can be 4 types: strings, boolean, numeric or composite.
-- First 3 types of tags are in Name: Value scheme. For strings, it can be any alphanumeric value without new line sign. For boolean it must be "true" or "false", for numeric any number. Program will detect self which type of tag is and properly set it. It always falls back to string value.
-- Composite tags first must be initialized with Name: [] then just add as many as you want values to it by Name: Value scheme.
-- For more information about tags please check program documentation.
-- If you have enabled creation of sitemap in the project config file, you can set some sitemap parameters too. They are defined in this same way like tags, with ParameterName: Value.
-- priority - The priority of this URL relative to other URLs on your site, value between 0.0 and 1.0.
-- changefreq - How frequently the page is likely to change, value can be always, hourly, daily, weekly, monthly, yearly or never.
-- For more information how this options works, please look at the program documentation.
-- Additionally, you can exclude this file from adding to sitemap by setting option insitemap: false.
-- If you have enabled creating Atom feed for the site, you must specify "title" tag for this page. If you want to use this file as a main source of Atom feed, then you must add "title" tag for each section which will be used as source for Atom feed entry. If you want to set author name for Atom feed, you must add "author" tag or setting Author from configuration file will be used. When you want to set author email for Atom feed, you must add "authoremail" tag. If you want to add short entry summary, you must add tag "summary". Do that tag will be for whole page or for each entry depends on your Atom feed configuration.
-- You can also specify canonical link for the page. If you don't set it here, the program will generate it automatically. To set the default canonical link for the page set tag "canonicallink". It must be a full URL (with https://).
-- By setting "author" tag for the page, you can overwrite the configuration setting for meta tag author for the page.
-- title: How to use Alire
-- You can without problem delete all this comments from this file.

## How to use alire?
Check out [GetAda.dev](https://getada.dev) on how to get alire.

### Create a new ada project
```sh
alr init --bin new_project
```
This will create a new ada project in the folder "new_project".

### Add a dependency
```sh
# for example, json support
alr with json
```
Check out [all of the available alire crates](https://alire.ada.dev/crates.html).

### Edit a project
```sh
alr edit
```
Don't use [GNAT Studio](https://github.com/AdaCore/gnatstudio)? Check out the [Ada Language Server](https://github.com/AdaCore/ada_language_server) and open the project folder with emacs, vim, vscode, or your editor of choice.

#### Set alr edit to your editor of choice
```sh
# For example, vscode
alr settings --global --set editor.cmd "code ."
```

### Select your toolchain
```sh
alr toolchain --select
```
The default is gnat-native, which is the compiler for whatever system you're running, but you can even cross-compile.

### Compile your project
```sh
alr build
```

Get started with [Ada-lang.io's tutorial](https://https://ada-lang.io/docs/learn/tutorial/hello-world#starting-a-new-project).