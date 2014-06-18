How to setup this XCompose configuration
========================================

Delegate a key to XCompose, a.k.a. the `Multi_key`.
I have these lines in my `~/.Xmodmap`:

    keycode 108 = Multi_key
    clear mod1
    add mod1 = Alt_L

Optionally you can have a look and modify the mappings in `xcompose-generator/XComposeGen/Unicode.hs`,
add new symbols or change shortcuts.

Then, generate the `.XCompose` file by running `./install.sh`.
This script automatically links the generated `XCompose` file to your `$HOME`.

Moreover if you want it to be run automatically you could have a look at [mr](http://joeyh.name/code/mr/)
and setup a fixups hook:

    [.XCompose.d]
    checkout = git clone git@github.com:np/xcompose.git .XCompose.d
    fixups = ./install.sh

