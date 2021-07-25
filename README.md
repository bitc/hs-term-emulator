# hs-term-emulator

This is a Terminal Emulator (like xterm) implemented entirely in Haskell.

## Screenshots

![screenshot_shell_01](screenshots/screenshot_shell_01.png)

![screenshot_htop_01](screenshots/screenshot_htop_01.png)

![screenshot_vim_01](screenshots/screenshot_vim_01.png)

## Misc Development Tricks

Here is a good ghci trick:

    :def! R \_ -> Prelude.return (":!clear\n:r\n:main")
    :R

Running `cabal-fmt`:

    $ mkdir -p ./.cabal/bin
    $ cabal v2-install cabal-fmt --installdir=./.cabal/bin --overwrite-policy=always
    $ ./.cabal/bin/cabal-fmt --version
