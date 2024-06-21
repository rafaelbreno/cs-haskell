## Preface

### Summary
1. [About](#about)
2. [Installing](#installing)
3. [References](#references)

### About
- **Reporting Error.** If you see an error, please open an [Issue](https://github.com/rafaelbreno/cs-haskell/issues/new), this is may be a infinite _Work in Progress_ project, so opening an issue may help us improve this material.
- **Background.** This repository is being written by a newbie in the FP World, so I may take some time to write some trustworthy material and errors may be made in the process.
- **Exercises and Solutions.** At the end of each chapter I'll add some exercises **and** solutions(my approach).

### Installing

#### Linux
- [GHCup Official Installer](https://www.haskell.org/ghcup/install/)
```shell
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
```

- [asdf](https://asdf-vm.com/)
```shell
$ asdf plugin-add haskell https://github.com/vic/asdf-haskell.git
$ asdf install haskell latest
$ asdf global haskell latest
```

#### Windows
- [GHCup Official Installer](https://www.haskell.org/ghcup/install/)
```powershell
Set-ExecutionPolicy Bypass -Scope Process -Force;[System.Net.ServicePointManager]::SecurityProtocol = [System.Net.ServicePointManager]::SecurityProtocol -bor 3072; try { Invoke-Command -ScriptBlock ([ScriptBlock]::Create((Invoke-WebRequest https://www.haskell.org/ghcup/sh/bootstrap-haskell.ps1 -UseBasicParsing))) -ArgumentList $true } catch { Write-Error $_ }
```
_Note: tbh I don't understand why powershell is so complicated._

### References

Here I'll be trying to organize all links to every material that I've read to write this repository, not only that, you should be able to find mentionings of references through the following chapters.

btw, I'm only reading free content, so, you can read the material that I'm basing this repo.

#### Ecosystem
- [GHCup](https://www.haskell.org/ghcup/)
    - GHCup is the main installer for the general purpose language Haskell.

#### Language
- [Haskell - WikiBooks](https://en.wikibooks.org/wiki/Haskell)
- [Haskell without Theory - Vacation Labs](https://www.vacationlabs.com/haskell/index.html)
    - From what I saw this guide is pretty pratical, with a few mentions about how things work under the hood. Pretty great!
- [Learn You a Haskell for Great Good!](https://learnyouahaskell.com/chapters)
- [Real World Haskell](https://book.realworldhaskell.org/read/index.html)
- [School of Haskell](https://www.schoolofhaskell.com)
- CIS 1940:
    - [Fall 2018](https://www.seas.upenn.edu/~cis1940/fall18/index.html)
    - [Fall 2016](https://www.seas.upenn.edu/~cis1940/fall16/index.html)
- [Typeclassopedia](https://wiki.haskell.org/Typeclassopedia)
- Packages
    - [Hackage](https://hackage.haskell.org)
- Search Engines
    - [Hoogle](https://hoogle.haskell.org)
- [Haskell 2019 Online Report](https://www.haskell.org/onlinereport/haskell2010/)
- Cheatsheets:
    - [Cheatsheet](http://cheatsheet.codeslower.com/CheatSheet.pdf)
    - [Haskell Operators](https://github.com/haskellcats/haskell-operators)
- []()
- []()
- []()
- []()

