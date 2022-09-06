# Setup

Mac & Linux:
- Install ghcup `curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh`
    - Install hls and stack

Windows: 
- Install ghcup `Set-ExecutionPolicy Bypass -Scope Process -Force;[System.Net.ServicePointManager]::SecurityProtocol = [System.Net.ServicePointManager]::SecurityProtocol -bor 3072;Invoke-Command -ScriptBlock ([ScriptBlock]::Create((Invoke-WebRequest https://www.haskell.org/ghcup/sh/bootstrap-haskell.ps1 -UseBasicParsing))) -ArgumentList $true`    

First Steps: https://www.haskell.org/ghcup/steps/

- Run `ghcup install `

- Install vscode
- Install the Haskell VsCode extension (ID: haskell.haskell)
- Choose to automatically discover tools via GHCUp
- Clone the repository
- Run `stack build` to download and compile all dependencies
- Open the project in VSCode and open `src/Lib.hs`
- Write something to make the program invalid. You should get syntax highlighting in a few seconds. If not check the output of the Haskell VSCode extension.

You can write
`-- >>> some valid Haskell expression`

to run any function defined in the file and view the result

For example
`-- >>> 1 + 1` should bring up the VSCode "evaluate" option and produce `2`.

Run `stack run` to build and run the application (`app/Main.hs`).

Run `stack test` to run tests (there is just one single test as a template right now)

# Helpful tools

https://hoogle.haskell.org/ or alternatively the Haskell Spotlight extension.

# Learning material

http://learnyouahaskell.com/chapters

https://serokell.io/blog/10-reasons-to-use-haskell
