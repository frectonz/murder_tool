# murder_tool

Kill processes with a TUI

## How to run it?

```bash
nix shell github:frectonz/murder_tool
murder_tool
```

## Demo

[![asciicast](https://asciinema.org/a/663075.svg)](https://asciinema.org/a/663075)

## Technologies

Murder tool uses [feather](https://github.com/charlesetc/feather) to run `ps` and `kill`. It also uses [angstrom](https://github.com/inhabitedtype/angstrom) to parse the `ps` output. The TUI is powered by [lwd + nottui](https://github.com/let-def/lwd).

