# ush serves http

<p align="center">
    <img width="250" src="https://github.com/CrociDB/ush/blob/main/images/logo.png?raw=true">
</p>

**USH** is a simple static HTTP Server written in Haskell, mostly for study purposes. It's created after the [CodeCrafters](https://app.codecrafters.io/r/healthy-otter-219488) HTTP Server challenge.

It supports static files, gzip compression and directory indexing.

## Building

Assuming the Haskell suite (ghcup, cabal, stack) is installed:

```shell
stack install
```

## Usage

```shell
ush --directory directory/to/serve/
```

## License

This project is licensed under the [BSD-3](LICENSE.md).

The logo is made by [Delapouite](https://delapouite.com/) and is licensed under [CC BY 3.0](https://creativecommons.org/licenses/by/3.0/). Changes were made through [Game-Icons.net](https://game-icons.net/tags/bottle.html).
