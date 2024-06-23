# ush serves http

<p align="center">
    <img width="220" src="https://github.com/CrociDB/ush/blob/main/images/logo.png?raw=true">
</p>

**USH** is a simple static HTTP Server written in Haskell, mostly for study purposes. It's created after the [CodeCrafters](https://app.codecrafters.io/r/healthy-otter-219488) HTTP Server challenge.

It supports static files, gzip compression and directory indexing.

## Roadmap

 - [x] Text/Binary file serving
 - [x] gzip compression
 - [] Directory indexing
 - [] HTTP caching

## Installing from Source

Assuming the Haskell suite (ghcup, cabal, stack) is installed:

```shell
stack install
```

## Usage

To serve the current directory:

```shell
ush
```

The directory and the port can be configured with `-d` and `-p` respectively.

## License

This project is licensed under the [BSD-3](LICENSE.md).

The logo is made by [Delapouite](https://delapouite.com/) and is licensed under [CC BY 3.0](https://creativecommons.org/licenses/by/3.0/). Changes were made through [Game-Icons.net](https://game-icons.net/tags/bottle.html).
