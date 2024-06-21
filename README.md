# Static :: Http -> Server

This is a basic static HTTP Server written in Haskell, mostly for study purposes. It's created after the [CodeCrafters](https://app.codecrafters.io/r/healthy-otter-219488) HTTP Server challenge.

It supports web pages, gzip compression and directory indexing.

## Building

```shell
$ stack install
```

## Usage

```shell
$ static-http-server --directory directory/to/serve/
```

