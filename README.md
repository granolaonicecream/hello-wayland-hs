# hello-wayland-hs
This is a reimplementation of https://github.com/emersion/hello-wayland but with haskell FFI bindings to libwayland-client.

The purpose is to show:

- What FFI bindings to libwayland *could* look like, informing what a protocol scanner should generate
- How such bindings interact with Haskell's threading model

## Usage
```
cabal build
cabal run
```
