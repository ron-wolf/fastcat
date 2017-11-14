# ats-cat

[![Build Status](https://travis-ci.org/vmchale/fastcat.svg?branch=master)](https://travis-ci.org/vmchale/fastcat)

`ac` is a modernized, faster version of `cat`. Forked from the `mycat.dats`
example shipped with `ats-anairiats`.

## The Pitch

Yes, it's really faster than `cat`. `cat` is faster on files of
1 million lines or more, but at that point it's not really useful as
a command-line tool. `ac` has slightly nicer help, and it can optionally
strip ANSI escape codes.

### Benchmarks

| Tool | Language/Compiler | Target | Time |
| ---- | -------- | ------ | ---- |
| ac   | ATS | `shake.hs` | 781.7 μs |
| cat  | C (gcc) | `shake.hs` | 954.9 μs |
| hcat | Haskell (ghc) | `shake.hs` | 1.468 ms |

## The Anti-Pitch

Some missing features and less support. Also, binaries are currently only
available for x86\_64 Linux.

## Building

`ats-cat` uses [shake](http://shakebuild.com/) as its build system. I recommend
installing [nix](https://nixos.org/nix/), in which case you can
install `stack` (for the build system) and `atscc` (the ATS compiler) with the
following:

```bash
 $ nix-env -i stack
 $ nix-env -i atscc
```

At this point, you can build with:

```bash
 $ ./shake.hs
```

To install:

```bash
 $ ./shake.hs install
```
