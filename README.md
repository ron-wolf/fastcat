# ats-cat

[![Build Status](https://travis-ci.org/vmchale/fastcat.svg?branch=master)](https://travis-ci.org/vmchale/fastcat)

`ac` is a modernized, faster version of `cat`. Forked from the `mycat.dats`
example shipped with `ats-anairiats`.

## The Pitch

Yes, it's really faster than `cat`. `cat` starts to be faster on files of
1 million lines or more, but at that point it's not really useful as
a command-line tool.

## The Anti-Pitch

Slightly fewer features and worse support. Also, binaries are currently only
available for x86\_64 Linux and OS X.

## Building

`ats-cat` uses [shake](http://shakebuild.com/) as its build system. I recommend
installing [nix](curl https://nixos.org/nix/install | sh), in which case you can
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
