# Madlang DSL for generating random text

[![Build Status](https://travis-ci.org/vmchale/madlang.svg?branch=master)](https://travis-ci.org/vmchale/madlang)

This is the Madlang DSL for generating text. You specify a template, and Madlang
will create randomized text from the template.

Madlang is an interpreted language, written in Haskell. The primary way to use
Madlang is on the command line using the interpreter, but there is also a partially completed library
and EDSL.

Madlang is intended to explore computational creativity and provide an easy
way to get started with generative literature.

## Installation

### Stack

Download `stack` with

```
curl -sSL http://haskellstack.org | sh
```

Then run `stack install madlang --resolver nightly`. This is the recommended way
to install `madlang`, but it may take awhile.

### Nix

If you're on linux or mac, you can get binaries via nix.

Download nix with

```
curl https://nixos.org/nix/install | sh
```

From there, `nix-env -i madlang` will install the executable.

## Tutorial

The smallest program possible in Madlang is simply a return declaration, viz.

```madlang
:return
    1.0 "heads"
    1.0 "tails"
```

The `:return` tells us this that this will be the final value when run, while
the numbers in front of the strings denote relative weights. Save this as
`gambling.mad`, and run

```bash
 $ madlang run gambling.mad
 heads
```

Now let's try something a little more complicated:

```madlang
:define person
    1.0 "me"
    1.0 "you"

:return
    1.0 "The only one of us walking out of this room alive is going to be " person "."
```

A bit more sinister, perhaps. The `:define` statement there declares a new
*identifier*, which we can later reference. Save this as `fate.mad` and run:

```bash
 $ madlang run fate.mad
 The only one of us walking out of this room alive is going to be you.
```

We can also refer to another identifier within a `:define` block.

```madlang
:define coin
    1.0 "heads"
    1.0 "tails"

:define realisticCoin
    1.0 coin
    0.03 "on its side"

:return realisticCoin
```

In addition to identifiers, we can also define *categories*. Categories are just
groups of identifiers. We can define one like so:

```madlang
:define color
    1.0 "yellow"
    1.0 "blue"

:define texture
    1.0 "soft"
    1.0 "scratchy"
    1.0 "dimpled"

:category adjective
    color
    texture

:return
    1.0 adjective
```

Then, when we can `adjective`, it will pick one of "yellow", "blue",…
"dimpled" with equal probability.

Finally, one of the most powerful features of `madlang` is the ability to
include libraries in a file. Open the following and save it as `gambling.mad`:

```madlang
:define coin
    1.0 "heads"
    1.0 "tails"

:return
    1.0 ""
```

Then, open the following and save it in the same directory as
`realistic-gambling.mad`:

```madlang
:include gambling.mad

:define realisticGambling
    1.0 coin
    0.03 "on its side"

:return
    1.0 realisticGambling
```

Then run it with:

```bash
 $ madlang run realistic-gambling.mad
```

`madlang` comes with several libraries prepackaged. You can install
them for the current user with:

```bash
 $ madlang install
```

Try this out:

```
:include colors.mad

:define weirdDog
    1.0 colors-color "dog"

:return
    1.0 "On my walk today I saw a " weirdDog "."
```

### Examples

There is a dog complimenter available to test out at [my
site](http://blog.vmchale.com/madlang).

## Documentation

You can view manpages for `madlang` on Linux and Mac by typing:

```bash
 $ man madlang
```

## Tooling

### Vim

There is a vim plugin available [here](https://github.com/vmchale/madlang-vim).

### Project Templates

There is a project template bundled with
[pi](https://github.com/vmchale/project-init), which you can install with

```bash
 $ curl -LSfs https://japaric.github.io/trust/install.sh | sh -s -- --git vmchale/project-init
```

and invoke with

```
 $ pi new madlang story
```
