# Contributing to Madlang

I emphatically welcome bug reports, issues you may encounter, documentation, and pull requests. I also welcome feature
requests, though whether we will incorporate them depends on contributors'
schedules.

## Getting started

If you'd like ideas for ways to contribute, check out `TODO.md`. Feel free to
open a PR or an issue if you want guidance on how to implement something. If
you're new to Haskell, I can provide help.

## Rules etc.
We follow the [rust standards of
conduct](https://www.rust-lang.org/en-US/conduct.html), with the addendum that
we are committed to providing a friendly, safe and welcoming environment
regardless of sex worker status or previous sex worker status.

In addition, please be aware that not everyone speaks English as a first
language.

## Builds & CI

All development takes places with `cabal` version 2.0.0.2 or later and `ghc`
version 8.2.1. On Ubuntu, you can use hvr's [ppa
repository](https://launchpad.net/~hvr/+archive/ubuntu/ghc) to get them.
You can use stack as well, so long as it passes CI. 

You may wish to install [just](https://github.com/casey/just) as well.
