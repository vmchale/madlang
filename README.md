# Madlang DSL for generating random text
[![Build Status](https://travis-ci.org/vmchale/madlibs.svg?branch=master)](https://travis-ci.org/vmchale/madlibs)

This is the Madlang DSL for generating random text. There is also a vim plugin, available [here](https://github.com/vmchale/madlang-vim).  

It enables you to generate random templated text with very little effort or expertise. 

It can be used for twitter bots and provides human-readable syntax for Markov
chains often used in natural language processing. 

## Exmaples

An exmaple is worth a thousand words, so suppose you wanted to generate a mediocre fortune telling bot. You could write the following code:

```
:define person
    0.7 "A close friend will "
    0.3 "You will "
:define goodfortune
    0.2 person "make rain on the planet Mars"
    0.8 "nice things will happen today :)"
:define fortune
    0.5 "drink a boatload of milk"
    0.5 "get angry for no reason"
:return
    0.8 person fortune
    0.2 goodfortune
```

There are two "statements" in madlang, `:define` and `:return`. `:return` is the main string we'll be spitting back, so there can be only one per file. `:define` on the other hand can be used to make multiple templates. These templates are combinations of strings (enclosed in quotes) and names of other templates.

Of course, you can't have a circular reference with names - if `goodfortune` depends on `fortune` while `fortune` depends on `goodfortune`, we end up with either no fortune or an infinite fortune. So we throw an error.

## Using the libary

The main function you'll want to use is probably `runFile`; it reads a file and generates randomized text:

```
 λ:> runFile [] "test/templates/gambling.mad"
 "heads"
```

Haddock documentation of all available functionality is located [here](https://hackage.haskell.org/package/madlang#readme).

## Installation

### Releases

If you're on windows or linux, grabbing release binaries is probably the
easiest. Find them [here](https://github.com/vmchale/madlibs/releases).

### Stack

Download `stack` with

```
curl -sSL http://haskellstack.org | sh
```

Then run `stack install madlang --resolver nightly` and you'll get the `madlang` executable installed on your path.

You can also run `stack install` in the appropriate directory after cloning this
repository. 

### Use

To use it, try

```
 $ madlang run fortune-teller.mad
```

You can do `madlang --help` if you want a couple other options for debugging.

## Syntax Highlighting

Syntax highlighting for the DSL is provided in the vim plugin [here](http://github.com/vmchale/madlang-vim). It includes integration with [syntastic](https://github.com/vim-syntastic/syntastic).
