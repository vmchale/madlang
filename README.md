## Madlibs DSL for generating random text

This is the Madlibs DSL for generating random text. There is also a vim plugin for highlighting `.mad` files. 

It enables you to generate random, templated text with very little effort or expertise. 

It can be used for twitter bots and more productive things.

### Exmaples

An exmaple is worth a thousand words (?), so suppose you wanted to generate a mediocre fortune telling bot. You could write the following code:

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

There are two "statements" in madlang, `:define` and `:return`. `:return` is the main string we'll be spitting back, so you're only allowed one of them per file. `:define` on the other hand can be used to make as many templates as you want. These templates are combinations of strings (enclosed in quotes) and names of other templates.

Of course, you can't have a circular reference with names - if `goodfortune` depends on `fortune` while `fortune` depends on `goodfortune`, you'll end up with either no fortune or an infinite fortune. So instead we just throw an error. 

### Using the libary

The main function you'll want to use is probably `runFile`; it reads a file and generates randomized text:

```
 λ> runFile "test/templates/gambling.mad"
 "heads"
```

Haddock documentation of all available functionality is located [here](https://hackage.haskell.org/package/madlang-0.1.0.0#readme). 

## Installation

### Stack

Download `stack` with

```
curl -sSL http://haskellstack.org | sh
```

Then run `stack install` and you'll get the `madlang` executable installed on your path. You can even do `stack install madlang` if you'd like. 

### Use

To use it, just try

```
 $ madlang --input fortune-teller.mad
```

You can do `madlang --help` if you want a couple other options for debugging.

### Syntax Highlighting

Syntax highlighting for the DSL is provided in the vim plugin [here](http://github.com/vmchale/madlang-vim). You'll have to do `:set syntax=madlang` the first time you run it but everything else should work out of the box.:
