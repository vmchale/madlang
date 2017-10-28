# Using Madlang in Haskell

As advertised in the `README.md`, Madlang can be used as an EDSL that embeds
into Haskell. Among other things, this can be used to write [performant
templates](http://vmchale.com/recursion-scheme-generator/index.html), making
Madlang suitable anywhere that Haskell is.

Here is a short example:

```haskell
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes     #-}

import Text.Madlibs

gambling :: RandTok
gambling = [|madlang|
:define something
    1.0 "hello"
    1.0 "goodbye"
:return
    1.0 something
|]
```

Alternately, for more sophisticated projects, you can embed an external source
file like so:

```haskell
{-# LANGUAGE TemplateHaskell #-}

sourceText :: IO T.Text
sourceText = run $(madFile "mad-src/template.mad")
```

This is used for example in the [recursion scheme
generator](http://vmchale.com/recursion-scheme-generator/index.html) example.
