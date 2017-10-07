-- | = Madlang Text Generation Library, EDSL, and Interpreted Language
--
-- == Purpose
--
-- Madlang is a text-genrating Domain-Specific Language (DSL). It is similar in purpose
-- to <https://github.com/galaxykate/tracery tracery>, but it is
-- written in Haskell and therefore offers more flexibility. 
--
-- == Example
--
-- In file example.mad:
-- @
-- :define gambling
--     1.0 "heads"
--     1.0 "tails"
--  :return
--     1.0 "The result of the coin flip was: " gambling
-- @
--
-- > $ madlang run example.mad
-- > tails
module Text.Madlibs (
                    -- * Parsers for @.mad@ files
                      parseTok
                    , parseTokM
                    , runFile
                    , parseFile
                    , makeTree
                    -- * Functions and constructs for the `RandTok` data type
                    , run
                    , runText
                    , RandTok (..)
                    -- * Types associated with the parser
                    , Context
                    , SemanticError (..)
                    -- * Command-line executable
                    , runMadlang
                    -- * Template Haskell EDSL
                    , madlang
                    , madFile
                    ) where

import Text.Madlibs.Ana.Resolve
import Text.Madlibs.Ana.Parse
import Text.Madlibs.Cata.Run
import Text.Madlibs.Cata.SemErr
import Text.Madlibs.Exec.Main
import Text.Madlibs.Internal.Types
import Text.Madlibs.Generate.TH
