{-# LANGUAGE TemplateHaskell #-}

-- | Module containing Quasi-Quoter and Template Haskell splice for use as an EDSL.
module Text.Madlibs.Generate.TH
    ( madFile
    , madlang
    ) where

import           Data.FileEmbed
import qualified Data.Text                   as T
import           Data.Text.Encoding
import           Data.Void
import           Language.Haskell.TH         hiding (Dec)
import           Language.Haskell.TH.Quote
import           Text.Madlibs.Ana.Parse
import           Text.Madlibs.Internal.Utils
import           Text.Megaparsec

-- | `QuasiQuoter` for an EDSL, e.g.
--
-- @
-- demoQQ :: T.Text
-- demoQQ = run
-- [madlang|
-- :define something
--     1.0 "hello"
--     1.0 "goodbye"
-- :return
--     1.0 something|]
-- @
--
-- Note that this is in general much faster than running interpreted code, though inclusions
-- do not work in the `QuasiQuoter` or in spliced expressions.
madlang :: QuasiQuoter
madlang = QuasiQuoter { quoteExp = textToExpression
                      , quotePat = error "quasi-quoter does not support patterns"
                      , quoteType = error "quasi-quoter does not support types"
                      , quoteDec = error "quasi-quoter does not support top-level quotes"
                      } -- TODO add quasiQuoter w/inclusions or context

-- | Convert a `String` containing  to a `Q Exp` with the parsed syntax tree.
textToExpression :: String -> Q Exp
textToExpression txt = do
    parse' <- [|parseTok "source" [] []|]
    pure $ (VarE 'errorgen) `AppE` (parse' `AppE` ((VarE 'T.pack) `AppE` (LitE (StringL (txt)))))

-- | Turn a parse error into an error that will be caught when Template Haskell compiles at runtime.
errorgen :: Either (ParseError Char (ErrorFancy Void)) a -> a
errorgen = either (error . T.unpack . show') id

-- | Splice for embedding a '.mad' file, e.g.
--
-- @
-- demo :: IO T.Text
-- demo = run
--     $(madFile "twitter-bot.mad")
-- @
--
-- Note that the embedded code cannot have any inclusions.
madFile :: FilePath -> Q Exp
madFile path = do
    file <- (embedFile path)
    parse' <- [|(parseTok "source" [] []) . decodeUtf8|] -- TODO make this recurse but still work!
    pure $ (VarE 'errorgen) `AppE` (parse' `AppE` file)
