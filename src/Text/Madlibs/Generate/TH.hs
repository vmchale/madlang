{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DeriveLift                 #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}

-- | Module containing Quasi-Quoter and Template Haskell splice for use as an EDSL.
module Text.Madlibs.Generate.TH
    ( madFile
    , madlang
    ) where

import           Control.Arrow               (first)
import           Control.Monad.IO.Class      (MonadIO, liftIO)
import           Data.FileEmbed              (embedStringFile)
import           Data.Monoid
import qualified Data.Text                   as T
import qualified Data.Text.IO                as TIO
import           Data.Void
import           Language.Haskell.TH         hiding (Dec)
import           Language.Haskell.TH.Quote
import           Language.Haskell.TH.Syntax  (lift)
import           System.Directory            (doesFileExist)
import           System.Environment          (getEnv)
import           Text.Madlibs.Ana.Parse
import           Text.Madlibs.Internal.Types (Key, RandTok)
import           Text.Madlibs.Internal.Utils
import           Text.Megaparsec

instance MonadIO Q where
    liftIO = runIO

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
--     1.0 something
-- |]
-- @
--
-- Note that this is in general much faster than running interpreted code, though inclusions
-- do not work in the `QuasiQuoter` or in spliced expressions.
madlang :: QuasiQuoter
madlang = QuasiQuoter { quoteExp = textToExpression
                      , quotePat = error "quasi-quoter does not support patterns"
                      , quoteType = error "quasi-quoter does not support types"
                      , quoteDec = error "quasi-quoter does not support top-level quotes"
                      }

-- | Convert a `String` containing  to a `Q Exp` with the parsed syntax tree.
-- Embedded code can contain inclusions.
textToExpression :: String -> Q Exp
textToExpression txt = do
    parse' <- [|parseTokInternal "haskell quasi-quote" []|]
    inclusions <- parseInclusions "haskell quasi-quote" <$> pure (T.pack txt)
    let context = fmap (traverse (madCtxCheck ".")) (fmap T.unpack <$> inclusions)
    erroredFiles <- errorgen context
    inclusions' <- lift (T.unpack <$> errorgen inclusions)
    pure $ VarE 'errorgen `AppE` (parse' `AppE` (VarE 'ctx `AppE` inclusions' `AppE` ListE erroredFiles) `AppE` (VarE 'T.pack `AppE` LitE (StringL txt)))

-- | Turn a parse error into an error that will be caught when Template Haskell compiles at runtime.
errorgen :: Either (ParseError Char (ErrorFancy Void)) a -> a
errorgen = either (error . T.unpack . show') id

madCtxCheck :: FilePath -> FilePath -> Q Exp
madCtxCheck folder path = do
    let tryPath = folder ++ path
    local <- runIO $ doesFileExist tryPath
    home <- runIO $ getEnv "HOME"
    if local then
        madCtx folder path
    else
        madCtx (home ++ "/.madlang/") path

ctx :: [FilePath] -> [[(Key, RandTok)]] -> [[(Key, RandTok)]]
ctx = zipWith resolveKeys
    where resolveKeys file = fmap (first (((T.pack . (<> "-")) . dropExtension) file <>))

madCtx :: FilePath -> FilePath -> Q Exp
madCtx folder path = do
    let tryPath = folder ++ path
    file <- embedStringFile tryPath
    inclusions <- parseInclusions tryPath <$> runIO (TIO.readFile tryPath)
    parse' <- [|parseTokFInternal path []|]
    dependencies <- traverse (madCtxCheck folder) (T.unpack <$> errorgen inclusions)
    inclusions' <- lift (T.unpack <$> errorgen inclusions)
    pure $ VarE 'errorgen `AppE` (parse' `AppE` (VarE 'ctx `AppE` inclusions' `AppE` ListE dependencies) `AppE` file)

-- | Splice for embedding a '.mad' file, e.g.
--
-- @
-- demo :: IO T.Text
-- demo = run
--     $(madFile "twitter-bot.mad")
-- @
--
-- Embedded code can contain inclusions.
madFile :: FilePath -> Q Exp
madFile path = do
    inclusions <- parseInclusions path <$> runIO (TIO.readFile path)
    file <- embedStringFile path
    let context = fmap (traverse (madCtxCheck (getDir path))) (fmap T.unpack <$> inclusions)
    erroredFiles <- errorgen context
    parse' <- [|parseTokInternal path []|]
    inclusions' <- lift (T.unpack <$> errorgen inclusions)
    pure $ VarE 'errorgen `AppE` (parse' `AppE` (VarE 'ctx `AppE` inclusions' `AppE` ListE erroredFiles) `AppE` file)
