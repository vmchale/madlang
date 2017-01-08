{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Madlibs.Exec.Main
    ( exec
    ) where

import Madlibs.Cata.Run
import Madlibs.Ana.Parse
import Madlibs.Internal.Types
import Madlibs.Internal.Utils
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Text.Megaparsec
import Options.Generic
import Control.Monad.State

-- | datatype for the program
data Program = Program { input :: FilePath <?> "filepath to template"
                       , debug :: Bool <?> "whether to display parsed RandTok" --also: display crontab in that case!
                       } deriving (Generic)

instance ParseRecord Program where

exec :: IO ()
exec = do
    x <- getRecord "Madlibs templating DSL"
    let filepath = unHelpful . input $ x
    parsed <- parseFile filepath
    runFile filepath >>= TIO.putStrLn
    if unHelpful . debug $ x then
        print parsed
    else
        pure ()

runFile :: FilePath -> IO T.Text
runFile filepath = do
    parsed <- parseFile filepath
    either (pure . show') (>>= (pure . show')) (run <$> parsed)

parseFile :: FilePath -> IO (Either (ParseError Char Dec) RandTok)
parseFile filepath = do
    txt <- T.pack <$> readFile filepath
    let val = runParser program "" txt
    let built = snd . head . (filter (\(i,j) -> i == "Template")) . (flip execState []) . build <$> val
    pure built
