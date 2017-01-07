{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Madlibs.Exec.Main
    ( exec
    ) where

import Madlibs.Cata.Run
import Madlibs.Ana.Parse
import qualified Data.Text as T
import Text.Megaparsec
import Options.Generic

data Program = Program { input :: FilePath <?> "filepath to template"
                       , twitter :: Bool <?> "whether to tweet" --also: display crontab in that case!
                       } deriving (Generic)

instance ParseRecord Program where

exec :: IO ()
exec = do
    x <- getRecord "Madlibs templating DSL"
    let filepath = unHelpful . input $ x
    run example >>= print
    txt <- T.pack <$> readFile filepath
    let val = runParser program "" txt
    print val
