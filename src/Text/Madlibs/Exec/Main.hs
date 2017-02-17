{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

-- | Provides `madlang` runMadlangutable
module Text.Madlibs.Exec.Main where

import Control.Monad
import Text.Madlibs.Cata.Run
import Text.Madlibs.Ana.Parse
import Text.Madlibs.Internal.Types
import Text.Madlibs.Internal.Utils
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Text.Megaparsec
import Options.Generic
import Development.GitRev

-- | datatype for the program
data Program = Program { input :: FilePath <?> "filepath to template"
                       , debug :: Bool <?> "whether to display parsed RandTok" 
                       , rep :: Maybe Int <?> "How many times to repeat"
                       , v :: [String] <?> "Extra input to the template" --fix soon?
                       , version :: Bool <?> "Display version information for debugging"
                       } deriving (Generic)

-- | Generated automatically by optparse-generic.
instance ParseRecord Program where

-- | Main program action
runMadlang :: IO ()
runMadlang = do
    x <- getRecord "Text.Madlibs templating DSL"
    if unHelpful . version $ x then putStrLn build else pure ()
    case unHelpful . rep $ x of
        (Just n) -> replicateM_ n $ template x
        Nothing -> template x

-- | given a parsed record perform the appropriate IO action
template :: Program -> IO ()
template rec = do
    let filepath = unHelpful . input $ rec
    let ins = map T.pack $ (unHelpful . v $ rec)
    parsed <- parseFile ins filepath
    runFile ins filepath >>= TIO.putStrLn
    if unHelpful . debug $ rec then
        print parsed
    else
        pure ()

-- | Generate randomized text from a template
templateGen :: [T.Text] -> T.Text -> Either (ParseError Char Dec) (IO T.Text)
templateGen ins txt = run <$> parseTok ins txt

-- | Generate randomized text from a file conatining a template
runFile :: [T.Text] -> FilePath -> IO T.Text
runFile ins filepath = do
    txt <- readFile' filepath
    either (pure . parseErrorPretty') (>>= (pure . show')) (templateGen ins txt)

-- | Parse a template file into the `RandTok` data type
parseFile :: [T.Text] -> FilePath -> IO (Either (ParseError Char Dec) RandTok)
parseFile ins filepath = do
    txt <- readFile' filepath
    let val = parseTok ins txt
    pure val

-- | String with git commit string
build :: String
build = concat [ "[version: ", $(gitBranch), "@", $(gitHash)
               , " (", $(gitCommitDate), ")"
               , " (", $(gitCommitCount), " commits in HEAD)"
               , dirty, "] "] 
    where
        dirty | $(gitDirty) = " (uncommitted files present)" 
              | otherwise   = ""
