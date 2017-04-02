-- | Module containing IO stuff to get/parse files with external dependencies
module Text.Madlibs.Ana.Resolve where

import qualified Data.Text as T
import Text.Megaparsec
import Text.Madlibs.Internal.Types
import Text.Madlibs.Internal.Utils
import Text.Madlibs.Ana.Parse
import Text.Madlibs.Ana.ParseUtils
import Text.Madlibs.Cata.Run
import Data.Composition

-- | Parse a template file into the `RandTok` data type
parseFile :: [T.Text] -> FilePath -> IO (Either (ParseError Char Dec) RandTok)
parseFile = fmap (fmap takeTemplate) .* (getInclusionCtx False)

-- | Generate text from file with inclusions
getInclusionCtx :: Bool -> [T.Text] -> FilePath -> IO (Either (ParseError Char Dec) [(Key, RandTok)])
getInclusionCtx isTree ins filepath = do
    file <- readFile' filepath
    let filenames = either (const []) id $ parseInclusions filepath file -- FIXME pass up errors correctly
    ctx <- mapM (getInclusionCtx isTree ins) filenames 
    parseCtx isTree ins (concatMap (either (const []) id) ctx) filepath

-- | Generate randomized text from a file conatining a template
runFile :: [T.Text] -> FilePath -> IO T.Text
runFile = ((either (pure . parseErrorPretty') (>>= (pure . show'))) =<<) .* (fmap (fmap run) .* parseFile)

-- | Get file as context
parseCtx :: Bool -> [T.Text] -> [(Key, RandTok)] -> FilePath -> IO (Either (ParseError Char Dec) [(Key, RandTok)]) 
parseCtx isTree ins state filepath = do
    txt <- readFile' filepath
    let keys = (if isTree then parseTreeF else parseTokF) filepath state ins txt
    pure keys

-- | Parse a template into a RandTok suitable to be displayed as a tree
makeTree :: [T.Text] -> FilePath -> IO (Either (ParseError Char Dec) RandTok)
makeTree = fmap (fmap takeTemplate) .* (getInclusionCtx True)
