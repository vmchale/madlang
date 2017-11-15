{-# LANGUAGE FlexibleContexts #-}

-- | Module containing IO stuff to get/parse files with external dependencies
module Text.Madlibs.Ana.Resolve (
    parseFile
  , runFile
  , makeTree
  , runText
  , runFileN
  ) where

import           Control.Arrow               (first)
import           Control.Composition
import           Control.Exception
import           Control.Monad               (replicateM, void)
import           Control.Monad.IO.Class      (MonadIO, liftIO)
import           Control.Monad.Random.Class
import           Data.Monoid
import qualified Data.Text                   as T
import           Data.Void
import           System.Directory
import           System.Environment
import           System.Info                 (os)
import           Text.Madlibs.Ana.Parse
import           Text.Madlibs.Ana.ParseUtils
import           Text.Madlibs.Cata.Run
import           Text.Madlibs.Internal.Types
import           Text.Madlibs.Internal.Utils
import           Text.Megaparsec             hiding (parseErrorPretty')

-- | Parse a template file into the `RandTok` data type
parseFile :: MonadIO m
    => [T.Text] -- ^ variables to substitute into the template
    -> FilePath -- ^ folder
    -> FilePath -- ^ filepath within folder
    -> m (Either (ParseError Char (ErrorFancy Void)) RandTok) -- ^ parsed `RandTok`
parseFile = fmap (fmap takeTemplate) .** getInclusionCtx False

-- | Generate text from file with inclusions
getInclusionCtx :: (MonadIO m) => Bool -> [T.Text] -> FilePath -> FilePath -> m (Either (ParseError Char (ErrorFancy Void)) [(Key, RandTok)])
getInclusionCtx isTree ins folder filepath = liftIO $ do
    libDir <- do { home <- getEnv "HOME" ; if os /= home then pure (home <> "/.madlang/") else pure (home <> "\\.madlang\\") }
    file <- catch (readFile' (folder ++ filepath)) (const (readLibFile (libDir <> folder <> filepath)) :: IOException -> IO T.Text)
    let filenames = map T.unpack $ either (error . show) id $ parseInclusions filepath file -- TODO pass up errors correctly
    let resolveKeys file' = fmap (first (((T.pack . (<> "-")) . dropExtension) file' <>))
    ctxPure <- mapM (getInclusionCtx isTree ins folder) filenames
    let ctx = zipWith resolveKeys filenames <$> sequence ctxPure
    catch
        (parseCtx isTree ins (concat . either (const []) id $ ctx) (folder ++ filepath))
        (const (do { home <- getEnv "HOME" ; parseCtx isTree ins (concat . either (const []) id $ ctx) (home <> (pathSep : ".madlang") <> [pathSep] <> folder <> filepath) }) :: IOException -> IO (Either (ParseError Char (ErrorFancy Void)) [(Key, RandTok)]))

pathSep :: Char
pathSep | os == "windows" = '\\'
        | otherwise = '/'

-- | Generate randomized text from a file containing a template
runFile :: [T.Text] -- ^ List of variables to substitute into the template
    -> FilePath -- ^ Path to @.mad@ file.
    -> IO T.Text -- ^ Result
runFile ins toFolder = do
    void $ doesDirectoryExist (getDir toFolder)
    let filepath = reverse . takeWhile (/= pathSep) . reverse $ toFolder
    runInFolder ins (getDir toFolder) filepath

runFileN :: Int -> [T.Text] -> FilePath -> IO [T.Text]
runFileN n ins toFolder = do
    void $ doesDirectoryExist (getDir toFolder)
    let filepath = reverse . takeWhile (/= pathSep) . reverse $ toFolder
    runInFolderN n ins (getDir toFolder) filepath

-- | Run 'n' times.
runInFolderN :: Int -> [T.Text] -> FilePath -> FilePath -> IO [T.Text]
runInFolderN n = replicateM n .** runInFolder

-- | Run in the appropriate folder
runInFolder :: [T.Text] -> FilePath -> FilePath -> IO T.Text
runInFolder = (either (pure . parseErrorPretty') (>>= (pure . show')) =<<) .** (fmap (fmap run) .** parseFile)

-- | Run based on text input, with nothing linked.
runText :: (MonadRandom m) => [T.Text] -> String -> T.Text -> m T.Text
runText vars name = either (pure . parseErrorPretty') id . fmap run . parseTok name [] vars

-- | Get file as context
parseCtx :: (MonadIO m) => Bool -> [T.Text] -> [(Key, RandTok)] -> FilePath -> m (Either (ParseError Char (ErrorFancy Void)) [(Key, RandTok)])
parseCtx isTree ins state filepath = do
    txt <- liftIO $ readFile' filepath
    let keys = (if isTree then parseTreeF else parseTokF) filepath state ins txt
    pure keys

-- | Parse a template into a RandTok suitable to be displayed as a tree
makeTree :: [T.Text] -> FilePath -> FilePath -> IO (Either (ParseError Char (ErrorFancy Void)) RandTok)
makeTree = fmap (fmap takeTemplate) .** getInclusionCtx True
