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
import Options.Applicative hiding (ParseError)
import Data.Monoid
import Data.Composition
import System.Directory
--
import Data.Tree

-- | datatype for the program
data Program = Program { sub :: Subcommand 
                       , input :: FilePath 
                       }

-- | datatype for the subcommands
data Subcommand = Debug { version :: Bool }
                | Run { rep :: Maybe Int , clInputs :: [String] }
                | Lint { clInputs :: [String] }

-- | Parser for command-line options for the program
orders :: Parser Program
orders = Program
    <$> (hsubparser
        (command "run" (info temp (progDesc "Generate text from a .mad file"))
        <> command "debug" (info debug (progDesc "Debug a template"))
        <> command "lint" (info lint (progDesc "Lint a file"))))
    <*> (argument str
        (metavar "FILEPATH"
        <> help "File path to madlang template"))

-- | Parser for debug subcommand
debug :: Parser Subcommand
debug = Debug
    <$> switch
        (long "version"
        <> short 'v'
        <> help "Show version information")

-- | Parser for the run subcommand
temp :: Parser Subcommand
temp = Run
    <$> (optional $ read <$> strOption
        (long "rep"
        <> short 'r'
        <> metavar "REPETITIONS"
        <> help "Number of times to repeat"))
    <*> (many $ strOption
        (short 'i'
        <> metavar "VAR"
        <> help "command-line inputs to the template."))

-- | Parser for the lint subcommand
lint :: Parser Subcommand
lint = Lint
    <$> (many $ strOption
        (short 'i'
        <> metavar "VAR"
        <> help "command-line inputs to the template."))

-- | Main program action
runMadlang :: IO ()
runMadlang = execParser wrapper >>= template 

-- | Wraps parser with help parser
wrapper = info (helper <*> orders)
    (fullDesc
    <> progDesc "Madlang templating language"
    <> header "Madlang - markov chains made easy")

-- | given a parsed record perform the appropriate IO action
template :: Program -> IO ()
template rec = do
    let toFolder = input $ rec
    setCurrentDirectory (getDir toFolder)
    let filepath = reverse . (takeWhile (/='/')) . reverse $ toFolder
    let ins = map T.pack $ (clInputs . sub $ rec)
    case sub rec of
        (Run reps _) -> do
            parsed <- parseFile ins filepath
            replicateM_ (maybe 1 id reps) $ runFile ins filepath >>= TIO.putStrLn 
        (Debug _) -> do
            putStr . (either show (drawTree . tokToTree 1.0)) =<< makeTree ins filepath -- parsed
        (Lint _) -> do
            parsed <- parseFile ins filepath
            putStrLn $ either parseErrorPretty (const "No syntax errors found.") parsed

-- | Generate randomized text from a template
templateGen :: FilePath -> [T.Text] -> T.Text -> Either (ParseError Char Dec) (IO T.Text)
templateGen filename ins txt = run <$> parseTok filename ins txt

-- | Generate text from file with inclusions
runInclusions :: [T.Text] -> FilePath -> IO (Either (ParseError Char Dec) (IO T.Text))
runInclusions = fmap (fmap run) .* parseFile

-- | Parse a template file into the `RandTok` data type
-- FIXME this should pass errors up correctly, also read files correctly?
parseFile :: [T.Text] -> FilePath -> IO (Either (ParseError Char Dec) RandTok)
parseFile ins filepath = do
    file <- readFile' filepath
    let filenames = either (const []) id $ parseInclusions filepath file 
    ctx <- mapM (getInclusionCtx ins) filenames 
    parseWithCtx ins filepath (concatMap (either (const []) id) ctx)

-- | Generate text from file with inclusions
getInclusionCtx :: [T.Text] -> FilePath -> IO (Either (ParseError Char Dec) [(Key, RandTok)])
getInclusionCtx ins filepath = do
    file <- readFile' filepath
    let filenames = either (const []) id $ parseInclusions filepath file -- FIXME pass up errors correctly
    ctx <- mapM (getInclusionCtx ins) filenames 
    parseCtx ins (concatMap (either (const []) id) ctx) filepath

-- | Generate randomized text from a file conatining a template
runFile :: [T.Text] -> FilePath -> IO T.Text
runFile ins filepath = do
    result <- runInclusions ins filepath
    either (pure . parseErrorPretty') (>>= (pure . show')) result

-- | Get file as context
parseCtx :: [T.Text] -> [(Key, RandTok)] -> FilePath -> IO (Either (ParseError Char Dec) [(Key, RandTok)]) 
parseCtx ins state filepath = do
    txt <- readFile' filepath
    let keys = parseTokF filepath state ins txt
    pure keys

parseWithCtx :: [T.Text] -> FilePath -> [(Key, RandTok)] -> IO (Either (ParseError Char Dec) RandTok) 
parseWithCtx ins filepath ctx = do
    txt <- readFile' filepath
    let val = parseTokCtx filepath ctx ins txt
    pure val

-- | Parse a template into a RandTok suitable to be displayed as a tree
makeTree :: [T.Text] -> FilePath -> IO (Either (ParseError Char Dec) RandTok)
makeTree ins filepath = do
    txt <- readFile' filepath
    let val = parseTree filepath ins txt
    pure val
