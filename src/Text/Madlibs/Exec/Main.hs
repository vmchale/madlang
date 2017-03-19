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
--
import Data.Tree

-- | datatype for the program
data Program = Program { sub :: Subcommand 
                       , input :: FilePath 
                       }

data Subcommand = Debug { version :: Bool }
                | Run { rep :: Maybe Int , clInputs :: [String] }
                | Lint { clInputs :: [String] }

orders :: Parser Program
orders = Program
    <$> (hsubparser
        (command "run" (info temp (progDesc "Generate text from a .mad file"))
        <> command "debug" (info debug (progDesc "Debug a template"))
        <> command "lint" (info lint (progDesc "Lint a file"))))
    <*> (argument str
        (metavar "FILEPATH"
        <> help "File path to madlang template"))

debug :: Parser Subcommand
debug = Debug
    <$> switch
        (long "version"
        <> short 'v'
        <> help "Show version information")

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
    let filepath = input $ rec
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

-- | Generate randomized text from a file conatining a template
runFile :: [T.Text] -> FilePath -> IO T.Text
runFile ins filepath = do
    txt <- readFile' filepath
    either (pure . parseErrorPretty') (>>= (pure . show')) (templateGen filepath ins txt)

-- | Parse a template file into the `RandTok` data type
parseFile :: [T.Text] -> FilePath -> IO (Either (ParseError Char Dec) RandTok)
parseFile ins filepath = do
    txt <- readFile' filepath
    let val = parseTok filepath ins txt
    pure val

-- | Parse a template into a RandTok suitable to be displayed as a tree
makeTree :: [T.Text] -> FilePath -> IO (Either (ParseError Char Dec) RandTok)
makeTree ins filepath = do
    txt <- readFile' filepath
    let val = parseTree filepath ins txt
    pure val
