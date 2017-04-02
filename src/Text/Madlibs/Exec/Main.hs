-- | Provides `madlang` runMadlangutable
module Text.Madlibs.Exec.Main where

import Control.Monad
import Text.Madlibs.Cata.Run
import Text.Madlibs.Ana.Resolve
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
import Text.Madlibs.Ana.Parse

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
        <>  metavar "VAR"
        <> help "command-line inputs to the template."))
        -- TODO consider making arguments nicer?

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
    let toFolder = input rec
    if getDir toFolder == "" then pure () else setCurrentDirectory (getDir toFolder)
    let filepath = reverse . (takeWhile (/='/')) . reverse $ toFolder
    let ins = map T.pack $ (clInputs . sub $ rec)
    case sub rec of
        (Run reps _) -> do
            parsed <- parseFile ins filepath
            replicateM_ (maybe 1 id reps) $ runFile ins filepath >>= TIO.putStrLn 
        (Debug _) -> do
            putStr . (either show displayTree) =<< makeTree ins filepath 
        (Lint _) -> do
            parsed <- parseFile ins filepath
            putStrLn $ either parseErrorPretty (const "No syntax errors found.") parsed
