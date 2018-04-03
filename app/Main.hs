-- | Provides `madlang` runMadlangutable
module Main (
    main ) where

import           Data.Maybe
import qualified Data.Text           as T
import qualified Data.Text.Lazy      as TL
import qualified Data.Text.Lazy.IO   as TLIO
import           Data.Version
import           Options.Applicative hiding (ParseError)
import           Paths_madlang
import           System.Directory
import           Text.Madlibs
import           Text.Megaparsec     hiding (many)

-- | datatype for the program
newtype Program = Program { sub :: Subcommand }

-- | datatype for the subcommands
data Subcommand = Debug { input :: FilePath }
                | Run { _rep :: Maybe Int , input :: FilePath }
                | Lint { clInputs :: [String] , input :: FilePath }
                | Sample { clInputs :: [String], input :: FilePath }
                | Get { _remote :: String }
                | Install
                | VimInstall

-- | Parser for command-line options for the program
orders :: Parser Program
orders = Program
    <$> (hsubparser
        (command "run" (info temp (progDesc "Generate text from a .mad file"))
        <> command "tree" (info debug (progDesc "Display a tree with all possible paths"))
        <> command "check" (info lint (progDesc "Check a file"))
        <> command "sample" (info sample (progDesc "Sample a template by generating text many times."))
        <> command "install" (info (pure Install) (progDesc "Install/update prebundled libraries."))
        <> command "get" (info fetch (progDesc "Sample a template by generating text many times."))
        <> command "vim" (info (pure VimInstall) (progDesc "Install vim plugin."))
        ))

-- | Parser for the run subcommand
temp :: Parser Subcommand
temp = Run
    <$> (optional $ read <$> strOption
        (long "rep"
        <> short 'r'
        <> metavar "REPETITIONS"
        <> help "Number of times to repeat"))
    <*> (argument str
        (metavar "FILEPATH"
        <> completer (bashCompleter "file -X '!*.mad' -o plusdirs")
        <> help "File path to madlang template"))

-- | Parser for the sample subcommand
sample :: Parser Subcommand
sample = Sample
    <$> (many $ strOption
        (short 'i'
        <> metavar "VAR"
        <> help "command-line inputs to the template."))
    <*> (argument str
        (metavar "FILEPATH"
        <> completer (bashCompleter "file -X '!*.mad' -o plusdirs")
        <> help "File path to madlang template"))

fetch :: Parser Subcommand
fetch = Get
    <$> (argument str
        (metavar "REPOSITORY"
        <> help "Repository to fetch, e.g. vmchale/some-library"))

debug :: Parser Subcommand
debug = Debug
    <$> (argument str
        (metavar "FILEPATH"
        <> completer (bashCompleter "file -X '!*.mad' -o plusdirs")
        <> help "File path to madlang template"))

-- | Parser for the lint subcommand
lint :: Parser Subcommand
lint = Lint
    <$> (many $ strOption
        (short 'i'
        <> metavar "VAR"
        <> help "command-line inputs to the template."))
    <*> (argument str
        (metavar "FILEPATH"
        <> completer (bashCompleter "file -X '!*.mad' -o plusdirs")
        <> help "File path to madlang template"))

-- | Main program action
--
-- Example Usage:
--
-- > $ madlang run example.mad
-- > some text generated
main :: IO ()
main = execParser wrapper >>= template

versionInfo :: Parser (a -> a)
versionInfo = infoOption ("madlang version: " ++ showVersion version) (short 'v' <> long "version" <> help "Show version")

-- | Wraps parser with help parser
wrapper :: ParserInfo Program
wrapper = info (helper <*> versionInfo <*> orders)
    (fullDesc
    <> progDesc "Madlang templating language. For more detailed help, use 'man madlang'"
    <> header ("Madlang - templating text made easy"))

-- | given a parsed record perform the appropriate IO action
template :: Program -> IO ()
template rec =
    case sub rec of
        Install -> fetchPackages >> cleanPackages
        VimInstall -> installVimPlugin
        Get remote -> fetchGithub remote
        _ -> do
            let toFolder = input . sub $ rec
            if getDir toFolder == "" then pure () else setCurrentDirectory (getDir toFolder)
            let filepath = reverse . (takeWhile (/='/')) . reverse $ toFolder
            let ins = map T.pack (clInputs . sub $ rec)
            case sub rec of
                (Run reps _) ->
                    (TL.init . TL.unlines . fmap TL.fromStrict <$> runFileN (fromMaybe 1 reps) ins filepath) >>= TLIO.putStrLn
                (Sample _ _) ->
                    (TL.init . TL.unlines . fmap TL.fromStrict <$> runFileN 60 ins filepath) >>= TLIO.putStrLn
                (Debug _) -> putStr . (either show displayTree) =<< makeTree ins "" filepath
                (Lint _ _) -> do
                    parsed <- parseFile ins "" filepath
                    putStrLn $ either (parseErrorPretty) (const "No syntax errors found.") parsed
                _ -> pure ()
