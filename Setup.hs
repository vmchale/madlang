{-# LANGUAGE TemplateHaskell #-}

import           Control.Monad       (unless, void)
import           Data.FileEmbed      (embedStringFile)
import           Data.Maybe
import           Distribution.Simple
import           System.Directory    (createDirectoryIfMissing)
import           System.Environment  (lookupEnv)
import           System.Process      (readCreateProcessWithExitCode, shell)

manpage :: String
manpage = $(embedStringFile "man/madlang.1")

main :: IO ()
main = setManpath >>
    writeManpages >>
    writeBashCompletions >>
    defaultMain

setManpath :: IO ()
setManpath = do
    home <- lookupEnv "HOME"
    case home of
        Just x -> do
            let bashRc = x ++ "/.bashrc"
            config <- readFile bashRc
            unless ("#manpath updated by madlang" `elem` lines config)
                (appendFile bashRc "\n#manpath updated by madlang\nexport MANPATH=~/.local/share:$MANPATH\n" >>
                 void (readCreateProcessWithExitCode (shell $ "MANPATH=" ++ x ++ "/.local/share mandb") ""))
        Nothing -> pure ()

writeManpages :: IO ()
writeManpages = do
    home <- lookupEnv "HOME"
    case home of
        Just x -> do
            let manPath = x ++ "/.local/share/man/man1"
            createDirectoryIfMissing True manPath
            writeFile (manPath ++ "/madlang.1") manpage
        Nothing -> pure ()

-- TODO only write if we can't find our own thing!
writeBashCompletions :: IO ()
writeBashCompletions = do
    home <- lookupEnv "HOME"
    case home of
        Just x -> do
            let bashRc = x ++ "/.bashrc"
            config <- readFile bashRc
            unless ("# Added by madlang" `elem` lines config)
                (appendFile bashRc "\n# Added by madlang\neval \"$(madlang --bash-completion-script madlang)\"\n")
        Nothing -> pure ()
