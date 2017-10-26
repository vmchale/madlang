{-# LANGUAGE OverloadedStrings #-}

module Text.Madlibs.Packaging.Fetch ( fetchGithub
                                    , fetchPackages
                                    , cleanPackages
                                    , installVimPlugin
                                    ) where

import Control.Monad (unless)
import qualified Codec.Archive.Tar      as Tar
import           Codec.Archive.Zip      (ZipOption (..),
                                         extractFilesFromArchive, toArchive)
import           Codec.Compression.GZip (decompress)
import           Network.HTTP.Client    hiding (decompress)
import           System.Directory       (removeFile, renameDirectory)
import           System.Environment     (getEnv)
import           System.Info            (os)
import Network.HTTP.Client.TLS (tlsManagerSettings)

-- https://hub.darcs.net/vmchale/madlang-libraries/dist

invalid :: String -> Bool
invalid = not . ('/' `elem`)

-- | As an example, `vmchale/some-library` would be valid input.
fetchGithub :: String -> IO ()
fetchGithub s = unless (invalid s) $ do

    putStrLn $ "fetching library at " ++ s
    
    manager <- newManager tlsManagerSettings
    initialRequest <- parseRequest $ "https://github.com/" ++ s ++ "/archive/master.zip"
    response <- responseBody <$> httpLbs (initialRequest { method = "GET" }) manager

    putStrLn "unpacking libraries..."
    home <- getEnv "HOME"
    let packageDir = if os /= "mingw32" then home ++ "/.madlang" else home ++ "\\.madlang"
    let options = OptDestination packageDir
    extractFilesFromArchive [options] (toArchive response)

    let repoName = filter (/='/') . dropWhile (/='/') $ s
    renameDirectory (packageDir ++ "/" ++ repoName ++ "-master") (packageDir ++ "/" ++ repoName)

installVimPlugin :: IO ()
installVimPlugin = do

    putStrLn "fetching latest vim plugin..."
    manager <- newManager defaultManagerSettings
    initialRequest <- parseRequest "http://vmchale.com/static/vim.zip"
    response <- responseBody <$> httpLbs (initialRequest { method = "GET" }) manager

    putStrLn "installing locally..."
    home <- getEnv "HOME"
    let packageDir = if os /= "mingw32" then home ++ "/.vim" else home ++ "\\vimfiles"
    let options = OptDestination packageDir
    extractFilesFromArchive [options] (toArchive response)

    putStrLn "cleaning junk..."
    removeFile (packageDir ++ "/TODO.md")
    removeFile (packageDir ++ "/vim-screenshot.png")
    removeFile (packageDir ++ "/README.md")
    removeFile (packageDir ++ "/LICENSE")

-- TODO set remote package url flexibly
fetchPackages :: IO ()
fetchPackages = do

    putStrLn "fetching libraries..."
    manager <- newManager defaultManagerSettings
    initialRequest <- parseRequest "http://vmchale.com/static/packages.tar.gz"
    response <- responseBody <$> httpLbs (initialRequest { method = "GET" }) manager

    putStrLn "unpacking libraries..."
    home <- getEnv "HOME"
    let packageDir = if os /= "mingw32" then home ++ "/.madlang" else home ++ "\\.madlang"
    Tar.unpack packageDir . Tar.read . decompress $ response

cleanPackages :: IO ()
cleanPackages =
    putStrLn "done."
