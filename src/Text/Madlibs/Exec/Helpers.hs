{-# LANGUAGE OverloadedStrings #-}

module Text.Madlibs.Exec.Helpers (fetchPackages, cleanPackages, installVimPlugin) where

import qualified Codec.Archive.Tar      as Tar
import           Codec.Archive.Zip      (ZipOption (..),
                                         extractFilesFromArchive, toArchive)
import           Codec.Compression.GZip (decompress)
import           Network.HTTP.Client    hiding (decompress)
import           System.Environment     (getEnv)
import           System.Info            (os)

installVimPlugin :: IO ()
installVimPlugin = do

    putStrLn "fetching latest vim plugin..."
    manager <- newManager defaultManagerSettings
    initialRequest <- parseRequest "http://vmchale.com/static/vim.zip"
    response <- httpLbs (initialRequest { method = "GET" }) manager
    let byteStringResponse = responseBody response

    putStrLn "installing locally..."
    home <- getEnv "HOME"
    let packageDir = if os /= "mingw32" then home ++ "/.vim" else home ++ "\\vimfiles"
    let archive = toArchive byteStringResponse
    let options = OptDestination packageDir
    extractFilesFromArchive [options] archive

-- TODO set remote package url flexibly
fetchPackages :: IO ()
fetchPackages = do

    putStrLn "fetching libraries..."
    manager <- newManager defaultManagerSettings
    initialRequest <- parseRequest "http://vmchale.com/static/packages.tar.gz"
    response <- httpLbs (initialRequest { method = "GET" }) manager
    let byteStringResponse = responseBody response

    putStrLn "unpacking libraries..."
    home <- getEnv "HOME"
    let packageDir = if os /= "mingw32" then home ++ "/.madlang" else home ++ "\\.madlang"
    Tar.unpack packageDir . Tar.read . decompress $ byteStringResponse

cleanPackages :: IO ()
cleanPackages = do
    putStrLn "done."
