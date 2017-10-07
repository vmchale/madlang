module Text.Madlibs.Exec.Helpers (fetchPackages, cleanPackages) where

import qualified Codec.Archive.Tar      as Tar
import           Codec.Compression.GZip (decompress)
import           Network.HTTP.Client    hiding (decompress)
import           System.Environment     (getEnv)

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
    let packageDir = home ++ "/.madlang"
    Tar.unpack packageDir . Tar.read . decompress $ byteStringResponse

cleanPackages :: IO ()
cleanPackages = do
    putStrLn "done."
