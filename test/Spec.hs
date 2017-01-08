{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec
import Test.Hspec.Megaparsec
import Control.Exception (evaluate)
import Text.Madlibs.Ana.Parse hiding (main)
import Text.Madlibs.Internal.Types
import Text.Megaparsec
import Control.Monad.IO.Class
import Control.Monad.State
import qualified Data.Text as T

main :: IO ()
main = hspec $ do
    describe "parseTok" $ do
        it "parses a .mad string" $ do
            parseTok madFile `shouldParse` (List [(1.0,List [(0.5,Value "heads"),(0.5,Value "tails")])])
        it "fails when quotes aren't closed" $ do
            parseTok `shouldFailOn` madFileFailure

madFile :: T.Text
madFile = ":define something\n    0.5 \"heads\"\n    0.5 \"tails\"\n:return\n    1.0 something"

madFileFailure :: T.Text
madFileFailure = ":define something\n    0.5 \"heads\"\n    0.5 \"tails\n:return\n    1.0 something"
