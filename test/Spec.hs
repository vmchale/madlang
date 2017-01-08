{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec
import Test.Hspec.Megaparsec
import Control.Exception (evaluate)
import Text.Madlibs.Ana.Parse hiding (main)
import Text.Madlibs.Internal.Types
import Text.Madlibs.Internal.Utils
import Text.Madlibs.Cata.Run
import Text.Megaparsec
import Control.Monad.IO.Class
import Control.Monad.State
import Data.Function
import qualified Data.Text as T

main :: IO ()
main = hspec $ do
    describe "parseTok" $ do
        it "parses a .mad string" $ do
            parseTok madFile `shouldParse` (List [(1.0,List [(0.5,Value "heads"),(0.5,Value "tails")])])
        it "fails when quotes aren't closed" $ do
            parseTok `shouldFailOn` madFileFailure
        it "parses when functions are out of order" $ do
            parseTok `shouldSucceedOn` madComplexFile
        it "returns a correct string from the template when evaluating a token" $ do
            (testIO . run) exampleTok `shouldSatisfy` (\a -> on (||) (a ==) "heads" "tails")

--shouldSatisfyIO = on shouldSatisfy testIO

exampleTok :: RandTok
exampleTok = List [(1.0,List [(0.5,Value "heads"),(0.5,Value "tails")])]

madFile :: T.Text
madFile = ":define something\n    0.5 \"heads\"\n    0.5 \"tails\"\n:return\n    1.0 something"

madFileFailure :: T.Text
madFileFailure = ":define something\n    0.5 \"heads\"\n    0.5 \"tails\n:return\n    1.0 something"

madComplexFile :: T.Text
madComplexFile = ":define person\
\    0.7 \"I will \"\
\    0.3 \"You will \"\
\:define goodfortune\
\    0.1 person \"make nice things happen today\"\
\    0.9 \"nice things will happen today\"\
\:define fortune\
\    0.5 \"drink a boatload of milk\"\
\    0.5 \"get heckin angry\"\
\:return\
\    0.8 person fortune\
\    0.2 goodfortune"
