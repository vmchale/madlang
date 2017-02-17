{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Madlibs
import Text.Megaparsec
import Control.Monad.IO.Class
import Control.Monad.State
import Data.Function
import Control.Exception
import qualified Data.Text as T
import System.IO.Unsafe

main :: IO ()
main = hspec $ do
    describe "parseTok" $ do
        parallel$ it "parses a .mad string" $ do
            parseTok [] madFile `shouldParse` (List [(1.0,List [(0.5,Value "heads"),(0.5,Value "tails")])])
        parallel $ it "fails when quotes aren't closed" $ do
            parseTok [] `shouldFailOn` madFileFailure
        parallel $ it "parses when functions are out of order" $ do
            parseTok [] `shouldSucceedOn` madComplexFile
        parallel $ it "returns a correct string from the template when evaluating a token" $ do
            (testIO . run) exampleTok `shouldSatisfy` (\a -> on (||) (a ==) "heads" "tails")
        parallel $ it "throws exception when two `:return`s are declared" $ do
            (parseTok [] `shouldFailOn` semErrFile) `shouldThrow` semErr
        parallel $ it "substitutes a variable correctly" $ do
            parseTok ["maxine"] `shouldSucceedOn` madVar
        parallel $ it "fails when variables are not passed in" $ do
            (parseTok [] `shouldFailOn` madVar) `shouldThrow` anyException

semErr :: Selector SemanticError
semErr = const True

exampleTok :: RandTok
exampleTok = List [(1.0,List [(0.5,Value "heads"),(0.5,Value "tails")])]

madFile :: T.Text
madFile = ":define something\n    0.5 \"heads\"\n    0.5 \"tails\"\n:return\n    1.0 something"

madFileFailure :: T.Text
madFileFailure = ":define something\
\    0.5 \"heads\"\
\    0.5 \"tails\
\:return\
\    1.0 something"

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

semErrFile :: T.Text
semErrFile = ":define something\
\    0.5 \"heads\"\
\    0.5 \"tails\"\
\:return\
\    1.0 something\
\:return\
\    0.5 something\
\    0.5 \"parallel$ it doesn't matter b/c this is gonna blow up in our faces anyways\""

madVar :: T.Text
madVar = ":return\
\    1.0 $1 \" is a good doggo.\""

-- | for the testing framework; to 
testIO :: IO a -> a
testIO = unsafePerformIO
