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
import qualified Data.Text.IO as TIO
import System.IO.Unsafe

main :: IO ()
main = hspec $ do
    describe "parseTok" $ do
        parallel $ it "parses a .mad string" $ do
            file <- madFile
            parseTok "" [] [] file `shouldParse` (List [(1.0,List [(0.5,Value "heads"),(0.5,Value "tails")])])
        parallel $ it "fails when quotes aren't closed" $ do
            file <- madFileFailure
            parseTok "" [] [] `shouldFailOn` file
        parallel $ it "parses when functions are out of order" $ do
            file <- madComplexFile
            parseTok "" [] [] `shouldSucceedOn` file
        parallel $ it "returns a correct string from the template when evaluating a token" $ do
            (run) exampleTok >>= (`shouldSatisfy` (\a -> on (||) (a ==) "heads" "tails"))
        parallel $ it "throws exception when two `:return`s are declared" $ do
            file <- semErrFile
            (parseTok "" [] [] `shouldFailOn` file) 
        parallel $ it "substitutes a variable correctly" $ do
            file <- madVar
            parseTok "" [] ["maxine"] `shouldSucceedOn` file
        parallel $ it "fails when variables are not passed in" $ do
            file <- madVar
            (parseTok "" [] [] `shouldFailOn` file) `shouldThrow` anyException
        --parallel $ it "parses tibetan numerals" $ do
        --    file <- madFileTibetan
        --    parseTok "" [] file `shouldParse` (List [(1.0,List [(0.5,Value "heads"),(0.5,Value "tails")])])
    describe "runFile" $ do
        it "parses file with inclusions" $ do
            runFile [] "test/templates/include.mad" >>= (`shouldSatisfy` (\a -> any (a==) ["heads","tails","on its side"]))
        it "runs on a file out of order" $ do
            runFile [] "test/templates/ordered.mad" >>= (`shouldSatisfy` (\a -> any (a==) ["heads","tails","one","two","three","thid"]))

semErr :: Selector SemanticError
semErr = const True

-- | Read a file in as a `Text`
readFile' :: FilePath -> IO T.Text
readFile' = (fmap T.pack) . readFile

exampleTok :: RandTok
exampleTok = List [(1.0,List [(0.5,Value "heads"),(0.5,Value "tails")])]

includeFile :: IO T.Text
includeFile = readFile' "test/templates/include.mad"

madFile :: IO T.Text
madFile = readFile' "test/templates/gambling.mad" 

madFileTibetan :: IO T.Text
madFileTibetan = readFile' "test/templates/ཤོ.mad"

madFileFailure :: IO T.Text
madFileFailure = readFile' "test/templates/err/bad.mad"

madComplexFile :: IO T.Text
madComplexFile = readFile' "test/templates/ordered.mad"

semErrFile :: IO T.Text
semErrFile = readFile' "test/templates/err/sem-err.mad"

madVar :: IO T.Text
madVar = readFile' "test/templates/var.mad"
