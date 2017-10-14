{-# LANGUAGE OverloadedStrings #-}

import           Data.Function
import qualified Data.Text             as T
import           Demo
import           Test.Hspec
import           Test.Hspec.Megaparsec
import           Text.Madlibs

main :: IO ()
main = hspec $ do
    describe "parseTok" $ do
        parallel $ it "parses a .mad string with modifiers" $ do
            file <- madFileBasic
            parseTok "" [] [] file `shouldParse` List [(1.0,List [(0.5,Value "HEADS"),(0.5,Value "tails")])]
        parallel $ it "fails when quotes aren't closed" $ do
            file <- madFileFailure
            parseTok "" [] [] `shouldFailOn` file
        parallel $ it "fails on nonsensical modifiers" $ do
            file <- readFile' "test/templates/err/bad-modifier.mad"
            parseTok "" [] [] `shouldFailOn` file
        parallel $ it "parses when functions are out of order" $ do
            file <- madComplexFile
            parseTok "" [] [] `shouldSucceedOn` file
        parallel $ it "parses a function with defined categories" $ do
            file <- readFile' "test/templates/cat.mad"
            parseTok "" [] [] `shouldSucceedOn` file
        parallel $ it "returns a correct string from the template when evaluating a token" $
            run exampleTok >>= (`shouldSatisfy` (\a -> on (||) (a ==) "heads" "tails"))
        parallel $ it "throws exception when two `:return`s are declared" $ do
            file <- semErrFile
            parseTok "" [] [] `shouldFailOn` file
        parallel $ it "substitutes a variable correctly" $ do
            file <- madVar
            parseTok "" [] ["maxine"] `shouldSucceedOn` file
        parallel $ it "fails when variables are not passed in" $ do
            file <- madVar
            parseTok "" [] [] `shouldFailOn` file `shouldThrow` anyException
    describe "runFile" $ do
        parallel $ it "parses nested modifiers and modifiers on variables correctly" $ \_ ->
            runFile ["aa"] "test/templates/modifiers.mad" >>= (`shouldSatisfy` (\a -> elem a ["AAAaaa","AAAaa","AAaaa","AAaa"]))
        parallel $ it "parses file with inclusions and modifiers on functions" $ \_ ->
            runFile [] "test/templates/include.mad" >>= (`shouldSatisfy` (\a -> elem a ["heads","tails","on its side"]))
        parallel $ it "parses file with recursive inclusions" $ \_ ->
            runFile [] "test/templates/include-recursive.mad" >>= (`shouldSatisfy` (\a -> elem a ["HEADS","tails","on its side"]))
        parallel $ it "runs on a file out of order" $ \_ ->
            runFile [] "test/templates/ordered.mad" >>= (`shouldSatisfy` (\a -> elem a ["heads","tails","one","two","three","third","fourth"]))
    describe "readFileQ" $
        parallel $ it "executes embedded code" $
        runTest >>= (`shouldSatisfy` (\a -> any (a==) ["HEADS","tails"]))
    describe "madlang" $
        parallel $ it "provides a quasi-quoter" $
        runTestQQ >>= (`shouldSatisfy` (\a -> any (a==) ["hello","goodbye"]))

-- | Read a file in as a `Text`
readFile' :: FilePath -> IO T.Text
readFile' = fmap T.pack . readFile

exampleTok :: RandTok
exampleTok = List [(1.0,List [(0.5,Value "heads"),(0.5,Value "tails")])]

--includeFile :: IO T.Text
--includeFile = readFile' "test/templates/include.mad"

madFileBasic :: IO T.Text
madFileBasic = readFile' "test/templates/gambling.mad"

madFileFailure :: IO T.Text
madFileFailure = readFile' "test/templates/err/bad.mad"

madComplexFile :: IO T.Text
madComplexFile = readFile' "test/templates/ordered.mad"

semErrFile :: IO T.Text
semErrFile = readFile' "test/templates/err/sem-err.mad"

madVar :: IO T.Text
madVar = readFile' "test/templates/var.mad"
