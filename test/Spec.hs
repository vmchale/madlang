{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec
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
            let built = snd . head . (filter (\(i,j) -> i == "Template")) . (flip execState []) <$> runParser parseTok "" madFile
            built `shouldBe` Right (List [(1.0,List [(0.5,Value "heads"),(0.5,Value "tails")])])

madFile :: T.Text
madFile = ":define something\n    0.5 \"heads\"\n    0.5 \"tails\"\n:return\n    1.0 something"
