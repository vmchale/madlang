{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}

module Demo (
    runTest
  , runTestQQ ) where

import qualified Data.Text    as T
import           Text.Madlibs

demo :: RandTok
demo = $(madFile "test/templates/include-recursive.mad")

-- demoDir :: [(Key, RandTok)]
-- demoDir = $(madEmbed "test/templates" "gambling.mad")

demoQQ :: RandTok
demoQQ = [madlang|
:define something
    1.0 "hello"
    1.0 "goodbye"
:return
    1.0 something
|]

runTest :: IO T.Text
runTest = run demo

runTestQQ :: IO T.Text
runTestQQ = run demoQQ
