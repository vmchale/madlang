{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Main where

import           Criterion.Main
import qualified Data.Text       as T
import qualified Data.Text.IO    as TIO
import           Text.Madlibs
import           Text.Megaparsec

fun = parse (parseTokM []) ""

demo = $(madFile "demo/shakespeare.mad")

demoQQ = [madlang|
:define something
    1.0 "hello"
    1.0 "goodbye"
:return
    1.0 something|]

runTestQQ :: IO T.Text
runTestQQ = run demoQQ

runTest :: IO T.Text
runTest = run demo

main = do
    file <- TIO.readFile "test/templates/fortune-teller.mad"
    file2 <- TIO.readFile "demo/shakespeare.mad"
    defaultMain [ bgroup "parseTok"
                      [ bench "fortune-teller" $ whnf fun file
                      , bench "shakespeare" $ whnf fun file2 ]
                , bgroup "run"
                      [ bench "shakespeare" $ nfIO runTest
                      , bench "shakespeare-qq" $ nfIO runTest ]
                ]
