module Main where

import Criterion.Main
import Text.Megaparsec
import Text.Madlibs
import qualified Data.Text.IO as TIO

fun = parse (parseTokM []) ""

main = do
    file <- TIO.readFile "test/templates/fortune-teller.mad"
    file2 <- TIO.readFile "bench/templates/magical-realism.mad"
    defaultMain [ bgroup "parseFile"
                      [ bench "fortune-teller" $ whnf fun file 
                      , bench "magical-realism" $ whnf fun file2 ]
                ]
