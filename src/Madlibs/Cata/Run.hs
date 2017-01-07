module Madlibs.Cata.Run where

import Madlibs.Internal.Types
import Madlibs.Internal.Utils
import qualified Data.Text as T
import System.Random.MWC

run :: RandTok -> IO T.Text
run tok@(List rs) = do
    value <- (withSystemRandom . asGenST $ \gen -> uniform gen)
    let ret = ((snd . head) . filter ((>=value) . fst)) $ mkCdf tok
    case ret of
        (Value txt) -> pure txt
        tok@(List rs) -> run tok
run (Value txt) = pure txt

mkCdf :: RandTok -> [(Double, RandTok)]
mkCdf (List rs) = zip (cdf . (map fst) $ rs) (map snd rs)

example :: RandTok
example = List [(0.3, (Value "fuck")), (0.7, List [(0.5, (Value "you")),(0.5, (Value "me"))])]

--also twitter-bot-generator maybe just like package it all together?
