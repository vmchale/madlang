-- | Module containing functions to get `Text` from `RandTok`
module Text.Madlibs.Cata.Run where

import Text.Madlibs.Internal.Types
import Text.Madlibs.Internal.Utils
import qualified Data.Text as T
import System.Random.MWC

-- | Generate randomized text from a `RandTok`
run :: RandTok -> IO T.Text
run tok@(List rs) = do
    value <-(withSystemRandom . asGenST $ \gen -> uniform gen)
    let ret = ((snd . head) . filter ((>=value) . fst)) $ mkCdf tok
    case ret of
        (Value txt) -> pure txt
        tok@(List rs) -> run tok
run (Value txt) = pure txt

-- | Helper function to compute the cdf when we have a pdf
mkCdf :: RandTok -> [(Double, RandTok)]
mkCdf (List rs) = zip (cdf . (map fst) $ rs) (map snd rs)
