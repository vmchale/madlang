-- | Module containing functions to get `Text` from `RandTok`
module Text.Madlibs.Cata.Run ( run
                             ) where

import           Control.Monad.Random.Class
import qualified Data.Text                   as T
import           Text.Madlibs.Internal.Types
import           Text.Madlibs.Internal.Utils

-- | Generate randomized text from a `RandTok`
--
-- @
-- getText :: IO T.Text
-- getText = do
--     let exampleTok = List [(1.0,List [(0.5,Value "heads"),(0.5,Value "tails")])]
--     run exampleTok
-- @
run :: (MonadRandom m) => RandTok -> m T.Text
run tok@List{} = do
    value <- getRandomR (0,1)
    let ret = ((snd . head) . filter ((>=value) . fst)) $ mkCdf tok
    case ret of
        (Value txt)    -> pure txt
        tokNest@List{} -> run tokNest
run (Value txt) = pure txt

-- | Helper function to compute the cdf when we have a pdf
mkCdf :: RandTok -> [(Prob, RandTok)]
mkCdf (List rs) = zip (cdf . fmap fst $ rs) (fmap snd rs)
mkCdf v@Value{} = [(1, v)]
