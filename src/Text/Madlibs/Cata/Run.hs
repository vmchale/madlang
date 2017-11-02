-- | Module containing functions to get `Text` from `RandTok`
module Text.Madlibs.Cata.Run ( run
                             , runCata ) where

import           Control.Monad.Random.Class
import           Data.Functor.Foldable.Exotic (cataM)
import qualified Data.Text                    as T
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

-- | Same thing, but uses a catamorphism (slower)
runCata :: (MonadRandom m) => RandTok -> m T.Text
runCata = cataM alg where
    alg (ListF tok) = do
        value <- getRandomR (0,1)
        pure $ ((snd . head) . filter ((>= value) . fst)) $ mkCdfCata tok
    alg (ValueF txt) = pure txt

-- | Helper function to compute the cdf when we have a pdf
mkCdf :: RandTok -> [(Prob, RandTok)]
mkCdf (List rs) = zip (cdf . fmap fst $ rs) (fmap snd rs)
mkCdf v@Value{} = [(1, v)]

-- | Another helper function, this time for use with our catamorphism.
mkCdfCata :: [(Prob, T.Text)] -> [(Prob, T.Text)]
mkCdfCata rs = zip (cdf . fmap fst $ rs) (fmap snd rs)
