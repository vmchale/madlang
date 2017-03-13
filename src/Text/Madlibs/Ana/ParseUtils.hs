-- | Helper functions to sort out parsing
module Text.Madlibs.Ana.ParseUtils where

import Text.Madlibs.Internal.Types
import Text.Madlibs.Internal.Utils
import Text.Madlibs.Cata.SemErr
import Data.List
import qualified Data.Text as T
import Control.Monad.State
import Data.Foldable
import Control.Exception
import Control.Arrow

--consider moving Ana.ParseUtils to Cata.Sorting

-- | Convert the stuff after the number to a `RandTok`
concatTok :: T.Text -> Context [PreTok] -> Context RandTok
concatTok param pretoks = do
    ctx <- get
    let unList (List a) = a
    let toRand (Name str) = List . snd . (head' str param) . (filter ((== str) . fst)). (map (second unList)) $ ctx
        toRand (PreTok txt) = Value txt
    fold . (map toRand) <$> pretoks

-- | Given keys naming the tokens, and lists of `PreTok`, build our `RandTok`
build :: [(Key, [(Prob, [PreTok])])] -> Context RandTok
build list
    | length list == 1 = do
        let [(key, pairs)] = list
        toks <- mapM (\(i,j) -> concatTok key (pure j)) pairs
        let probs = map fst pairs
        let tok = List $ zip probs toks
        state (\s -> (tok,((key, tok):s)))
        --should do: recurse or take "Template" key
    | otherwise = do
        let (x:xs) = list
        y <- (build [x])
        ys <- pure <$> build xs
        pure $ fold (y:ys)

-- | Sort the keys that we have parsed so that dependencies are in the correct places
sortKeys :: [(Key, [(Prob, [PreTok])])] -> [(Key, [(Prob, [PreTok])])]
sortKeys = sortBy orderKeys

-- | Ordering on the keys to account for dependency
orderKeys :: (Key, [(Prob, [PreTok])]) -> (Key, [(Prob, [PreTok])]) -> Ordering
orderKeys (key1, l1) (key2, l2)
    | key1 == "Template" = GT
    | key2 == "Template" = LT
    | any (\pair -> any (T.isInfixOf key1) (map unTok . snd $ pair)) l1 = LT
    | any (\pair -> any (T.isInfixOf key2) (map unTok . snd $ pair)) l1 = GT
    -- issue: if we define nationality, subject object NOW object can't be used in subject?
    | otherwise = EQ
