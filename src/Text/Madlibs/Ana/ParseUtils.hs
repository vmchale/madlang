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

--TODO consider moving Ana.ParseUtils to Cata.Sorting

takeTemplate :: [(Key, RandTok)] -> RandTok
takeTemplate = snd . head . filter (\(i,j) -> i == "Template")

-- | Convert the stuff after the number to a `RandTok`
concatTok :: T.Text -> Context [PreTok] -> Context RandTok
concatTok param pretoks = do
    ctx <- get
    let unList (List a) = a
    let toRand (Name str) = List . snd . (head' str param) . (filter ((== str) . fst)) . (map (second unList)) $ ctx -- TODO move the shared functions to utils
        toRand (PreTok txt) = Value txt
    fold . (map toRand) <$> pretoks

-- | Build token in tree structure, without concatenating. 
buildTok :: T.Text -> Context [PreTok] -> Context RandTok
buildTok param pretoks = do
    ctx <- get
    let unList (List a) = a
    let toRand (Name str) = List . snd . (head' str param) . (filter ((==str) . fst)) . (map (second unList)) $ ctx
        toRand (PreTok txt) = Value txt
    List . zip ([1..]) . (map toRand) <$> pretoks

-- | Build the token without concatenating, yielding a `RandTok` suitable to be
-- printed as a tree. 
buildTree :: [(Key, [(Prob, [PreTok])])] -> Context RandTok
buildTree list@[(key,pairs)] = do
    toks <- mapM (\(i,j) -> buildTok key (pure j)) pairs
    let probs = map fst pairs
    let tok = List $ zip probs toks
    state (\s -> (tok,((key,tok):s)))
buildTree list@(x:xs) = do
    y <- buildTree [x]
    ys <- pure <$> buildTree xs
    pure . List . zip ([1..]) $ (y:ys)

-- | Given keys naming the tokens, and lists of `PreTok`, build our `RandTok`
build :: [(Key, [(Prob, [PreTok])])] -> Context RandTok
build list@[(key,pairs)] = do
    toks <- mapM (\(i,j) -> concatTok key (pure j)) pairs
    let probs = map fst pairs
    let tok = List $ zip probs toks
    state (\s -> (tok,((key, tok):s)))
build list@(x:xs) = do
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
