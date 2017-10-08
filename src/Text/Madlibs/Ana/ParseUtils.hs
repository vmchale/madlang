{-# LANGUAGE OverloadedStrings #-}

-- | Helper functions to sort out parsing
module Text.Madlibs.Ana.ParseUtils (
    modifierList
  , strip
  , takeTemplate
  , sortKeys
  , build
  , buildTree
  , jumble
  ) where

import           Control.Arrow
import           Control.Monad.Random.Class
import           Control.Monad.State
import           Data.Char
import           Data.Foldable
import           Data.List
import qualified Data.Map                    as M
import qualified Data.Text                   as T
import           System.Random.Shuffle
import           Text.Madlibs.Cata.SemErr
import           Text.Madlibs.Internal.Types
import           Text.Madlibs.Internal.Utils

--TODO consider moving Ana.ParseUtils to Cata.Sorting

-- | A map with all the modifiers for Madlang
modifierList :: M.Map String (T.Text -> T.Text)
modifierList = M.fromList [("to_upper", T.map toUpper)
    , ("to_lower", T.map toLower)
    , ("capitalize", \t -> toUpper (T.head t) `T.cons` T.tail t)
    , ("reverse", T.reverse)
    , ("reverse_words", T.unwords . reverse . T.words)
    , ("oulipo", T.filter (/='e'))]

-- | Jumble the words in a string
jumble :: (MonadRandom m) => T.Text -> m T.Text
jumble = fmap (T.pack . unwords) . shuffleM . words . T.unpack

-- | Strip file extension
strip :: String -> T.Text
strip = T.pack . reverse . drop 4 . reverse

-- | Get the :return value
takeTemplate :: [(Key, RandTok)] -> RandTok
takeTemplate = snd . headNoReturn . filter (\(i,_) -> i == "Return")

-- | Convert the stuff after the number to a `RandTok`
concatTok :: T.Text -> Context [PreTok] -> Context RandTok
concatTok param pretoks = do
    ctx <- get
    let unList (List a) = a
        unList _        = mempty
    let toRand (Name str f) = apply f . List . snd . head' str param . filter ((== str) . fst) . fmap (second unList) $ ctx
        toRand (PreTok txt) = Value txt
    fold . fmap toRand <$> pretoks

-- | Build token in tree structure, without concatenating.
buildTok :: T.Text -> Context [PreTok] -> Context RandTok
buildTok param pretoks = do
    ctx <- get
    let unList (List a) = a
        unList _        = mempty
    let toRand (Name str f) = apply f . List . snd . head' str param . filter ((== str) . fst) . fmap (second unList) $ ctx
        toRand (PreTok txt) = Value txt
    List . zip [1..] . fmap toRand <$> pretoks

-- | Build the token without concatenating, yielding a `RandTok` suitable to be
-- printed as a tree.
buildTree :: [(Key, [(Prob, [PreTok])])] -> Context RandTok
buildTree [] = pure mempty
buildTree [(key,pairs)] = do
    toks <- mapM (\(_,j) -> buildTok key (pure j)) pairs
    let probs = fmap fst pairs
    let tok = List $ zip probs toks
    state (\s -> (tok,(key,tok):s))
buildTree (x:xs) = do
    y <- buildTree [x]
    ys <- pure <$> buildTree xs
    pure . List . zip [1..] $ (y:ys)

-- | Given keys naming the tokens, and lists of `PreTok`, build our `RandTok`
build :: [(Key, [(Prob, [PreTok])])] -> Context RandTok
build [] = pure mempty
build [(key,pairs)] = do
    toks <- mapM (\(_,j) -> concatTok key (pure j)) pairs
    let probs = fmap fst pairs
    let tok = List $ zip probs toks
    state (\s -> (tok,(key, tok):s))
build (x:xs) = do
    y <- build [x]
    ys <- pure <$> build xs
    pure $ fold (y:ys)

-- | Sort the keys that we have parsed so that dependencies are in the correct places
sortKeys :: [(Key, [(Prob, [PreTok])])] -> [(Key, [(Prob, [PreTok])])]
sortKeys = sortBy orderKeys

orderHelper :: Key -> [(Prob, [PreTok])] -> Bool
orderHelper key = any (\pair -> key /= "" && key `elem` (map unTok . snd $ pair))

hasNoDeps :: [(Prob, [PreTok])] -> Bool
hasNoDeps = all isPreTok . (>>= snd)
    where isPreTok PreTok{} = True
          isPreTok _        = False

-- FIXME if a depends on b depends on c, then we shouldn't consider a and c to be equal.
-- Consider some fancy morphism here too. (chronomorphism? - comonad to pop
-- values off, monad to store things.)

-- | Ordering on the keys to account for dependency
orderKeys :: (Key, [(Prob, [PreTok])]) -> (Key, [(Prob, [PreTok])]) -> Ordering
orderKeys (key1, l1) (key2, l2)
    | key1 == "Return" = GT
    | key2 == "Return" = LT
    | orderHelper key1 l2 = LT
    | orderHelper key2 l1 = GT
    | any (flip orderHelper l1) (flatten l2) = LT
    | any (flip orderHelper l2) (flatten l1) = GT
    | hasNoDeps l1 = LT
    | hasNoDeps l2 = GT
    | otherwise = EQ -- FIXME transitive dependencies

flatten :: [(Prob, [PreTok])] -> [Key]
flatten = (>>= (fmap unTok . snd))
