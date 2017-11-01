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
import           Data.Maybe                  (catMaybes)
import           Data.Monoid
import qualified Data.Set                    as S
import qualified Data.Text                   as T
import           Data.Text.Titlecase
import           System.Random.Shuffle
import           Text.Madlibs.Cata.SemErr
import           Text.Madlibs.Internal.Types
import           Text.Madlibs.Internal.Utils

-- | A map with all the modifiers for Madlang
modifierList :: M.Map String (T.Text -> T.Text)
modifierList = M.fromList [("to_upper", T.map toUpper)
    , ("to_lower", T.map toLower)
    , ("capitalize", \t -> toUpper (T.head t) `T.cons` T.tail t)
    , ("reverse", T.reverse)
    , ("titlecase", T.pack . titlecase . T.unpack) -- FIXME this improperly trims spaces at beginning/end of a word
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
sortKeys = sortBy =<< orderKeys

orderHelper :: Key -> [(Prob, [PreTok])] -> Bool
orderHelper key = any (\pair -> key /= "" && key `elem` (map unTok . snd $ pair))

maybeList :: Maybe [a] -> [a]
maybeList (Just x) = x
maybeList Nothing  = []

allDeps :: M.Map Key [(Prob, [PreTok])] -> Key -> S.Set Key
allDeps context key = let deps = (maybeList . fmap (catMaybes . fmap maybeName) . getNames) context in S.fromList (deps <> (S.toList . allDeps context =<< deps))
    where getNames = fmap ((=<<) snd) . M.lookup key
          maybeName (Name n _) = Just n
          maybeName _          = Nothing

-- | Ordering on the keys to account for dependency
orderKeys :: [(Key, [(Prob, [PreTok])])] -> (Key, [(Prob, [PreTok])]) -> (Key, [(Prob, [PreTok])]) -> Ordering
orderKeys context (key1, l1) (key2, l2)
    | key1 == "Return" = GT
    | key2 == "Return" = LT
    | orderHelper key1 l2 = LT
    | orderHelper key2 l1 = GT
    | key2 `S.member` allDeps (M.fromList context) key1 = GT
    | key1 `S.member` allDeps (M.fromList context) key2 = LT
    | otherwise = EQ
