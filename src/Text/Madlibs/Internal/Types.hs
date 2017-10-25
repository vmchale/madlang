{-# LANGUAGE DeriveFoldable       #-}
{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE DeriveLift           #-}
{-# LANGUAGE DeriveTraversable    #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- | Module with the type of a random token
module Text.Madlibs.Internal.Types where

import           Control.Arrow              (second)
import           Control.Monad.State
import           Data.Function
import           Data.Functor.Foldable.TH   (makeBaseFunctor)
import           Data.Monoid
import qualified Data.Text                  as T
import           Instances.TH.Lift          ()
import           Language.Haskell.TH.Syntax (Lift (..))

-- | datatype for a double representing a probability
type Prob = Double

-- | dataype for a key aka token name
type Key = T.Text

-- | Pretoken, aka token as first read in
data PreTok = Name T.Text (T.Text -> T.Text) | PreTok T.Text

instance Show PreTok where
    show (Name t _) = show ("lambda: " <> t)
    show (PreTok t) = show t

instance Eq PreTok where
    (==) (Name a1 f1) (Name a2 f2) = a1 == a2 && (f1 . T.pack $ ['a'..'z']) == (f2 . T.pack $ ['a'..'z'])
    (==) (PreTok a) (PreTok b) = a == b
    (==) _ _ = False

-- | datatype for a token returning a random string
data RandTok = List [(Prob, RandTok)] | Value T.Text
    deriving (Show, Eq, Lift)

apply :: (T.Text -> T.Text) -> RandTok -> RandTok -- TODO make a base functor so we can map f over stuff?
apply f (Value str) = Value (f str)
apply f (List l)    = List $ fmap (second (apply f)) l

-- | Make `RandTok` a monoid so we can append them together nicely (since they do generate text).
--
-- > (Value "Hello") <> (List [(0.5," you"), (0.5, " me")])
-- > (List [(0.5,"Hello you"), (0.5, "Hello me")])
instance Monoid RandTok where
    mempty = Value ""
    mappend (Value v1) (Value v2) = Value (T.append v1 v2)
    mappend (List l1) v@Value{} = List $ fmap (second (`mappend` v)) l1
    mappend v@Value{} (List l2) = List $ fmap (second (mappend v)) l2
    mappend l@List{} (List l2) = List [ (p, l `mappend` tok) | (p,tok) <- l2 ]

-- TODO make this a map instead of keys for faster parse.
-- | State monad providing context, i.e. function we've already called before
type Context a = State [(Key, RandTok)] a

-- | Compare inside the state monad using only the underlying objects
instance (Eq a) => Eq (Context a) where
    (==) = on (==) (flip evalState [])

makeBaseFunctor ''RandTok
