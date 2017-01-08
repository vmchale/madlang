{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

-- | Module with the type of a random token
module Madlibs.Internal.Types where

import qualified Data.Text as T
import Control.Monad.State
import Data.Functor.Identity
import Control.Lens hiding (List, Context)
import Data.Function

-- | datatype for a double representing a probability
type Prob = Double

-- | dataype for a key aka token name
type Key = T.Text

-- | Pretoken, aka token as first read in
data PreTok = Name T.Text | PreTok T.Text 
    deriving (Show)

-- | datatype for a token returning a random string
-- maybe make it work on more than just T.Text? and add a functor instance idk
data RandTok = List [(Prob, RandTok)] | Value T.Text 
    deriving (Show, Eq)

instance Monoid RandTok where
    mempty = Value ""
    mappend (Value v1) (Value v2) = Value (T.append v1 v2)
    mappend (List l1) v@(Value v1) = List $ map (over (_2) (`mappend` v)) l1 
    mappend v@(Value v2) (List l2) = List $ map (over (_2) (`mappend` v)) l2
    mappend l@(List l1) (List l2) = List $ [ (p, l `mappend` tok) | (p,tok) <- l2 ]

type Context a = State [(Key, RandTok)] a

instance (Eq a) => Eq (Context a) where
    (==) a b = (on (==) (flip evalState [])) a b
