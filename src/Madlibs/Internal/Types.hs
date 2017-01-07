-- | Module with the type of a random token
module Madlibs.Internal.Types where

import qualified Data.Text as T

-- | datatype for a double representing a probability
type Prob = Double

-- | dataype for a key aka token name
type Key = T.Text

-- | Pretoken, aka token as first read in
data PreTok = Name String | PreTok T.Text 
    deriving (Show)

-- | datatype for a token returning a random string
-- maybe make it work on more than just T.Text? and add a functor instance idk
data RandTok = List [(Prob, RandTok)] | Value T.Text 
    deriving (Show)
