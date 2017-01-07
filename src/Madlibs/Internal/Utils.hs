{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Madlibs.Internal.Utils where

import Madlibs.Internal.Types
import Text.Megaparsec.Text

-- | Function to apply a value on both arguments, e.g.
-- > between .$ (symbol "\'")
(.$) :: (a -> a -> b) -> a -> b
(.$) f x = f x x

-- | Add a PR for this? Could be useful in Megaparsec idk
-- Allows us to use monoidal addition on parsers
instance (Monoid a) => Monoid (Parser a) where
    mempty = pure mempty
    mappend x y = mappend <$> x <*> y

-- | Helper function for creating a cdf from a pdf
cdf :: [Prob] -> [Prob]
cdf = (drop 2) . (scanl (+) 0) . ((:) 0)
