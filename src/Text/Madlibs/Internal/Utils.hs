{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

-- | Internal utils to help out elsewhere
module Text.Madlibs.Internal.Utils where

import Text.Madlibs.Internal.Types
import Text.Megaparsec.Text
import Text.Megaparsec.Error
import qualified Data.Text as T
import System.IO.Unsafe

-- | Function to apply a value on both arguments, e.g.
--
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

-- | Show as a T.Text
show' :: (Show a) => a -> T.Text
show' = (T.drop 1) . T.init . T.pack . show

-- | Pretty-print a ParseError
parseErrorPretty' :: ParseError Char Dec -> T.Text
parseErrorPretty' = T.pack . parseErrorPretty

-- | Strip a pre-token's name
unTok :: PreTok -> T.Text
unTok (PreTok txt) = ""
unTok (Name txt) = txt

-- | Read a file in as a `Text`
readFile' :: FilePath -> IO T.Text
readFile' = (fmap T.pack) . readFile
