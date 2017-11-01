{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- | Internal utils to help out elsewhere
module Text.Madlibs.Internal.Utils where

import           Control.Arrow               (first)
import           Control.Exception           (IOException, catch, throw)
import qualified Data.Text                   as T
import           Data.Void
import           Text.Madlibs.Cata.SemErr
import           Text.Madlibs.Internal.Types
import           Text.Megaparsec.Error

-- | Drop file Extension
dropExtension :: FilePath -> FilePath
dropExtension = reverse . drop 1 . dropWhile (/='.') . reverse

-- | Get directory associated to a file
getDir :: FilePath -> FilePath
getDir = reverse . dropWhile (/='/') . reverse

-- | Function to apply a value on both arguments, e.g.
--
-- > between .$ (char '"')
(.$) :: (a -> a -> b) -> a -> b
(.$) f x = f x x

-- | Normalize pre-tokens/corresponding probabilities
normalize :: [(Prob, [PreTok])] -> [(Prob, [PreTok])]
normalize list = fmap (first (/total)) list
    where total = sum . fmap fst $ list

-- | Helper function for creating a cdf from a pdf
cdf :: [Prob] -> [Prob]
cdf = drop 2 . scanl (+) 0 . (:) 0

-- | Show as a T.Text
show' :: (Show a) => a -> T.Text
show' = T.drop 1 . T.init . T.pack . show

-- | Pretty-print a ParseError
parseErrorPretty' :: ParseError Char (ErrorFancy Void) -> T.Text
parseErrorPretty' = T.pack . parseErrorPretty

-- | Strip a pre-token's name
unTok :: PreTok -> T.Text
unTok PreTok{}     = ""
unTok (Name txt _) = txt

readLibFile :: FilePath -> IO T.Text
readLibFile path = catch (fmap T.pack . readFile $ path) (throw (ImportNotFound path) :: IOException -> IO T.Text)

-- | Read a file in as a `Text`
readFile' :: FilePath -> IO T.Text
readFile' = fmap T.pack . readFile
