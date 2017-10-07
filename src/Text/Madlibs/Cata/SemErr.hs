{-# LANGUAGE FlexibleContexts #-}

-- | Module defining the SemErr data type
module Text.Madlibs.Cata.SemErr (
    SemanticError (..)
  , Parser
  , access
  , checkSemantics
  , head'
  , headNoReturn ) where


import           Control.Exception
import           Control.Monad
import qualified Data.Text                    as T
import           Data.Typeable
import           Data.Void
import           Text.Madlibs.Internal.Types
import           Text.Megaparsec
import           Text.PrettyPrint.ANSI.Leijen

type Parser = Parsec (ErrorFancy Void) T.Text

-- | Datatype for a semantic error
data SemanticError = NoReturn | CircularFunctionCalls T.Text T.Text | InsufficientArgs Int Int | DoubleDefinition T.Text | NoContext T.Text
    deriving (Typeable)

-- | display a `SemanticError` nicely with coloration & whatnot
instance Show SemanticError where
    show (DoubleDefinition f) = show $ semErrStart <> text "File contains two declarations of:" <> indent 4 (yellow (text' f))
    show NoReturn = show $ semErrStart <> text "File must contain exactly one declaration of :return"
    show (NoContext f1) = show $ semErrStart <> text "Call in function: " <> indent 4 (yellow (text' f1)) <> "which is not in scope"
    show (CircularFunctionCalls f1 f2) = show $ semErrStart <> text "Function" </> indent 4 (yellow (text' f2)) <> text' " refers to a function" </> indent 4 (yellow (text' f1)) <> text' ", which is not in scope." </> indent 2 (text' "This may be due to a circular function dependecy.")
    show (InsufficientArgs i j) = show $ semErrStart <> text "Insufficent arguments from the command line; given " <> (text . show $ i) <> ", expected at least " <> (text . show $ j)

-- | Derived via our show instance;
instance Exception SemanticError where

-- | Throw custom error given by string, within the parser
customError :: String -> Parser a
customError = fail

showCustomError :: (Show a) => a -> Parser b
showCustomError = customError . show

-- | Throw `NoReturn` error within parser
noReturn :: Parser a
noReturn = showCustomError NoReturn

-- | Throws error when a function is defined twice
doubleDefinition :: T.Text -> Parser a
doubleDefinition f = showCustomError $ DoubleDefinition f

-- | Constant to start `SemanticError`s
semErrStart :: Doc
semErrStart = dullred (text "\n  Semantic Error: ")

-- | Convert a `Text` to a `Doc` for use with a pretty-printer
text' :: T.Text -> Doc
text' = text . T.unpack

--do we need this all in a monad??
-- | big semantics checker that sequences stuff
checkSemantics :: [(Key, [(Prob, [PreTok])])] -> Parser [(Key, [(Prob, [PreTok])])]
checkSemantics keys = foldr (<=<) pure ((checkKey "Return"):[checkKey key | key <- allKeys keys ]) keys
    where allKeys = fmap name . (concatMap snd) . (concatMap snd)--traversal?
          name (Name str _) = str
          name (PreTok _)   = "Return"

-- | Take the head of the list, or throw the appropriate error given which functions we are trying to call.
head' :: T.Text -> T.Text -> [a] -> a
head' _ _ (x:_) = x
head' f1 f2 _   = throw (CircularFunctionCalls f1 f2)

headNoReturn :: [a] -> a
headNoReturn (x:_) = x
headNoReturn _     = throw NoReturn

-- | Access argument, or throw error if the list is too short.
access :: [a] -> Int -> a
access xs i = if (i >= length xs) then throw (InsufficientArgs (length xs) (i+1)) else xs !! i

-- | checker to verify there is at most one @:return@ or @:define key@ statement
checkKey :: Key -> [(Key, [(Prob, [PreTok])])] -> Parser [(Key, [(Prob, [PreTok])])]
checkKey key keys
    | singleInstance key keys = pure keys
    | noInstance key keys = pure keys -- noContext key -- FIXME only if it recurses properly!
    | key == "Return" && noInstance key keys = noReturn
    | otherwise = doubleDefinition key

-- | Checks that we have at most one `:return` template in the file
singleInstance :: Key -> [(Key, [(Prob, [PreTok])])] -> Bool
singleInstance key = singleton . (filter ((==key) . fst))
    where singleton [_] = True
          singleton _   = False

-- | Checks that there are no instances of a key
noInstance :: Key -> [(Key, [(Prob, [PreTok])])] -> Bool
noInstance key = not . any ((== key) . fst)
