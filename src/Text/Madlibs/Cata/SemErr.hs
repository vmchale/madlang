-- | Module defining the SemErr data type
module Text.Madlibs.Cata.SemErr where

import Text.Madlibs.Internal.Types
import Data.Typeable
import Text.PrettyPrint.ANSI.Leijen
import Control.Exception
import qualified Data.Text as T
import Control.Monad
import qualified Data.Set as S
import Text.Megaparsec.Text
import Text.Megaparsec.Prim
import Text.Megaparsec.Error

-- | Datatype for a semantic error
data SemanticError = OverloadedReturns | CircularFunctionCalls T.Text T.Text | InsufficientArgs Int Int
    deriving (Typeable)

-- | display a `SemanticError` nicely with coloration & whatnot
instance Show SemanticError where
    show OverloadedReturns = show $ semErrStart <> text "File contains multiple declarations of :return"
    show (CircularFunctionCalls f1 f2) = show $ semErrStart <> text "Circular function declaration between:" <> indent 4 (yellow $ (text' f1) <> (text ", ") <> (text' f2))
    show (InsufficientArgs i j) = show $ semErrStart <> text "Insufficent arguments from the command line, given " <> (text . show $ i) <> ", expected at least " <> (text . show $ j)

-- | Derived via our show instance;
instance Exception SemanticError where

-- | Throw custom error given by string, within the parser
customError :: String -> Parser a
customError = failure S.empty S.empty . S.singleton . representFail

-- | Throw `OverloadedReturns` error within parser
overloadedReturns :: Parser a
overloadedReturns = customError . show $ OverloadedReturns

-- | Throws argument for circular function calls
circularFunctionCalls :: T.Text -> T.Text -> Parser a
circularFunctionCalls f1 f2 = customError . show $ CircularFunctionCalls f1 f2

-- | Throws error for insufficient arguments
insufficientArgs :: Int -> Int -> Parser a
insufficientArgs i j = customError . show $ InsufficientArgs i j

-- | Constant to start `SemanticError`s
semErrStart :: Doc
semErrStart = dullred (text "\n  Semantic Error: ")

-- | Convert a `Text` to a `Doc` for use with a pretty-printer
text' :: T.Text -> Doc
text' = text . T.unpack

-- | big semantics checker that sequences stuff
checkSemantics :: [(Key, [(Prob, [PreTok])])] -> Parser [(Key, [(Prob, [PreTok])])]
checkSemantics = foldr (<=<) pure [ checkReturn
                                  ]
-- | helper to filter out stuff that doesn't
sumProb :: [(Prob, [PreTok])] -> Bool
sumProb = ((==1) . sum . (map fst))
--check for approximation too

-- | Take the head of the list, or throw the appropriate error given which functions we are trying to call.
head' :: T.Text -> T.Text -> [a] -> a
head' _ _ (x:xs) = x
head' f1 f2 _ = throw (CircularFunctionCalls f1 f2)

-- | Access argument, or throw error if the list is too short. 
access :: [a] -> Int -> a
access xs i = if (i >= length xs) then throw (InsufficientArgs (length xs) (i+1)) else xs !! i

-- | checker to verify there is at most one `:return` statement
checkReturn :: [(Key, [(Prob, [PreTok])])] -> Parser [(Key, [(Prob, [PreTok])])]
checkReturn keys
    | singleReturn keys = pure keys
    | otherwise = overloadedReturns

-- | Checks that we have at most one `:return` template in the file
singleReturn :: [(Key, [(Prob, [PreTok])])] -> Bool
singleReturn = singleton . (filter ((=="Template") . fst))
    where singleton [a] = True
          singleton _   = False
