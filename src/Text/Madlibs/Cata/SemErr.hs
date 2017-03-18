-- | Module defining the SemErr data type
module Text.Madlibs.Cata.SemErr where

import Text.Madlibs.Internal.Types
import Data.Typeable
import Text.PrettyPrint.ANSI.Leijen
import Control.Exception
import qualified Data.Text as T

-- | Datatype for a semantic error
data SemanticError = OverloadedReturns | CircularFunctionCalls T.Text T.Text | ProbSum T.Text | InsufficientArgs Int Int
    deriving (Typeable)

--also consider overloading parseError tbqh
-- | display a `SemanticError` nicely with coloration & whatnot
instance Show SemanticError where
    show OverloadedReturns = show $ semErrStart <> text "File contains multiple declarations of :return"
    show (CircularFunctionCalls f1 f2) = show $ semErrStart <> text "Circular function declaration between:" <> indent 4 (yellow $ (text' f1) <> (text ", ") <> (text' f2))
    show (InsufficientArgs i j) = show $ semErrStart <> text "Insufficent arguments from the command line, given " <> (text . show $ i) <> ", expected at least " <> (text . show $ j)
    show (ProbSum f) = show $ semErrStart <> text "Function's options do not sum to 1:\n" <> indent 4 (yellow (text' f))
    --we probably want to do our instance of `Show` for `ParseError` since that will let us color the position nicely @ least

-- | Constant to start `SemanticError`s
semErrStart :: Doc
semErrStart = dullred (text "\n  Semantic Error: ")

-- | Convert a `Text` to a `Doc` for use with a pretty-printer
text' :: T.Text -> Doc
text' = text . T.unpack

-- | derived exception instance
instance Exception SemanticError

-- | big semantics checker that sequences stuff
checkSemantics :: [(Key, [(Prob, [PreTok])])] -> [(Key, [(Prob, [PreTok])])]
checkSemantics = foldr (.) id [ checkProb
                              , checkReturn ]

-- checker to verify we have the right number of command-line args
-- checkArgs :: [(Key, [(Prob, [PreTok])])] -> [(Key, [(Prob, [PreTok])])]

-- | checker to verify probabilities sum to 1
checkProb :: [(Key, [(Prob, [PreTok])])] -> [(Key, [(Prob, [PreTok])])]
checkProb = map (\(i,j) -> if sumProb j then (i,j) else throw (ProbSum i))
--potentially consider throwing mult. errors at once obvi

-- | helper to filter out stuff that doesn't
sumProb :: [(Prob, [PreTok])] -> Bool
sumProb = ((==1) . sum . (map fst))
--check for approximation too

-- | Take the head of the list, or throw the appropriate error given which functions we are trying to call.
head' :: T.Text -> T.Text -> [a] -> a
head' _ _ (x:xs) = x
head' f1 f2 _ = throw (CircularFunctionCalls f1 f2)

access :: [a] -> Int -> a
access xs i = if (i >= length xs) then throw (InsufficientArgs (length xs) (i+1)) else xs !! i

-- | checker to verify there is at most one `:return` statement
checkReturn :: [(Key, [(Prob, [PreTok])])] -> [(Key, [(Prob, [PreTok])])]
checkReturn keys
    | singleReturn keys = keys
    | otherwise = throw OverloadedReturns

-- | Checks that we have at most one `:return` template in the file
singleReturn :: [(Key, [(Prob, [PreTok])])] -> Bool
singleReturn = singleton . (filter ((=="Template") . fst))
    where singleton = not . null
