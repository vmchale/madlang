-- | Module with helper functions for displaying the parsed tree
module Text.Madlibs.Cata.Display (displayTree) where

import           Data.Functor.Foldable
import           Data.Tree
import           Text.Madlibs.Internal.Types

-- | Draw as a syntax Tree
displayTree :: RandTok -> String
displayTree = drawTree . tokToTree 1.0 . cleanTree

cleanTree :: RandTok -> RandTok
cleanTree = cata algebra
    where algebra (ValueF t)       = Value t
          algebra (ListF [(1, t)]) = t
          algebra (ListF x)        = List x

-- | Function to transform a `RandTok` into a `Tree String` so that it can be pretty-printed.
--
-- > tokToTree 1.0 tok
tokToTree :: Prob -> RandTok -> Tree String
tokToTree p (Value a) = Node ((take 4 . show . min 1.0) p ++ " " ++ show a) []
tokToTree p (List [(_,Value a)]) = Node ((take 4 . show . min 1.0) p ++ " " ++ show a) []
tokToTree p (List xs) = Node (take 4 . show . min 1.0 $ p) (map (uncurry tokToTree) xs)
