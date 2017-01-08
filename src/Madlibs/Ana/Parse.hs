-- | Parse our DSL
module Madlibs.Ana.Parse where

import Madlibs.Internal.Types
import Madlibs.Internal.Utils
import qualified Data.Text as T
import Text.Megaparsec
import Text.Megaparsec.Text
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Lexer as L
import Data.Monoid
import Control.Monad
import Control.Lens hiding (Context, List)
import Control.Monad.State
import Control.Monad.Reader
import Data.Composition
import Data.Foldable
import Data.Monoid
import Data.List

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

spaceConsumer :: Parser ()
spaceConsumer = L.space (void . some $ spaceChar) (L.skipLineComment "#") (L.skipBlockComment "{#" "#}")

symbol :: String -> Parser String
symbol = L.symbol spaceConsumer

float :: Parser Prob
float = lexeme L.float

nonIndented = L.nonIndented spaceConsumer

indentGuard = L.indentGuard spaceConsumer

--maybe make a Cata/Vim.hs to gen. syntax stuff?

quote :: Parser a -> Parser a
quote = between .$ (symbol "\"")

keyword :: String -> Parser String
keyword str = (pure <$> char ':') <> (symbol str) <?> "keyword"

define :: Parser ()
define = (void $ nonIndented (keyword "define"))
    <?> "define block"

main :: Parser ()
main = (void $ nonIndented (keyword "return"))
    <?> "return block"

name :: Parser String
name = lexeme (some letterChar) <?> "template name"

preStr :: Parser PreTok
preStr = (fmap (Name . T.pack) name) <|>
    do {
    s <- quote (many $ (alphaNumChar <|> char ' ')) ;
    --should be any char except '"'
    pure $ PreTok . T.pack $ s
    } 
    <?> "string or function name"

pair :: Parser (Prob, [PreTok])
pair = do
    --indentGuard
    p <- float
    str <- some $ preStr
    pure (p, str)

definition :: Parser (Key, [(Prob, [PreTok])])
definition = do
    define
    str <- name
    val <- some pair
    --linebreak
    pure (T.pack str, val)

final :: Parser [(Prob, [PreTok])]
final = do
    main
    val <- some pair
    pure val

program :: Parser [(Key, [(Prob, [PreTok])])]
program = sortKeys <$> do
    p <- many (try definition <|> ((,) "Template" <$> final))
    pure p

isVal :: (Foldable f) => f PreTok -> Bool
isVal = all f
    where f (PreTok txt) = True
          f (Name str) = False

--create an instance of (Ord a) for which definitions are dependent on one another?
concatTok :: Context [PreTok] -> Context RandTok
concatTok pretoks = do
    ctx <- get
    let unList (List a) = a
    let toRand (Name str) = List . snd . head . (filter ((== str) . fst)). (map (\(i,j) -> (i, unList j))) $ ctx
        toRand (PreTok txt) = Value txt
    fold . (map toRand) <$> pretoks
              --also make sure there aren't multiple definitions
              --make from two tokens: mulptiply probabilities, append in some sense?
              --((==) .* fst))

sortKeys :: [(Key, [(Prob, [PreTok])])] -> [(Key, [(Prob, [PreTok])])]
sortKeys = sortBy orderKeys

orderKeys :: (Key, [(Prob, [PreTok])]) -> (Key, [(Prob, [PreTok])]) -> Ordering
orderKeys (key1, l1) (key2, l2)
    | key1 == "Template" = GT
    | key2 == "Template" = LT
    | any (\pair -> any (T.isInfixOf key1) (map unTok . snd $ pair)) l1 = LT
    | any (\pair -> any (T.isInfixOf key2) (map unTok . snd $ pair)) l1 = GT
    | otherwise = EQ

unTok :: PreTok -> T.Text
unTok (PreTok txt) = ""
unTok (Name txt) = txt

build :: Context [(Key, [(Prob, [PreTok])])] -> Context RandTok
build ctxList
    | fmap length ctxList == pure 1 = do
        list <- ctxList
        let [(key, pairs)] = list
        toks <- sequence $ map (\(i,j) -> concatTok (pure j)) $ pairs
        let probs = map (fst) $ pairs
        let tok = List $ zip probs toks
        --env <- get
        --put ((key, tok):env)
        --pure tok
        state (\s -> (tok,((key, tok):s)))
        --should do: recurse or take "Template" key
    | otherwise = do
        list <- ctxList
        let (x:xs) = list
        y <- (build . pure $ [x])
        ys <- pure <$> (build . pure) xs
        pure $ fold (y:ys)
