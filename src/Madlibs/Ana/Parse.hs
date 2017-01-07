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

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

spaceConsumer :: Parser ()
spaceConsumer = L.space (void . some $ spaceChar) (L.skipLineComment "#") (L.skipBlockComment "{#" "#}")

symbol :: String -> Parser String
symbol = L.symbol spaceConsumer

float :: Parser Double
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
preStr = (fmap Name name) <|>
    do {
    s <- quote (many $ (alphaNumChar <|> char ' ')) ;
    --should be any char except '"'
    pure $ PreTok . T.pack $ s
    } 
    <?> "string or function name"

pair :: Parser (Double, [PreTok])
pair = do
    --indentGuard
    p <- float
    str <- some $ preStr
    pure (p, str)

definition :: Parser (Key, [(Double, [PreTok])])
definition = do
    define
    str <- name
    val <- some pair
    --linebreak
    pure (T.pack str, val)

final :: Parser [(Double, [PreTok])]
final = do
    main
    val <- some pair
    pure val

program :: Parser [(Key, [(Double, [PreTok])])]
program = do
    p <- many (try definition <|> ((,) "Template" <$> final))
    pure p
