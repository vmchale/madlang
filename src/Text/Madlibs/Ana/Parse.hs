-- | Parse our DSL
module Text.Madlibs.Ana.Parse where

import Text.Madlibs.Internal.Types
import Text.Madlibs.Internal.Utils
import Text.Madlibs.Ana.ParseUtils
import Text.Madlibs.Cata.SemErr
import qualified Data.Text as T
import Text.Megaparsec
import Text.Megaparsec.Text
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Lexer as L
import Data.Monoid
import Control.Monad
import Control.Monad.State

-- | Parse a lexeme, aka deal with whitespace nicely. 
lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

-- | space consumer with awareness for comments
spaceConsumer :: Parser ()
spaceConsumer = L.space (void . some $ spaceChar) (L.skipLineComment "#") (L.skipBlockComment "{#" "#}")

-- | parse a symbol, i.e. string plus surrouding whitespace
symbol :: String -> Parser String
symbol = L.symbol spaceConsumer

-- | Parse a number/probability
float :: Parser Prob
float = lexeme L.float

-- | Make sure definition blocks start un-indented
nonIndented = L.nonIndented spaceConsumer

--indentGuard = L.indentGuard spaceConsumer

-- | Parse between quotes
quote :: Parser a -> Parser a
quote = between .$ (symbol "\"")

-- | Parse a keyword
keyword :: String -> Parser String
keyword str = (pure <$> char ':') <> (symbol str) <?> "keyword"

-- | Parse the `define` keyword.
define :: Parser ()
define = (void $ nonIndented (keyword "define"))
    <?> "define block"
    --make them more similar/reuse code here!!

-- | Parse the `:return` keyword.
main :: Parser ()
main = (void $ nonIndented (keyword "return"))
    <?> "return block"

-- | Parse a template name (what follows a `:define` or `return` block)
name :: Parser String
name = lexeme (some letterChar) <?> "template name"

-- | Parse template into a `PreTok` of referents and strings
preStr :: Parser PreTok
preStr = (fmap (Name . T.pack) name) <|>
    do {
    s <- quote (many $ noneOf ("\"\'" :: String)) ;
    pure $ PreTok . T.pack $ s
    } 
    <?> "string or function name"

-- | Parse a probability/corresponding template
pair :: Parser (Prob, [PreTok])
pair = do
    --indentGuard
    p <- float
    str <- some $ preStr
    pure (p, str)

-- | Parse a `define` block
definition :: Parser (Key, [(Prob, [PreTok])])
definition = do
    define
    str <- name
    val <- some pair
    --linebreak
    pure (T.pack str, val)

-- | Parse the `:return` block
final :: Parser [(Prob, [PreTok])]
final = do
    main
    val <- some pair
    pure val

-- | Parse the program in terms of `PreTok` and the `Key`s to link them.
program :: Parser [(Key, [(Prob, [PreTok])])]
program = sortKeys . checkSemantics <$> do
    p <- many (try definition <|> ((,) "Template" <$> final))
    pure p

-- | Parse text as a token + context (aka a reader monad with all the other functions)
parseTokM :: Parser (Context RandTok)
parseTokM = fmap build program

-- | Parse text as a token
--
-- > f <- readFile "template.mad"
-- > parseTok f
parseTok :: T.Text -> Either (ParseError Char Dec) RandTok
parseTok f = snd . head . (filter (\(i,j) -> i == "Template")) . (flip execState []) <$> runParser parseTokM "" f
