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

-- | Parse an integer
integer :: Parser Integer
integer = lexeme L.integer

-- | Make sure definition blocks start un-indented
nonIndented = L.nonIndented spaceConsumer

-- | Make contents of definition blocks are indented.
indentGuard = L.indentGuard spaceConsumer GT (unsafePos 4)

-- | Parse between quotes
quote :: Parser a -> Parser a
quote = between .$ (char '"') --also CAN'T have any \n AFTER

-- | Parse a keyword
keyword :: String -> Parser String
keyword str = (pure <$> char ':') <> (symbol str) <?> "keyword"

-- | Parse a var
var :: Parser Int
var = fromIntegral <$> do
    char '$'
    integer <?> "variable"

-- | Parse the `define` keyword.
define :: Parser ()
define = (void $ nonIndented (keyword "define"))
    <?> "define block"

-- | Parse the `:return` keyword.
main :: Parser ()
main = (void $ nonIndented (keyword "return"))
    <?> "return block"

-- | Parse a template name (what follows a `:define` or `return` block)
name :: Parser String
name = lexeme (some letterChar) <?> "template name"

-- | Parse template into a `PreTok` of referents and strings
preStr :: [T.Text] -> Parser PreTok
preStr ins = (fmap (Name . T.pack) name) <|>
    do {
        v <- var ;
        pure . PreTok $ ins `access` (v-1) -- ins !! (v - 1)
    } <|>
    do {
        s <- quote (many $ noneOf ("\"" :: String)) ;
        spaceConsumer ;
        pure $ PreTok . T.pack $ s
    } 
    <?> "string or function name"

-- | Parse a probability/corresponding template
pair :: [T.Text] -> Parser (Prob, [PreTok])
pair ins = do
    indentGuard
    p <- float
    str <- some $ (preStr ins)
    pure (p, str) <?> "Probability/text pair"

-- | Parse a `define` block
definition :: [T.Text] -> Parser (Key, [(Prob, [PreTok])])
definition ins = do
    define
    str <- name
    val <- fmap normalize . some $ pair ins
    pure (T.pack str, val) <?> "define block"

-- | Parse the `:return` block
final :: [T.Text] -> Parser [(Prob, [PreTok])]
final ins = do
    main
    val <- fmap normalize . some $ pair ins
    pure val

-- | Parse the program in terms of `PreTok` and the `Key`s to link them.
program :: [T.Text] -> Parser [(Key, [(Prob, [PreTok])])]
program ins = sortKeys . checkSemantics <$> do
    p <- many (try (definition ins) <|> ((,) "Template" <$> final ins))
    pure p

parseTreeM :: [T.Text] -> Parser (Context RandTok)
parseTreeM ins = buildTree <$> program ins

-- | Parse text as a token + context (aka a reader monad with all the other functions)
parseTokM :: [T.Text] -> Parser (Context RandTok)
parseTokM ins = build <$> program ins

-- | Parse text as a token
--
-- > f <- readFile "template.mad"
-- > parseTok f
parseTok :: FilePath -> [T.Text] -> T.Text -> Either (ParseError Char Dec) RandTok
parseTok filename ins f = snd . head . (filter (\(i,j) -> i == "Template")) . (flip execState []) <$> runParser (parseTokM ins) filename f

parseTree :: FilePath -> [T.Text] -> T.Text -> Either (ParseError Char Dec) RandTok
parseTree filename ins f = snd . head . (filter (\(i,j) -> i == "Template")) . (flip execState []) <$> runParser (parseTreeM ins) filename f
