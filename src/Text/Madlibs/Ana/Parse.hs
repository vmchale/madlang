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
import Text.Megaparsec.Lexer.Tibetan
import Control.Exception hiding (try)
import Data.Composition

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
float = lexeme L.float <|> (fromIntegral <$> integer) <?> "Float"

-- | Parse an integer
integer :: Parser Integer
integer = lexeme (L.integer <|> parseNumber) <?> "Integer"

-- | Make sure definition blocks start un-indented
nonIndented = L.nonIndented spaceConsumer

-- | Make contents of definition blocks are indented.
indentGuard = L.indentGuard spaceConsumer GT (unsafePos 4)

-- | Parse between quotes
quote :: Parser a -> Parser a
quote = between (char '"') (char '"') 

-- | Parse a keyword
keyword :: String -> Parser String
keyword str = (char ':') >> (symbol str) <?> "keyword"

-- | Parse a var
var :: Parser Int
var = fromIntegral <$> do
    char '$'
    integer <?> "variable"

-- | Parse the `define` keyword.
define :: Parser ()
define = void (nonIndented (keyword "define"))
    <?> "define block"

include :: Parser ()
include = void (nonIndented (keyword "include"))
    <?> "include"

-- | Parse the `:return` keyword.
main :: Parser ()
main = void (nonIndented (keyword "return"))
    <?> "return block"

-- | Parse a template name (what follows a `:define` or `return` block)
name :: Parser String
name = lexeme (some (letterChar <|> oneOf ("-/" :: String))) <?> "template name"

-- | Parse template into a `PreTok` of referents and strings
preStr :: [T.Text] -> Parser PreTok
preStr ins = (fmap (Name . T.pack) name) <|>
    do {
        v <- var ;
        pure . PreTok $ ins `access` (v-1) -- ins !! (v - 1)
    } <|>
    do {
        s <- quote (many $ noneOf ("\n\"" :: String)) ;
        spaceConsumer ;
        pure $ PreTok . T.pack $ s
    } 
    <?> "string or function name"

-- | Parse a probability/corresponding template
pair :: [T.Text] -> Parser (Prob, [PreTok])
pair ins = do
    indentGuard
    p <- float
    str <- some (preStr ins)
    pure (p, str) <?> "Probability/text pair"

-- | Parse an `include`
inclusions :: Parser [String]
inclusions = many . try $ do
    include
    str <- name
    string ".mad"
    pure (str ++ ".mad")
        -- still needs dependency resolution but eh

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
program ins = sortKeys <$> (checkSemantics =<< do
    inclusions
    p <- many (try (definition ins) <|> ((,) "Template" <$> final ins))
    lexeme eof
    pure p)

-- | Parse text as a token + context (aka a reader monad with all the other functions)
parseTokM :: [T.Text] -> Parser (Context RandTok)
parseTokM ins = build <$> program ins

-- | Parse text as token + context
parseTreeM :: [T.Text] -> Parser (Context RandTok)
parseTreeM ins = buildTree <$> program ins

-- | Parse text as a list of functions
parseTokF :: FilePath -> [(Key, RandTok)] -> [T.Text] -> T.Text -> Either (ParseError Char Dec) [(Key, RandTok)]
parseTokF filename state ins f = (flip execState (filterTemplate state)) <$> runParser (parseTokM ins) filename f -- FIXME fix labelling
    where filterTemplate = filter (\(i,j) -> i /= "Template")

-- | Parse text given a context
parseTokCtx :: FilePath -> [(Key, RandTok)] -> [T.Text] -> T.Text -> Either (ParseError Char Dec) RandTok
parseTokCtx = (fmap takeTemplate) .*** parseTokF

-- | Parse text as a token
--
-- > f <- readFile "template.mad"
-- > parseTok f
parseTok :: FilePath -> [T.Text] -> T.Text -> Either (ParseError Char Dec) RandTok
parseTok filename ins f = takeTemplate . (flip execState []) <$> runParser (parseTokM ins) filename f

-- | Parse text as a token, suitable for printing as a tree..
parseTree :: FilePath -> [T.Text] -> T.Text -> Either (ParseError Char Dec) RandTok
parseTree filename ins f = takeTemplate . (flip execState []) <$> runParser (parseTreeM ins) filename f

-- | Parse inclustions
parseInclusions :: FilePath -> T.Text -> Either (ParseError Char Dec) [String]
parseInclusions filename f = runParser (inclusions) filename f
