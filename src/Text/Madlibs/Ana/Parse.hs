{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Parse our DSL
module Text.Madlibs.Ana.Parse (
    parseTok
  , parseTokF
  , parseInclusions
  , parseTree
  , parseTreeF
  , parseTokM
  , parseTokInternal
  , parseTokFInternal
  ) where

import           Control.Composition
import           Control.Monad.State
import qualified Data.Map                    as M
import           Data.Maybe
import           Data.Semigroup              ((<>))
import qualified Data.Text                   as T
import           Data.Void
import           Text.Madlibs.Ana.ParseUtils
import           Text.Madlibs.Cata.SemErr
import           Text.Madlibs.Internal.Types
import           Text.Madlibs.Internal.Utils
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer  as L
-- | Parse a lexeme, aka deal with whitespace nicely.
lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

spaceOnly :: Parser ()
spaceOnly = L.space (void . some $ oneOf ("\t " :: String)) (L.skipLineComment "#") (L.skipBlockComment "{#" "#}")

-- | space consumer with awareness for comments
spaceConsumer :: Parser ()
spaceConsumer = L.space (void . some $ spaceChar) (L.skipLineComment "#") (L.skipBlockComment "{#" "#}")

-- | parse a symbol, i.e. string plus surrouding whitespace
symbol :: T.Text -> Parser T.Text
symbol = L.symbol spaceConsumer

-- | Parse a number/probability
float :: Parser Prob
float = lexeme L.float <|> (fromIntegral <$> integer) <?> "Number"

-- | Parse an integer
integer :: Parser Integer
integer = lexeme L.decimal <?> "Integer"

-- | Make sure definition blocks start un-indented
nonIndented :: Parser a -> Parser a
nonIndented = L.nonIndented spaceConsumer

-- | Make contents of definition blocks are indented.
indentGuard :: Parser Pos
indentGuard = L.indentGuard spaceConsumer GT (mkPos 4)

-- | Parse between quotes
quote :: Parser a -> Parser a
quote = between .$ char '"'

-- | Parse a keyword
keyword :: T.Text -> Parser T.Text
keyword str = char ':' >> symbol str <?> "keyword"

-- | Parse a var
var :: Parser Int
var = fromIntegral <$> do
    char '$'
    integer <?> "variable"

-- | Parse the `define` keyword.
define :: Parser ()
define = void (nonIndented (keyword "define"))
    <?> "define block"

-- | Parse the `category` keyword.
cat :: Parser ()
cat = void (nonIndented (keyword "category"))
    <?> "category block"

-- | Parse the `include` keyword.
include :: Parser ()
include = void (nonIndented (keyword "include"))
    <?> "include"

-- | Parse the `:return` keyword.
main :: Parser ()
main = void (nonIndented (keyword "return"))
    <?> "return block"

-- | Parse a template name (what follows a `:define` or `return` block)
name :: Parser T.Text
name = (lexeme . fmap T.pack) (some (letterChar <|> oneOf ("-/" :: String))) <?> "template name" -- TODO make this broader in terms of what it includes

-- | Parse a modifier
modifier :: Parser (T.Text -> T.Text)
modifier = do
    char '.'
    str <- foldr ((<|>) . try . string) (pure "") ["to_upper", "to_lower", "reverse", "reverse_words", "oulipo", "capitalize", "titlecase"]
    pure (fromMaybe id (M.lookup (T.unpack str) modifierList)) <?> "modifier"

-- | Parse template into a `PreTok` of referents and strings
preStr :: [T.Text] -> Parser PreTok
preStr ins = do {
        n <- name ;
        mod' <- many modifier ;
        spaceOnly ;
        pure $ Name n (foldr (.) id mod')
    } <|>
    do {
        v <- var ;
        mod' <- many modifier ;
        spaceOnly ;
        pure . PreTok . foldr (.) id mod' $ ins `access` (v-1)
    } <|>
    do {
        s <- quote (many $ noneOf ("\n\"" :: String)) ;
        mod' <- many modifier ;
        spaceOnly ;
        pure . PreTok . foldr (.) id mod' . T.pack $ s
    }
    <?> "string or function name"

-- | Parse a probability/corresponding template
pair :: [T.Text] -> Parser (Prob, [PreTok])
pair ins = lexeme $ do
    indentGuard
    p <- float
    str <- some (preStr ins)
    pure (p, str) <?> "Probability-text pair"

-- | Parse a function name for a `:category` block.
function :: Parser (Prob, [PreTok])
function = lexeme $ do
    indentGuard
    char '|'
    spaceOnly
    str <- some (preStr mempty)
    pure (1.0, str) <?> "Function name"

-- | Parse an `include`
inclusions :: Parser [T.Text]
inclusions = many . try $ do
    include
    str <- name
    string ".mad"
    pure (str <> ".mad") <?> "Include statement"

-- | Parse a `define` block
definition :: [T.Text] -> Parser (Key, [(Prob, [PreTok])])
definition ins = do
    define
    str <- name
    val <- fmap normalize . some $ pair ins
    pure (str, val) <?> "define block"

-- | Parse a `category` block
category :: Parser (Key, [(Prob, [PreTok])])
category = do
    cat
    str <- name
    val <- fmap normalize . some $ function
    pure (str, val) <?> "category block"

-- | Parse a `:library` declaration
preLibrary :: Parser ()
preLibrary = void (nonIndented (keyword "library"))
    <?> "library declaration"

library :: Parser (Key, [(Prob, [PreTok])])
library = preLibrary >> pure ("Return", [(1, [])])

-- | Parse the `:return` block
final :: [T.Text] -> Parser [(Prob, [PreTok])]
final ins = do
    main
    fmap normalize . some $ pair ins

-- | Parse the program in terms of `PreTok` and the `Key`s to link them.
program :: [T.Text] -> Parser [(Key, [(Prob, [PreTok])])]
program ins = sortKeys <$> (checkSemantics =<< do
    inclusions
    p <- many (try (definition ins) <|> try category <|> try ((,) "Return" <$> final ins) <|> library)
    lexeme eof
    pure p)

-- | Parse text as a token + context (aka a reader monad with all the other functions)
parseTokM :: [T.Text] -> Parser (Context RandTok)
parseTokM ins = build <$> program ins

-- | Parse text as token + context
parseTreeM :: [T.Text] -> Parser (Context RandTok)
parseTreeM ins = buildTree <$> program ins

-- | Parse text as a list of functions
parseTokF :: FilePath -> [(Key, RandTok)] -> [T.Text] -> T.Text -> Either (ParseError Char (ErrorFancy Void)) [(Key, RandTok)]
parseTokF filename state' ins f = flip execState (filterTemplate filename state') <$> runParser (parseTokM ins) filename f

-- | Parse text as a list of tokens, suitable for printing as a tree.
parseTreeF :: FilePath -> [(Key, RandTok)] -> [T.Text] -> T.Text -> Either (ParseError Char (ErrorFancy Void)) [(Key, RandTok)]
parseTreeF filename state' ins f = flip execState (filterTemplate filename state') <$> runParser (parseTreeM ins) filename f

filterTemplate :: String -> [(T.Text, t)] -> [(T.Text, t)]
filterTemplate filename = map (\(i,j) -> if i == "Return" then (strip filename, j) else (i,j)) -- TODO fix the extras

-- | Parse text given a context
--
-- > import qualified Data.Text.IO as TIO
-- >
-- > getParsed = do
-- >     f <- TIO.readFile "template.mad"
-- >     parseTok "filename.mad" [] [] f
parseTok :: FilePath -- ^ File name to use for parse errors
    -> [(Key, RandTok)] -- ^ Context, i.e. other random data paired with a key.
    -> [T.Text] -- ^ list of variables to substitute into the template
    -> T.Text -- ^ Actaul text to parse
    -> Either (ParseError Char (ErrorFancy Void)) RandTok -- ^ Result
parseTok = fmap takeTemplate .*** parseTokF

parseTokInternal :: FilePath
                 -> [T.Text]
                 -> [[(Key, RandTok)]]
                 -> T.Text
                 -> Either (ParseError Char (ErrorFancy Void)) RandTok
parseTokInternal path ins ctx = parseTok path (join ctx) ins

parseTokFInternal :: FilePath -> [T.Text] -> [[(Key, RandTok)]] -> T.Text -> Either (ParseError Char (ErrorFancy Void)) [(Key, RandTok)]
parseTokFInternal path ins ctx = parseTokF path (join ctx) ins

-- | Parse text as a token, suitable for printing as a tree..
parseTree :: FilePath -> [(Key, RandTok)] -> [T.Text] -> T.Text -> Either (ParseError Char (ErrorFancy Void)) RandTok
parseTree = fmap takeTemplate .*** parseTreeF

-- | Parse inclusions
parseInclusions :: FilePath -> T.Text -> Either (ParseError Char (ErrorFancy Void)) [T.Text]
parseInclusions = runParser inclusions
