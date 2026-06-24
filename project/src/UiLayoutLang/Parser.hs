{-# LANGUAGE OverloadedStrings #-}

-- | Parser for the UiLayoutLang DSL using Megaparsec.
--
-- Supports:
--   * @window "name" W x H { ... }@ declarations
--   * @row { ... }@ and @col { ... }@ containers
--   * @box { width: ..., height: ..., color: ... }@ boxes
--   * Line comments with @\/\/@
--   * Useful error messages with source positions
module UiLayoutLang.Parser
  ( parseLayout
  , parseLayout'
  , ParseError
  ) where

import           Data.Void              (Void)
import           Data.Char              (isAlphaNum, isAlpha)
import           Text.Megaparsec hiding (ParseError)
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import           UiLayoutLang.Types

-- | The parser error type.
type ParseError = ParseErrorBundle String Void

-- | The parser monad.
type Parser = Parsec Void String

------------------------------------------------------------------------
-- Lexer helpers
------------------------------------------------------------------------

-- | Skip whitespace and line comments (// ...)
sc :: Parser ()
sc = L.space space1 (L.skipLineComment "//") empty

-- | Run a parser and consume trailing whitespace.
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- | Parse a fixed keyword/symbol and consume trailing whitespace.
symbol :: String -> Parser String
symbol = L.symbol sc

-- | Parse content between braces @{ ... }@.
braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

------------------------------------------------------------------------
-- Size parsers
------------------------------------------------------------------------

-- | Parse a size value: either a percentage (e.g. @50%@) or
-- pixels (e.g. @200px@ or just @200@).
pSize :: Parser Size
pSize = try pPct <|> pPx
  where
    pPct = do
      n <- lexeme L.decimal
      _ <- symbol "%"
      pure (Pct (fromIntegral (n :: Int) / 100.0))

    pPx = do
      n <- lexeme L.decimal
      _ <- optional (symbol "px")
      pure (Px (n :: Int))

------------------------------------------------------------------------
-- Property parsers
------------------------------------------------------------------------

-- | Parse an identifier (alphabetic start, then alphanumeric).
pIdentifier :: Parser String
pIdentifier = lexeme ((:) <$> satisfy isAlpha <*> many (satisfy isAlphaNum))

-- | Parse a color value — a simple identifier like @red@, @blue@, etc.
-- Also supports hex colors like @#ff0000@.
pColor :: Parser String
pColor = lexeme (pHex <|> pName)
  where
    pHex  = (:) <$> char '#' <*> some (satisfy isHexChar)
    pName = some (satisfy isAlphaNum)
    isHexChar c = isAlphaNum c

-- | Parse a comma or semicolon separator (both accepted).
pSep :: Parser ()
pSep = optional (symbol "," <|> symbol ";") *> pure ()


------------------------------------------------------------------------
-- Layout parsers
------------------------------------------------------------------------

-- | Parse a @box { ... }@ declaration.  A box may contain only
-- properties or properties followed by child layouts.
pBox :: Parser Layout
pBox = do
  _ <- symbol "box"
  propsAndChildren <- braces pBoxContents
  pure (uncurry Box propsAndChildren)
  where
    pBoxContents = do
      -- Try to parse properties first
      props <- pInlineProps
      children <- many pLayout
      pure (props, children)

-- | Parse inline properties (key: value pairs that appear before
-- any child layout declarations).
pInlineProps :: Parser Props
pInlineProps = buildProps <$> many (try pInlineProp)
  where
    buildProps ps = foldl applyProp defaultProps ps

    applyProp p ("width",  Left s)      = p { width = s }
    applyProp p ("height", Left s)      = p { height = s }
    applyProp p ("color",  Right c)     = p { color = Just c }
    applyProp p ("dir",    Right "row") = p { dir = Row }
    applyProp p ("dir",    Right "col") = p { dir = Col }
    applyProp p _                       = p

    pInlineProp = do
      key <- pIdentifier
      _ <- symbol ":"
      val <- (Left <$> try pSize) <|> (Right <$> pColor)
      pSep
      pure (key, val)

-- | Parse a @row { ... }@ container — shorthand for a box with
-- 'Row' direction.
pRow :: Parser Layout
pRow = do
  _ <- symbol "row"
  children <- braces (many pLayout)
  pure (Box defaultProps { dir = Row } children)

-- | Parse a @col { ... }@ container — shorthand for a box with
-- 'Col' direction.
pCol :: Parser Layout
pCol = do
  _ <- symbol "col"
  children <- braces (many pLayout)
  pure (Box defaultProps { dir = Col } children)

-- | Parse any layout element: a @row@, @col@, or @box@.
pLayout :: Parser Layout
pLayout = pRow <|> pCol <|> pBox

------------------------------------------------------------------------
-- Window parser
------------------------------------------------------------------------

-- | Parse a window declaration:
-- @window "name" W x H { ... }@
pWindow :: Parser Window
pWindow = do
  _ <- symbol "window"
  name <- lexeme (char '"' *> manyTill L.charLiteral (char '"'))
  w <- lexeme L.decimal
  _ <- symbol "x"
  h <- lexeme L.decimal
  layout <- braces pLayout
  pure (Window name w h layout)

------------------------------------------------------------------------
-- Top-level API
------------------------------------------------------------------------

-- | Parse a complete UiLayoutLang source string.  Returns either
-- a parse error or the parsed 'Window'.
parseLayout :: String -> Either ParseError Window
parseLayout = parse (sc *> pWindow <* eof) "<input>"

-- | Parse with a custom source name (for better error messages).
parseLayout' :: String -> String -> Either ParseError Window
parseLayout' srcName = parse (sc *> pWindow <* eof) srcName
