module Assignment (markdownParser, convertADTHTML) where


import           Data.Time.Clock  (getCurrentTime)
import           Data.Time.Format (defaultTimeLocale, formatTime)
import           Data.Char        (isDigit)
import           Instances        (Parser (..))
import           Parser           ( some, many, char, is, string, space, spaces1, noneof, satisfy )
import           Control.Applicative hiding (some, many)

-- Define the ADT for various markdown elements
data ADT
  = Heading Int String              -- Level 1-6 with the heading text
  | Italic String                   -- Italic text
  | Bold String                     -- Bold text
  | Strikethrough String            -- Strikethrough text
  | Link String String              -- Link text and URL
  | InlineCode String               -- Inline code
  | Footnote String Int             -- Footnote with text and index
  | FreeText String                 -- Free text
  | Blockquote [ADT]                -- Blockquote content (recursive)
  | CodeBlock String String         -- Code block with language and content
  | OrderedList [[ADT]]             -- Nested ordered list
  | Image String String String      -- Alt Text, URL, and Caption
  | Table [[String]]                -- Table with rows of strings
  deriving (Show, Eq)

-- The main markdown parser function, with the ADT type signature
markdownParser :: Parser ADT
markdownParser = parseMarkdownElement

-- Function to parse individual markdown elements into ADT
parseMarkdownElement :: Parser ADT
parseMarkdownElement =
      parseHeading
  <|> parseItalic
  <|> parseBold
  <|> parseStrikethrough
  <|> parseLink
  <|> parseInlineCode
  <|> parseFootnote
  <|> parseImage
  <|> parseBlockquote
  <|> parseCodeBlock
  <|> parseOrderedList
  <|> parseTable
  <|> parseFreeText

-- Implement each parser function

-- Parse headings (e.g., # Heading 1)
parseHeading :: Parser ADT
parseHeading = do
  hashes <- some (char '#')
  space
  text <- many (noneof "\n")
  return $ Heading (length hashes) text

-- Parse italic text (e.g., _italic_)
parseItalic :: Parser ADT
parseItalic = do
  char '_'
  content <- some (noneof "_")
  char '_'
  return $ Italic content

-- Parse bold text (e.g., **bold**)
parseBold :: Parser ADT
parseBold = do
  string "**"
  content <- some (noneof "**")
  string "**"
  return $ Bold content

-- Parse strikethrough text (e.g., ~~strikethrough~~)
parseStrikethrough :: Parser ADT
parseStrikethrough = do
  string "~~"
  content <- some (noneof "~~")
  string "~~"
  return $ Strikethrough content

-- Parse links (e.g., [link](url))
parseLink :: Parser ADT
parseLink = do
  char '['
  linkText <- many (noneof "]")
  char ']'
  char '('
  url <- many (noneof ")")
  char ')'
  return $ Link linkText url

-- Parse inline code (e.g., `code`)
parseInlineCode :: Parser ADT
parseInlineCode = do
  char '`'
  code <- many (noneof "`")
  char '`'
  return $ InlineCode code

-- Parse footnotes (e.g., [^1])
parseFootnote :: Parser ADT
parseFootnote = do
  string "[^"
  idx <- some (satisfy isDigit)
  char ']'
  return $ Footnote "Footnote" (read idx)

-- Parse blockquote (e.g., > blockquote)
parseBlockquote :: Parser ADT
parseBlockquote = do
  char '>'
  space
  content <- many (noneof "\n")
  return $ Blockquote [FreeText content]

-- Parse code blocks (e.g., ```lang\ncode\n```)
parseCodeBlock :: Parser ADT
parseCodeBlock = do
  string "```"
  lang <- many (noneof "\n")
  char '\n'
  code <- many (noneof "`")
  string "```"
  return $ CodeBlock lang code

-- Parse ordered lists (e.g., 1. list item)
parseOrderedList :: Parser ADT
parseOrderedList = do
  some (satisfy isDigit)
  char '.'
  space
  items <- some parseMarkdownElement
  return $ OrderedList [items]

parseImage :: Parser ADT
parseImage = do
  _ <- string "!["
  alt <- many (noneof "]")
  _ <- string "]("
  url <- many (noneof " ")
  _ <- spaces1
  _ <- char '"'
  caption <- many (noneof "\"")
  _ <- char '"'
  _ <- char ')'
  return $ Image alt url caption

-- Parse tables (simple parser for now)
parseTable :: Parser ADT
parseTable = do
  rows <- many parseTableRow
  return $ Table rows

parseTableRow :: Parser [String]
parseTableRow = do
  cells <- many (noneof "|")
  return [cells]

-- Parse any free text (non-special text)
parseFreeText :: Parser ADT
parseFreeText = do
  text <- some (noneof "\n")
  return $ FreeText text

-- Function to convert ADT to HTML string
convertADTHTML :: ADT -> String
convertADTHTML (Heading level text) = "<h" ++ show level ++ ">" ++ text ++ "</h" ++ show level ++ ">"
convertADTHTML (Italic text) = "<em>" ++ text ++ "</em>"
convertADTHTML (Bold text) = "<strong>" ++ text ++ "</strong>"
convertADTHTML (Strikethrough text) = "<del>" ++ text ++ "</del>"
convertADTHTML (Link text url) = "<a href=\"" ++ url ++ "\">" ++ text ++ "</a>"
convertADTHTML (InlineCode code) = "<code>" ++ code ++ "</code>"
convertADTHTML (Footnote text idx) = "<sup id=\"footnote-" ++ show idx ++ "\">" ++ text ++ "</sup>"
convertADTHTML (FreeText text) = text
convertADTHTML (Blockquote content) = "<blockquote>" ++ concatMap convertADTHTML content ++ "</blockquote>"
convertADTHTML (CodeBlock lang code) = "<pre><code class=\"" ++ lang ++ "\">" ++ code ++ "</code></pre>"
convertADTHTML (OrderedList items) = "<ol>" ++ concatMap convertListItem items ++ "</ol>"
convertADTHTML (Image alt url caption) = "<figure><img src=\"" ++ url ++ "\" alt=\"" ++ alt ++ "\"><figcaption>" ++ caption ++ "</figcaption></figure>"
convertADTHTML (Table rows) = "<table>" ++ concatMap convertTableRow rows ++ "</table>"

-- Helper to convert list items
convertListItem :: [ADT] -> String
convertListItem item = "<li>" ++ concatMap convertADTHTML item ++ "</li>"

-- Helper to convert table rows
convertTableRow :: [String] -> String
convertTableRow row = "<tr>" ++ concatMap (\cell -> "<td>" ++ cell ++ "</td>") row ++ "</tr>"

getTime :: IO String
getTime = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S" <$> getCurrentTime

