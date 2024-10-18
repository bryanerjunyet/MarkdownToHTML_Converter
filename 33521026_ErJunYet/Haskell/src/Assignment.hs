module Assignment (markdownParser, convertADTHTML, generateHTML) where


import           Data.Time.Clock  (getCurrentTime)
import           Data.Time.Format (defaultTimeLocale, formatTime)
import           Data.Char        (isDigit)
import           Instances        (Parser (..), ParseResult (..), ParseError (..))
import           Parser           ( some, many, is, string, inlineSpace, noneof, satisfy )
import           Control.Applicative ((<|>), optional, empty)

-- ==========================================================
-- Data Types for Algebraic Data Types for Markdown document
-- ==========================================================

-- The Document type representing the entire Markdown document
data Document 
    = Document [Element]    -- A document consists of a list of elements
    deriving (Show, Eq)

-- Elements in the document are either block-level or paragraph-level
data Element 
    = ParagraphElement ParagraphElement -- Paragraph elements
    | BlockElement BlockElement         -- Block elements
    deriving (Show, Eq)

-- Paragraph-level content consists of FreeText with possible modifiers
data ParagraphElement
    = Paragraph [FreeText]  -- Paragraphs contain FreeText
    deriving (Show, Eq)

-- FreeText is either plain or modified text
data FreeText
    = PlainText PlainText               -- Plain text
    | TextModifier TextModifier         -- Special Markdown content
    deriving (Show, Eq)

-- Plain text is non-special Markdown content
data PlainText
    = String String                     -- Non-special Markdown content
    deriving (Show, Eq)

-- Text modifiers include bold, italic, links, inline code, etc.
data TextModifier
    = Bold PlainText                    -- Bold text
    | Italic PlainText                  -- Italic text
    | Strikethrough PlainText           -- Strikethrough text
    | Link PlainText PlainText          -- Link
    | InlineCode PlainText              -- Inline code (e.g., `code`)
    | FootNote Int                      -- Footnote
    deriving (Show, Eq)

-- Block-level elements include headers, blockquotes, lists, tables, etc.
data BlockElement
    = Header Int [FreeText]                 -- Headers
    | BlockQuote [ParagraphElement]         -- Blockquotes
    | BlockCode PlainText PlainText         -- Code
    | FootNoteReference Int PlainText  -- Footnote reference
    | OrderedLists [OrderedListItem]             -- Ordered lists
    | Table TableHeader [TableRow]          -- Tables
    | Image PlainText PlainText PlainText   -- Images
    deriving (Show, Eq)

-- Ordered list items can be nested
-- data OrderedLists
--     = OrderedLists [OrderedListItem]      -- Each list item can have free text elements

data OrderedListItem 
    = ListItem [FreeText]                 -- Each list item can have free text elements
    | SubList [OrderedListItem]           -- Nested sublists
    deriving (Show, Eq)

-- Table structures
data TableHeader 
    = TableHeader [TableCell]           -- Table header consists of table cells
    deriving (Show, Eq)

data TableRow 
    = TableRow [TableCell]              -- Table row consists of table cells
    deriving (Show, Eq)

data TableCell 
    = TableCell [FreeText]              -- Table cells consist of free text elements
    deriving (Show, Eq)


-- ==================================
-- Parsing Markdown to ADT (Document)
-- ==================================

-- Main markdown parser
markdownParser :: Parser Document
markdownParser = do
    elements <- many elementParser
    return $ Document elements

-- Parser for individual elements (blocks or paragraphs)
elementParser :: Parser Element
elementParser = (BlockElement <$> blockElementParser)
            <|> (ParagraphElement <$> paragraphElementParser)

-------------------- Block-level parsers --------------------

-- Block-level element parser
blockElementParser :: Parser BlockElement
blockElementParser = headerParser
                 <|> blockQuoteParser
                 <|> blockCodeParser
                 <|> footNoteReferenceParser
                 <|> orderedListParser
                 <|> tableParser
                 <|> imageParser

-- | Parser for both types of headers
headerParser :: Parser BlockElement
headerParser = plainHeaderParser <|> alternativeHeaderParser

-- | Parses plain Markdown headers, like '# Header'
plainHeaderParser :: Parser BlockElement
plainHeaderParser = do
    _ <- inlineSpace
    level <- length <$> some (is '#')  -- Count the number of '#'
    _ <- inlineSpace
    content <- freeTextParser
    return $ Header level [content]

-- | Parses alternative Markdown headers, like 'Header' followed by '=====' or '-----'
alternativeHeaderParser :: Parser BlockElement
alternativeHeaderParser = do
    content <- freeTextParser
    _ <- newline
    level <- (is '=' >> return 1) <|> (is '-' >> return 2)
    return $ Header level [content]

-- Parsing blockquotes (> quote)
blockQuoteParser :: Parser BlockElement
blockQuoteParser = do
    _ <- is '>'
    content <- paragraphElementParser
    return $ BlockQuote [content]

-- Parsing code blocks (```language\ncode```)
blockCodeParser :: Parser BlockElement
blockCodeParser = do
    _ <- string "```"
    language <- optionalPlainText
    code <- manyTill anyChar (try $ string "```")
    return $ BlockCode (maybePlainText language) (String code)

footNoteReferenceParser :: Parser BlockElement
footNoteReferenceParser = do
    _ <- string "[^"
    ref <- some digit
    _ <- is ']'
    content <- plainTextParser
    return $ FootNoteReference (read ref) content

-- Ordered list parser (1. item)
orderedListParser :: Parser BlockElement
orderedListParser = do
    items <- some orderedListItemParser
    return $ OrderedLists items

-- Parsing ordered list items
orderedListItemParser :: Parser OrderedListItem
orderedListItemParser = do
    _ <- some digit
    _ <- string ". "
    content <- freeTextParser
    return $ ListItem [content]

-- Table parser (| col1 | col2 |)
tableParser :: Parser BlockElement
tableParser = do
    TableRow headerCells <- tableRowParser  -- Extract [TableCell] from TableRow
    rows <- many tableRowParser
    return $ Table (TableHeader headerCells) rows  -- Pass [TableCell] to TableHeader

-- Table row parser
tableRowParser :: Parser TableRow
tableRowParser = do
    _ <- is '|'
    cells <- tableCellParser `sepBy` is '|'
    _ <- is '|'
    return $ TableRow cells

-- Table cell parser
tableCellParser :: Parser TableCell
tableCellParser = do
    content <- freeTextParser
    return $ TableCell [content]

-- Image parser (![alt text](url "caption"))
imageParser :: Parser BlockElement
imageParser = do
    _ <- string "!["
    altText <- plainTextParser
    _ <- is ']'
    _ <- is '('
    url <- plainTextParser
    caption <- optional plainTextParser
    _ <- is ')'
    return $ Image altText url (maybePlainText caption)

-------------------- Paragraph-level parsers --------------------

-- Paragraph parser
paragraphElementParser :: Parser ParagraphElement
paragraphElementParser = do
    content <- some freeTextParser
    return $ Paragraph content

-- Free text parser (can include plain text or text modifiers)
freeTextParser :: Parser FreeText
freeTextParser = (TextModifier <$> textModifierParser)
             <|> (PlainText <$> plainTextParser)

-- Plain text parser
plainTextParser :: Parser PlainText
plainTextParser = String <$> many (noneof "*_~`[]()|")

-- Text modifier parser (bold, italic, strikethrough, etc.)
textModifierParser :: Parser TextModifier
textModifierParser = boldParser
                 <|> italicParser
                 <|> strikethroughParser
                 <|> linkParser
                 <|> inlineCodeParser
                 <|> footNoteParser

-- Bold text parser (**bold**)
boldParser :: Parser TextModifier
boldParser = do
    _ <- string "**"
    content <- many (noneof "*")
    _ <- string "**"
    return $ Bold (String content)

-- Italic text parser (_italic_)
italicParser :: Parser TextModifier
italicParser = do
    _ <- is '_'
    content <- many (noneof "_")
    _ <- is '_'
    return $ Italic (String content)

-- Strikethrough text parser (~~text~~)
strikethroughParser :: Parser TextModifier
strikethroughParser = do
    _ <- string "~~"
    content <- many (noneof "~")
    _ <- string "~~"
    return $ Strikethrough (String content)

-- Link parser ([text](url))
linkParser :: Parser TextModifier
linkParser = do
    _ <- is '['
    text <- many (noneof "]")
    _ <- is ']'
    _ <- is '('
    url <- many (noneof ")")
    _ <- is ')'
    return $ Link (String text) (String url)

-- Inline code parser (`code`)
inlineCodeParser :: Parser TextModifier
inlineCodeParser = do
    _ <- is '`'
    content <- many (noneof "`")
    _ <- is '`'
    return $ InlineCode (String content)

footNoteParser :: Parser TextModifier
footNoteParser = do
    _ <- string "[^"
    ref <- some digit
    _ <- is ']'
    return $ FootNote (read ref) 


-- Optional plain text
optionalPlainText :: Parser (Maybe PlainText)
optionalPlainText = optional (String <$> many (noneof "\n"))

-- Helper to handle Maybe PlainText
maybePlainText :: Maybe PlainText -> PlainText
maybePlainText = maybe (String "") id


-- =============================
-- Converting ADT to Full HTML
-- =============================

-- Main function to convert the parsed ADT into a complete HTML document
generateHTML :: Document -> String
generateHTML doc = 
    "<!DOCTYPE html>\n" ++
    "<html lang=\"en\">\n" ++
    "  <head>\n" ++
    "    <meta charset=\"UTF-8\">\n" ++
    "    <meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">\n" ++
    "    <title>Markdown to HTML</title>\n" ++
    "  </head>\n" ++
    "  <body>\n" ++
    convertADTHTML 0 doc ++  -- Converts the Document to the body content with indentation level 0
    "  </body>\n" ++
    "</html>"

-- Main function to convert the parsed ADT into HTML body content with indentation level
convertADTHTML :: Int -> Document -> String
convertADTHTML level (Document elements) = unlines (map (elementToHTML level) elements)

-- Convert elements to HTML with indentation level
elementToHTML :: Int -> Element -> String
elementToHTML level (ParagraphElement p) = paragraphToHTML level p
elementToHTML level (BlockElement b) = blockElementToHTML level b

-------------------- HTML Conversions --------------------

-- Convert paragraph elements to HTML
paragraphToHTML :: Int -> ParagraphElement -> String
paragraphToHTML level (Paragraph texts) = indent level ++ "<p>" ++ concatMap freeTextToHTML texts ++ "</p>"

-- Convert free text to HTML
freeTextToHTML :: FreeText -> String
freeTextToHTML (PlainText (String text)) = text
freeTextToHTML (TextModifier modifier) = textModifierToHTML modifier

-- Convert text modifiers to HTML
textModifierToHTML :: TextModifier -> String
textModifierToHTML (Bold (String text)) = "<strong>" ++ text ++ "</strong>"
textModifierToHTML (Italic (String text)) = "<em>" ++ text ++ "</em>"
textModifierToHTML (Strikethrough (String text)) = "<del>" ++ text ++ "</del>"
textModifierToHTML (Link (String text) (String url)) = "<a href=\"" ++ url ++ "\">" ++ text ++ "</a>"
textModifierToHTML (InlineCode (String text)) = "<code>" ++ text ++ "</code>"
textModifierToHTML (FootNote num) = "<sup id=\"fnref" ++ show num ++ "\"><a href=\"#fn" ++ show num ++ "\">" ++ show num ++ "</a></sup>"

-- Convert block elements to HTML with indentation level
blockElementToHTML :: Int -> BlockElement -> String
blockElementToHTML level (Header lvl content) = 
    indent level ++ "<h" ++ show lvl ++ ">" ++ concatMap freeTextToHTML content ++ "</h" ++ show lvl ++ ">"
blockElementToHTML level (BlockQuote paragraphs) = 
    indent level ++ "<blockquote>\n" ++ concatMap (paragraphToHTML (level + 4)) paragraphs ++ indent level ++ "</blockquote>"
blockElementToHTML level (BlockCode (String lang) (String code)) = 
    indent level ++ "<pre><code class=\"" ++ lang ++ "\">" ++ code ++ "</code></pre>"
blockElementToHTML level (FootNoteReference num (String text)) = 
    indent level ++ "<li id=\"fn" ++ show num ++ "\"><p>" ++ text ++ " <a href=\"#fnref" ++ show num ++ "\">↩</a></p></li>"
blockElementToHTML level (OrderedLists items) = 
    indent level ++ "<ol>\n" ++ concatMap (orderedListItemToHTML (level + 4)) items ++ indent level ++ "</ol>"
blockElementToHTML level (Table (TableHeader header) rows) = 
    indent level ++ "<table>\n" ++
    indent (level + 4) ++ "<thead>\n" ++
    headerRowToHTML level header ++
    indent (level + 4) ++ "</thead>\n" ++
    indent (level + 4) ++ "<tbody>\n" ++
    concatMap (tableRowToHTML level) rows ++ 
    indent (level + 4) ++ "</tbody>\n" ++
    indent level ++ "</table>"
blockElementToHTML level (Image (String alt) (String url) (String caption)) = 
    indent level ++ "<img src=\"" ++ url ++ "\" alt=\"" ++ alt ++ "\">\n" ++ "title=\"" ++ caption ++ "\"\n"

-- Convert ordered list items to HTML
orderedListItemToHTML :: Int -> OrderedListItem -> String
orderedListItemToHTML level (ListItem content) = indent level ++ "<li>" ++ concatMap freeTextToHTML content ++ "</li>\n"
orderedListItemToHTML level (SubList items) = indent level ++ "<ul>\n" ++ concatMap (orderedListItemToHTML (level + 4)) items ++ indent level ++ "</ul>\n"

-- Convert table header rows to HTML
headerRowToHTML :: Int -> [TableCell] -> String
headerRowToHTML level cells = indent level ++ "<tr>" ++ concatMap (tableCellToHTML level) cells ++ "</tr>\n"

-- Convert table rows to HTML
tableRowToHTML :: Int -> TableRow -> String
tableRowToHTML level (TableRow cells) = indent level ++ "<tr>" ++ concatMap (tableCellToHTML level) cells ++ "</tr>\n"

-- Convert table cells to HTML
tableCellToHTML :: Int -> TableCell -> String
tableCellToHTML level (TableCell content) = indent level ++ "<td>" ++ concatMap freeTextToHTML content ++ "</td>"

-------------------- HTML Utility Functions --------------------

-- Helper function to create indentation based on the current level
indent :: Int -> String
indent level = replicate (level * 4) ' '  -- Indent with 4 spaces per level



-- | -------------------------------------------------
-- | --------------- Parser Utilities -----------------
-- | -------------------------------------------------


manyTill :: Parser a -> Parser b -> Parser [a]
manyTill p end = (end *> pure []) <|> ((:) <$> p <*> manyTill p end)

anyChar :: Parser Char
anyChar = Parser f
  where
    f []     = Error UnexpectedEof
    f (x:xs) = Result xs x

newline :: Parser Char
newline = is '\n'

sepBy :: Parser a -> Parser sep -> Parser [a]
sepBy p sep = (:) <$> p <*> many (sep *> p) <|> pure []

try :: Parser a -> Parser a
try (Parser p) = Parser $ \input ->
  case p input of
    Error _ -> Error UnexpectedEof
    result  -> result

digit :: Parser Char
digit = satisfy isDigit

-- choice :: [Parser a] -> Parser a
-- choice = foldr (<|>) empty



getTime :: IO String
getTime = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S" <$> getCurrentTime

