module Assignment (markdownParser, convertADTHTML, writeTextToFile) where


import           Data.Time.Clock  (getCurrentTime)
import           Data.Time.Format (defaultTimeLocale, formatTime)
import           Data.Char        (isDigit)
import           Data.List        (dropWhile, intersperse)
import           Instances        (Parser (..), ParseResult (..), ParseError (..))
import           Parser           ( char, some, many, is, string, space, inlineSpace, noneof, satisfy, stringTok )
import           Control.Applicative ((<|>), optional, empty)
import           Control.Monad (void)

-- This file contains code that was generated with the assistance of ChatGPT and Copilot.
-- Some parts of the code were created or modified using these tools to improve efficiency and accuracy.
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
    | FootNoteReference Int PlainText           -- Single footnote reference
    | FootNoteReferences [(Int, PlainText)]     -- Multiple footnote references
    | OrderedLists [OrderedListItem]             -- Ordered lists
    | Table TableHeader [TableRow]          -- Tables
    | Image PlainText PlainText PlainText   -- Images
    deriving (Show, Eq)

data OrderedListItem 
    = ListItem [FreeText] [OrderedListItem] -- Each list item can have free text elements and sublists
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
blockElementParser = plainHeaderParser
                 <|> alternativeHeaderParser
                 <|> blockQuoteParser
                 <|> blockCodeParser
                 <|> footNoteReferencesParser
                 <|> orderedListParser
                 <|> tableParser
                 <|> imageParser
                 <*  optional blankLines

-- | Parser for both types of headers
headerParser :: Parser BlockElement
headerParser = plainHeaderParser <|> alternativeHeaderParser

-- | Parses plain Markdown headers, like '# Header'
plainHeaderParser :: Parser BlockElement
plainHeaderParser = do
    _ <- inlineSpace
    level <- length <$> some (is '#')  -- Count the number of '#'
    _ <- space
    content <- manyTill freeTextParser newline  -- Consume content until newline
    return $ Header level content

-- | Parses alternative Markdown headers, like 'Header' followed by '=====' or '-----'
alternativeHeaderParser :: Parser BlockElement
alternativeHeaderParser = do
    _ <- newline
    content <- manyTill freeTextParser newline
    nextLine <- manyTill anyChar newline  -- Read the next line
    case nextLine of
        '=':_ | all (== '=') nextLine -> do
            _ <- many (is '=')
            return $ Header 1 content
        '-':_ | all (== '-') nextLine -> do
            _ <- many (is '-')
            return $ Header 2 content
        _ -> empty

blockQuoteParser :: Parser BlockElement
blockQuoteParser = do
    _ <- inlineSpace
    _ <- is '>'
    _ <- inlineSpace
    firstParagraph <- paragraphElementParser
    -- Parse additional paragraphs within the same blockquote, allowing optional blank lines
    restParagraphs <- many (try $ optional blankLines *> is '>' *> inlineSpace *> paragraphElementParser)
    return $ BlockQuote (firstParagraph : restParagraphs)

-- Parsing code blocks (```language\ncode```)
blockCodeParser :: Parser BlockElement
blockCodeParser = do
    _ <- inlineSpace
    _ <- string "```"
    language <- optionalPlainText
    code <- manyTill anyChar (try $ string "```")
    return $ BlockCode (maybePlainText language) (String code)

-- Parser for multiple consecutive footnote references
footNoteReferencesParser :: Parser BlockElement
footNoteReferencesParser = do
    refs <- some footNoteReferenceParser -- Parse one or more footnote references
    return $ FootNoteReferences refs

-- Modify the original footNoteReferenceParser to return tuple for use in multiple references
footNoteReferenceParser :: Parser (Int, PlainText)
footNoteReferenceParser = do
    _ <- string "[^"
    ref <- some digit
    _ <- string "]:"
    _ <- optional inlineSpace
    content <- manyTill anyChar (try (newline <|> eof))
    return (read ref, (String content))

-- Ordered list parser (1. item)
orderedListParser :: Parser BlockElement
orderedListParser = do
    items <- some orderedListItemParser
    return $ OrderedLists items

-- Parsing ordered list items, with possible nested sublists
orderedListItemParser :: Parser OrderedListItem
orderedListItemParser = do
    -- Parse the main list item
    _ <- some digit
    _ <- string ". "
    content <- some freeTextParser -- Handle multiple `FreeText` elements
    -- Optionally parse a nested sublist
    sublist <- optional (try orderedSubListParser)
    return $ ListItem content (maybe [] id sublist)


-- Parse nested sublist
orderedSubListParser :: Parser [OrderedListItem]
orderedSubListParser = do
    -- Assumes sublists are indented and items begin with a number and dot
    _ <- newline
    indentedItems <- many (string "    ") -- Handle indentation
    sublistItems <- some orderedListItemParser
    return sublistItems

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

imageParser :: Parser BlockElement
imageParser = do
    _ <- inlineSpace
    _ <- string "![" 
    altText <- plainTextParser <* string "]" 
    _ <- inlineSpace
    url <- string "(" *> urlParser 
    _ <- inlineSpace
    caption <- stringTok "\"" *> plainTextParser <* string "\")" 
    return $ Image altText url caption 

urlParser :: Parser PlainText
urlParser = String <$> many (noneof " \n)")

-------------------- Paragraph-level parsers --------------------

-- Paragraph parser
paragraphElementParser :: Parser ParagraphElement
paragraphElementParser = do
    content <- some freeTextParser
    _ <- optional blankLines
    return $ Paragraph content

-- Free text parser (can include plain text or text modifiers)
freeTextParser :: Parser FreeText
freeTextParser = (TextModifier <$> textModifierParser)
             <|> (PlainText <$> plainTextParser)

-- Plain text parser
plainTextParser :: Parser PlainText
plainTextParser = String <$> some (noneof "\n\"*_~`[]()|")

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
    content <- manyTill anyChar (try $ string "**")
    return $ Bold (String content)

-- Italic text parser (_italic_)
italicParser :: Parser TextModifier
italicParser = do
    _ <- is '_'
    content <- manyTill anyChar (is '_')
    return $ Italic (String content)

-- Strikethrough text parser (~~text~~)
strikethroughParser :: Parser TextModifier
strikethroughParser = do
    _ <- string "~~"
    content <- manyTill anyChar (try $ string "~~")
    return $ Strikethrough (String content)

-- Link parser ([text](url))
linkParser :: Parser TextModifier
linkParser = do
    _ <- is '['
    text <- manyTill anyChar (is ']')
    _ <- is '('
    url <- manyTill anyChar (is ')')
    return $ Link (String text) (String url)

-- Inline code parser (`code`)
inlineCodeParser :: Parser TextModifier
inlineCodeParser = do
    _ <- is '`'
    content <- manyTill anyChar (is '`')
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
convertADTHTML :: Document -> String
convertADTHTML (Document elements) = 
    "<!DOCTYPE html>\n" ++
    "<html lang=\"en\">\n\n" ++
    "<head>\n" ++
    "    <meta charset=\"UTF-8\">\n" ++
    "    <title>Test</title>\n" ++
    "</head>\n\n" ++
    "<body>\n" ++
    unlines (map (elementToHTML 1) elements) ++  -- Converts the Document to the body content with indentation level 0
    "</body>\n\n" ++
    "</html>\n"

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
textModifierToHTML (FootNote num) = "<sup><a id=\"fn" ++ show num ++ "ref\" href=\"#fn" ++ show num ++ "\">" ++ show num ++ "</a></sup>"

-- Convert block elements to HTML with indentation level
blockElementToHTML :: Int -> BlockElement -> String
blockElementToHTML level (Header lvl content) = 
    indent level ++ "<h" ++ show lvl ++ ">" ++ concatMap freeTextToHTML content ++ "</h" ++ show lvl ++ ">"
blockElementToHTML level (BlockQuote paragraphs) = 
    indent level ++ "<blockquote>\n" ++ concatMap (\p -> paragraphToHTML (level + 1) p ++ "\n") paragraphs ++ indent level ++ "</blockquote>"
blockElementToHTML level (BlockCode (String lang) (String code)) = 
    concat [indent level, "<pre><code", if null lang then ">" else " class=\"language-" ++ lang ++ "\">", processCode (trimLeadingNewlines code), "</code></pre>"]
blockElementToHTML level (FootNoteReference num (String text)) = 
    indent level ++ "<p id=\"fn" ++ show num ++ "\">" ++ text ++ "</p>"
blockElementToHTML level (FootNoteReferences num) = 
    concatMapSepBy (\(num, content) -> blockElementToHTML level (FootNoteReference num content)) "\n" num
blockElementToHTML level (OrderedLists items) = 
    orderedListToHTML level items
blockElementToHTML level (Table (TableHeader header) rows) = 
    indent level ++ "<table>\n" ++
    indent (level + 1) ++ "<thead>\n" ++
    headerRowToHTML level header ++
    indent (level + 1) ++ "</thead>\n" ++
    indent (level + 1) ++ "<tbody>\n" ++
    concatMap (tableRowToHTML level) rows ++ 
    indent (level + 1) ++ "</tbody>\n" ++
    indent level ++ "</table>"
blockElementToHTML level (Image (String alt) (String url) (String caption)) = 
    indent level ++ "<img src=\"" ++ url ++ "\" alt=\"" ++ alt ++ "\" title=\"" ++ caption ++ "\">"

orderedListToHTML :: Int -> [OrderedListItem] -> String
orderedListToHTML level items = 
    indent level ++ "<ol>\n" ++ concatMap (orderedListItemToHTML (level + 1)) items ++ indent level ++ "</ol>\n"

orderedListItemToHTML :: Int -> OrderedListItem -> String
orderedListItemToHTML level (ListItem content sublist) = 
    if null sublist
        then indent level ++ "<li>" ++ concatMap freeTextToHTML content ++ "</li>\n"
        else indent level ++ "<li>" ++ concatMap freeTextToHTML content ++ "</li>\n" ++ indent level ++ "<ol>\n" ++ concatMap (orderedListItemToHTML (level)) sublist ++ indent level ++ "</ol>\n"

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

-- Helper function to process the code
processCode :: String -> String
processCode code = 
    case lines code of
        [] -> ""
        (firstLine:restLines) -> firstLine ++ concatMap ("\n" ++) restLines

-- Helper function to trim leading newlines
trimLeadingNewlines :: String -> String
trimLeadingNewlines = dropWhile (`elem` "\n\r")

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

blankLines :: Parser ()
blankLines = do
    _ <- many (void newline <|> void (string "\n\r"))
    return ()

newline :: Parser Char
newline = is '\n'

sepBy :: Parser a -> Parser sep -> Parser [a]
sepBy p sep = (:) <$> p <*> many (sep *> p) <|> pure []

try :: Parser a -> Parser a
try (Parser p) = Parser $ \input ->
  case p input of
    Error _ -> Error UnexpectedEof
    result  -> result

-- Custom 'eof' parser that checks if input is exhausted
eof :: Parser Char
eof = Parser $ \input ->
    case input of
        "" -> Result "" ' '  -- If the input is empty, we succeed with unit
        _  -> Error (ExpectedEof input)  -- Otherwise, we fail with ExpectedEof

digit :: Parser Char
digit = satisfy isDigit

-- Helper function to concatenate elements with a separator
concatMapSepBy :: (a -> String) -> String -> [a] -> String
concatMapSepBy f sep xs = concat (intersperse sep (map f xs))

----------------------------------------------------------------------------

writeTextToFile :: String -> IO ()
writeTextToFile text = do
    time <- getTime
    writeFile (time ++ ".html") text

getTime :: IO String
getTime = formatTime defaultTimeLocale "%Y-%m-%dT%H.%M.%S" <$> getCurrentTime

