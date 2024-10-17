module Assignment (markdownParser, convertADTHTML, generateHTML) where


import           Data.Time.Clock  (getCurrentTime)
import           Data.Time.Format (defaultTimeLocale, formatTime)
import           Data.Char        (isDigit, isAlpha, isSpace)
import           Instances        (Parser (..), ParseResult (..), ParseError (..))
import           Parser           ( some, many, char, is, string, space, spaces1, noneof, satisfy )
import           Control.Applicative ((<|>), optional, empty)

-- Algebraic Data Type to represent parsed Markdown components
data ADT 
    = Heading Int String
    | Paragraph [ADT]   -- Paragraph can contain inline text modifiers
    | Bold String
    | Italic String
    | Strikethrough String
    | Link String String  -- Link text and URL
    | InlineCode String
    | Image String String String  -- Alt text, URL, Caption
    | Footnote Int String  -- Footnote number and reference text
    | Blockquote [ADT]
    | CodeBlock (Maybe String) String  -- Optional language and code content
    | OrderedList [[ADT]]  -- A list of list items, each containing a list of ADT
    | Table [[String]]  -- Table rows and columns
    | FreeText String   -- Free text for any non-special Markdown content
    deriving (Show, Eq)

-- Parsing a heading
headingParser :: Parser ADT
headingParser = do
    level <- length <$> some (is '#')
    _ <- is ' '  -- at least one space is required
    content <- manyTill anyChar newline
    return $ Heading level content

-- Parsing a paragraph (free text and inline modifiers)
paragraphParser :: Parser ADT
paragraphParser = do
    content <- manyTill anyChar newline
    return $ Paragraph [FreeText content]  -- FreeText wrapped in Paragraph

-- Parsing bold text (**bold**)
boldParser :: Parser ADT
boldParser = do
    _ <- string "**"
    content <- manyTill anyChar (try $ string "**")
    return $ Bold content

-- Parsing italic text (_italic_)
italicParser :: Parser ADT
italicParser = do
    _ <- is '_'
    content <- manyTill anyChar (is '_')
    return $ Italic content

-- Parsing strikethrough text (~~strikethrough~~)
strikethroughParser :: Parser ADT
strikethroughParser = do
    _ <- string "~~"
    content <- manyTill anyChar (try $ string "~~")
    return $ Strikethrough content

-- Parsing inline code (`code`)
inlineCodeParser :: Parser ADT
inlineCodeParser = do
    _ <- is '`'
    content <- manyTill anyChar (is '`')
    return $ InlineCode content

-- Parsing a link ([link text](url))
linkParser :: Parser ADT
linkParser = do
    _ <- is '['
    linkText <- manyTill anyChar (is ']')
    _ <- is '('
    url <- manyTill anyChar (is ')')
    return $ Link linkText url

-- Parsing an image (![alt text](url "caption"))
imageParser :: Parser ADT
imageParser = do
    _ <- string "!["
    altText <- manyTill anyChar (is ']')
    _ <- is '('
    url <- manyTill anyChar (is ' ')
    caption <- manyTill anyChar (is ')')
    return $ Image altText url caption

-- Parsing a blockquote (> blockquote)
blockquoteParser :: Parser ADT
blockquoteParser = do
    _ <- is '>'
    content <- manyTill anyChar newline
    return $ Blockquote [FreeText content]  -- Blockquote contains free text

-- Parsing a code block (``` code ```)
codeBlockParser :: Parser ADT
codeBlockParser = do
    _ <- string "```"
    language <- optional (manyTill anyChar newline)
    content <- manyTill anyChar (try $ string "```")
    return $ CodeBlock language content

-- Parsing an ordered list (1. item)
orderedListParser :: Parser ADT
orderedListParser = do
    items <- some orderedListItemParser
    return $ OrderedList items

-- Helper function to parse an ordered list item
orderedListItemParser :: Parser [ADT]
orderedListItemParser = do
    _ <- some digit
    _ <- string ". "
    content <- manyTill anyChar newline
    return [FreeText content]

-- Parsing a table (| col1 | col2 |)
tableParser :: Parser ADT
tableParser = do
    rows <- some tableRowParser
    return $ Table rows

-- Helper function to parse a table row
tableRowParser :: Parser [String]
tableRowParser = do
    _ <- is '|'
    cells <- cellParser `sepBy` is '|'
    _ <- is '|'
    return cells

-- Helper function to parse a table cell
cellParser :: Parser String
cellParser = many (noneof "|")

-- Parsing free text (any non-special markdown text)
freeTextParser :: Parser ADT
freeTextParser = FreeText <$> manyTill anyChar newline

-- !!!!!!!!!
-- freeTextParser => textmodifier
-- plainTextParser => normalText

-- Combined markdown parser
markdownParser :: Parser ADT
markdownParser = do
    elements <- many (choice [headingParser, paragraphParser, boldParser, italicParser, strikethroughParser, linkParser, inlineCodeParser, imageParser, blockquoteParser, codeBlockParser, orderedListParser, tableParser, freeTextParser])
    return $ Paragraph elements  -- Wrap everything inside a root Paragraph



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

choice :: [Parser a] -> Parser a
choice = foldr (<|>) empty




-- Helper function to indent HTML content
indent :: Int -> String -> String
indent level content = replicate (level * 4) ' ' ++ content

-- Converts ADT into an HTML string
convertADTHTML :: ADT -> String
convertADTHTML adt = wrapHTML (generateHTML 0 adt)

-- Wraps generated HTML inside proper HTML structure
wrapHTML :: String -> String
wrapHTML body = unlines
    [ "<!DOCTYPE html>"
    , "<html lang=\"en\">"
    , "<head>"
    , "    <meta charset=\"UTF-8\">"
    , "    <title>Markdown</title>"
    , "</head>"
    , "<body>"
    , body
    , "</body>"
    , "</html>"
    ]

-- Recursive function to generate HTML from ADT
generateHTML :: Int -> ADT -> String
generateHTML level (Heading n content) = indent level ("<h" ++ show n ++ ">" ++ content ++ "</h" ++ show n ++ ">")
generateHTML level (Paragraph elements) = unlines (map (generateHTML (level + 1)) elements)
generateHTML level (Bold content) = indent level ("<strong>" ++ content ++ "</strong>")
generateHTML level (Italic content) = indent level ("<em>" ++ content ++ "</em>")
generateHTML level (Strikethrough content) = indent level ("<del>" ++ content ++ "</del>")
generateHTML level (Link text url) = indent level ("<a href=\"" ++ url ++ "\">" ++ text ++ "</a>")
generateHTML level (InlineCode content) = indent level ("<code>" ++ content ++ "</code>")
generateHTML level (Image alt url caption) = indent level ("<img src=\"" ++ url ++ "\" alt=\"" ++ alt ++ "\" title=\"" ++ caption ++ "\">")
generateHTML level (Footnote n content) = indent level ("<sup><a id=\"fn" ++ show n ++ "ref\" href=\"#fn" ++ show n ++ "\">" ++ show n ++ "</a></sup>")
generateHTML level (Blockquote elements) = indent level ("<blockquote>\n" ++ unlines (map (generateHTML (level + 1)) elements) ++ indent level "</blockquote>")
generateHTML level (CodeBlock (Just lang) content) = indent level ("<pre><code class=\"language-" ++ lang ++ "\">\n" ++ content ++ "\n</code></pre>")
generateHTML level (CodeBlock Nothing content) = indent level ("<pre><code>\n" ++ content ++ "\n</code></pre>")
generateHTML level (OrderedList items) = indent level "<ol>\n" ++ concatMap (generateListItem (level + 1)) items ++ indent level "</ol>"
generateHTML level (Table rows) = indent level "<table>\n" ++ unlines (map (generateTableRow (level + 1)) rows) ++ indent level "</table>"
generateHTML level (FreeText content) = indent level ("<p>" ++ content ++ "</p>")

-- Helper function to generate HTML for a list item
generateListItem :: Int -> [ADT] -> String
generateListItem level item = indent level "<li>" ++ unlines (map (generateHTML (level + 1)) item) ++ indent level "</li>"

-- Helper function to generate HTML for a table row
generateTableRow :: Int -> [String] -> String
generateTableRow level row = indent level "<tr>\n" ++ concatMap (generateTableCell (level + 1)) row ++ indent level "</tr>"

-- Helper function to generate HTML for a table cell
generateTableCell :: Int -> String -> String
generateTableCell level content = indent level ("<td>" ++ content ++ "</td>")

getTime :: IO String
getTime = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S" <$> getCurrentTime

