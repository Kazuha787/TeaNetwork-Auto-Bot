import System.IO
import System.Directory
import Data.List
import Control.Monad (when)

todoFile :: FilePath
todoFile = "todo.txt"

main :: IO ()
main = do
    putStrLn "Welcome to the Haskell To-Do App!"
    menuLoop

menuLoop :: IO ()
menuLoop = do
    putStrLn "\nMenu:"
    putStrLn "1. View To-Do List"
    putStrLn "2. Add Item"
    putStrLn "3. Remove Item"
    putStrLn "4. Clear All"
    putStrLn "5. Exit"
    putStr "Choose an option: "
    hFlush stdout
    choice <- getLine
    case choice of
        "1" -> viewItems >> menuLoop
        "2" -> addItem >> menuLoop
        "3" -> removeItem >> menuLoop
        "4" -> clearItems >> menuLoop
        "5" -> putStrLn "Goodbye!"
        _   -> putStrLn "Invalid option." >> menuLoop

viewItems :: IO ()
viewItems = do
    exists <- doesFileExist todoFile
    if exists
        then do
            contents <- readFile todoFile
            let items = lines contents
            if null items
                then putStrLn "Your to-do list is empty."
                else putStrLn $ unlines $ zipWith (\i item -> show i ++ ". " ++ item) [1..] items
        else putStrLn "Your to-do list is empty."

addItem :: IO ()
addItem = do
    putStr "Enter item to add: "
    hFlush stdout
    item <- getLine
    appendFile todoFile (item ++ "\n")
    putStrLn "Item added!"

removeItem :: IO ()
removeItem = do
    exists <- doesFileExist todoFile
    if not exists
        then putStrLn "List is empty."
        else do
            contents <- readFile todoFile
            let items = lines contents
            if null items
                then putStrLn "Nothing to remove!"
                else do
                    viewItems
                    putStr "Enter item number to remove: "
                    hFlush stdout
                    numberStr <- getLine
                    let number = read numberStr :: Int
                    if number > 0 && number <= length items
                        then do
                            let newItems = delete (items !! (number - 1)) items
                            writeFile todoFile (unlines newItems)
                            putStrLn "Item removed!"
                        else putStrLn "Invalid item number."

clearItems :: IO ()
clearItems = do
    putStr "Are you sure you want to clear all items? (yes/no): "
    hFlush stdout
    confirm <- getLine
    when (map toLower confirm == "yes") $ do
        writeFile todoFile ""
        putStrLn "All items cleared!"

{-# LANGUAGE OverloadedStrings #-}

import Data.Char (isSpace)
import Data.List (isPrefixOf, dropWhileEnd)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment (getArgs)

data Markdown
    = Heading Int T.Text
    | Paragraph T.Text
    | Bold T.Text
    | Italic T.Text
    | List [T.Text]
    | CodeBlock [T.Text]
    deriving Show

-- Main program
main :: IO ()
main = do
    args <- getArgs
    case args of
        [filename] -> do
            content <- TIO.readFile filename
            let parsed = parseMarkdown $ T.lines content
            TIO.putStrLn $ renderHTML parsed
        _ -> putStrLn "Usage: runhaskell MarkdownParser.hs <filename.md>"

-- Parse the entire Markdown document
parseMarkdown :: [T.Text] -> [Markdown]
parseMarkdown [] = []
parseMarkdown (l:ls)
    | T.null l = parseMarkdown ls
    | isHeading l = let (h, rest) = parseHeading (l:ls) in h : parseMarkdown rest
    | isCodeBlockStart l = let (cb, rest) = parseCodeBlock (l:ls) in cb : parseMarkdown rest
    | isListItem l = let (li, rest) = parseList (l:ls) in li : parseMarkdown rest
    | otherwise = let (p, rest) = parseParagraph (l:ls) in p : parseMarkdown rest

-- Helpers to identify line types
isHeading :: T.Text -> Bool
isHeading = T.isPrefixOf "#"

isListItem :: T.Text -> Bool
isListItem = T.isPrefixOf "- "

isCodeBlockStart :: T.Text -> Bool
isCodeBlockStart = (=="```")

-- Parse heading
parseHeading :: [T.Text] -> (Markdown, [T.Text])
parseHeading (l:ls) =
    let hashes = T.takeWhile (== '#') l
        level = T.length hashes
        content = T.strip $ T.dropWhile (== '#') l
    in (Heading level content, ls)
parseHeading [] = error "Unexpected end in parseHeading"

-- Parse paragraph
parseParagraph :: [T.Text] -> (Markdown, [T.Text])
parseParagraph ls =
    let (paraLines, rest) = break T.null ls
        paragraph = T.intercalate " " paraLines
    in (Paragraph (formatInline paragraph), dropWhile T.null rest)

-- Parse list
parseList :: [T.Text] -> (Markdown, [T.Text])
parseList ls =
    let (items, rest) = span isListItem ls
        cleanItems = map (T.strip . T.drop 2) items
    in (List cleanItems, dropWhile T.null rest)

-- Parse code block
parseCodeBlock :: [T.Text] -> (Markdown, [T.Text])
parseCodeBlock (_:ls) =
    let (codeLines, rest) = break (=="```") ls
    in (CodeBlock codeLines, drop 1 rest)
parseCodeBlock [] = error "Unexpected end in parseCodeBlock"

-- Inline format
formatInline :: T.Text -> T.Text
formatInline = italicize . bolden
  where
    bolden = replaceAll "**" "<b>" "</b>"
    italicize = replaceAll "*" "<i>" "</i>"

-- Replace markdown syntax with HTML
replaceAll :: T.Text -> T.Text -> T.Text -> T.Text -> T.Text
replaceAll token openTag closeTag txt =
    let parts = T.splitOn token txt
        formatted = zipWith (\i t -> if even i then t else openTag <> t <> closeTag) [0..] parts
    in T.concat formatted

-- Render parsed markdown to HTML
renderHTML :: [Markdown] -> T.Text
renderHTML = T.concat . map renderBlock

renderBlock :: Markdown -> T.Text
renderBlock (Heading n t) = T.concat ["<h", sn, ">", t, "</h", sn, ">\n"]
  where sn = T.pack (show n)
renderBlock (Paragraph t) = T.concat ["<p>", t, "</p>\n"]
renderBlock (Bold t) = T.concat ["<b>", t, "</b>"]
renderBlock (Italic t) = T.concat ["<i>", t, "</i>"]
renderBlock (List items) = T.concat ["<ul>\n", T.concat (map (\i -> T.concat ["<li>", i, "</li>\n"]) items), "</ul>\n"]
renderBlock (CodeBlock lines) = T.concat ["<pre><code>\n", T.unlines lines, "</code></pre>\n"]
