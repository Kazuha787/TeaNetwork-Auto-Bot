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
