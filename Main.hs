import Data.Time
import System.IO
import Data.List
import Control.Monad
import System.Directory (doesFileExist)

data BookLoan = BookLoan {
    bookId :: String,
    borrowTime :: UTCTime,
    returnTime :: Maybe UTCTime
} deriving (Show, Read)

fileName :: FilePath
fileName = "loans.txt"

-- Load loans from the file
loadLoans :: IO [BookLoan]
loadLoans = do
    exists <- doesFileExist fileName
    if exists
        then do
            content <- readFile fileName
            return (if null content then [] else read content)
        else return []

-- Save loans to the file
saveLoans :: [BookLoan] -> IO ()
saveLoans loans = writeFile fileName (show loans)

-- Register a new loan
registerLoan :: [BookLoan] -> IO [BookLoan]
registerLoan loans = do
    putStrLn "Enter Book ID to borrow:"
    bid <- getLine
    now <- getCurrentTime
    let newLoan = BookLoan bid now Nothing
    let updated = loans ++ [newLoan]
    saveLoans updated
    putStrLn "Loan registered."
    return updated

-- Search for a loan by Book ID
searchLoanById :: [BookLoan] -> IO ()
searchLoanById loans = do
    putStrLn "Enter Book ID to search:"
    bid <- getLine
    let results = filter (\l -> bookId l == bid && returnTime l == Nothing) loans
    if null results
        then putStrLn "No active loan found for that ID."
        else mapM_ print results

-- Calculate the duration of a loan
calculateDuration :: BookLoan -> IO ()
calculateDuration (BookLoan bid borrow Nothing) = do
    now <- getCurrentTime
    putStrLn $ "Book " ++ bid ++ " has been on loan for " ++ show (diffUTCTime now borrow) ++ " seconds."
calculateDuration (BookLoan bid borrow (Just ret)) =
    putStrLn $ "Book " ++ bid ++ " was on loan for " ++ show (diffUTCTime ret borrow) ++ " seconds."

-- List all borrowed books
listBorrowedBooks :: [BookLoan] -> IO ()
listBorrowedBooks loans = do
    let borrowed = filter (\l -> returnTime l == Nothing) loans
    if null borrowed
        then putStrLn "No books currently borrowed."
        else mapM_ print borrowed

-- Register a return
registerReturn :: [BookLoan] -> IO [BookLoan]
registerReturn loans = do
    putStrLn "Enter Book ID to return:"
    bid <- getLine
    now <- getCurrentTime
    let updated = map (\l -> if bookId l == bid && returnTime l == Nothing
                             then l { returnTime = Just now }
                             else l) loans
    saveLoans updated
    putStrLn "Book returned."
    return updated

-- Menu
mainMenu :: [BookLoan] -> IO ()
mainMenu loans = do
    putStrLn "\nLibrary Loan System"
    putStrLn "1. Register Loan"
    putStrLn "2. Search Loan by ID"
    putStrLn "3. List Borrowed Books"
    putStrLn "4. Register Return"
    putStrLn "5. Exit"
    putStrLn "Choose an option:"
    opt <- getLine
    case opt of
        "1" -> registerLoan loans >>= mainMenu
        "2" -> searchLoanById loans >> mainMenu loans
        "3" -> listBorrowedBooks loans >> mainMenu loans
        "4" -> registerReturn loans >>= mainMenu
        "5" -> putStrLn "Goodbye! :)"
        _   -> putStrLn "Invalid option. :(" >> mainMenu loans

main :: IO ()
main = do
    loans <- loadLoans
    mainMenu loans
