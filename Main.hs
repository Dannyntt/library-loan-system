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

-- Cargar prestamo desde el archivo
loadLoans :: IO [BookLoan]
loadLoans = do
    exists <- doesFileExist fileName
    if exists
        then do
            content <- readFile fileName
            return (if null content then [] else read content)
        else return []

-- Guardar prestamo en el archivo
saveLoans :: [BookLoan] -> IO ()
saveLoans loans = writeFile fileName (show loans)

-- Para regsistrar el prestamo
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

-- buscar prestamo por ID
searchLoanById :: [BookLoan] -> IO ()
searchLoanById loans = do
    putStrLn "Enter Book ID to search:"
    bid <- getLine
    let results = filter (\l -> bookId l == bid && returnTime l == Nothing) loans
    if null results
        then putStrLn "No active loan found for that ID."
        else mapM_ print results

-- Calcular la duración del préstamo
calculateDuration :: BookLoan -> IO ()
calculateDuration (BookLoan bid borrow Nothing) = do
    now <- getCurrentTime
    putStrLn $ "Book " ++ bid ++ " has been on loan for " ++ show (diffUTCTime now borrow) ++ " seconds."
calculateDuration (BookLoan bid borrow (Just ret)) =
    putStrLn $ "Book " ++ bid ++ " was on loan for " ++ show (diffUTCTime ret borrow) ++ " seconds."

-- listar libros prestados
listBorrowedBooks :: [BookLoan] -> IO ()
listBorrowedBooks loans = do
    let borrowed = filter (\l -> returnTime l == Nothing) loans
    if null borrowed
        then putStrLn "No books currently borrowed."
        else mapM_ print borrowed

-- registrar la devolución de un libro
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

-- Load books from Library.txt
loadBooks :: FilePath -> IO [(String, String)]
loadBooks path = do
    exists <- doesFileExist path
    if not exists
        then do
            putStrLn "Library.txt not found!"
            return []
        else do
            content <- readFile path
            let parseLine line = case break (== ',') line of
                                    (bid, ',' : title) -> (bid, title)
                                    _ -> ("", "")
            return $ filter (\(bid, title) -> not (null bid) && not (null title)) $ map parseLine (lines content)

-- List available books
listAvailableBooks :: [(String, String)] -> [BookLoan] -> IO ()
listAvailableBooks books loans = do
    let borrowedIds = [bookId l | l <- loans, returnTime l == Nothing]
        available = filter (\(bid, _) -> bid `notElem` borrowedIds) books
    if null available
        then putStrLn "No books available."
        else do
            putStrLn "Available books:"
            mapM_ (\(bid, title) -> putStrLn $ bid ++ ": " ++ title) available

-- List all loans with their durations
listLoanDurations :: [BookLoan] -> IO ()
listLoanDurations loans = do
    now <- getCurrentTime
    forM_ loans $ \loan ->
        case returnTime loan of
            Just ret -> do
                let duration = diffUTCTime ret (borrowTime loan)
                putStrLn $ "Book " ++ bookId loan ++ " was on loan for " ++ show duration ++ " seconds."
            Nothing -> do
                let duration = diffUTCTime now (borrowTime loan)
                putStrLn $ "Book " ++ bookId loan ++ " has been on loan for " ++ show duration ++ " seconds."

-- Menu
mainMenu :: [(String, String)] -> [BookLoan] -> IO ()
mainMenu books loans = do
    putStrLn "\nLibrary Loan System"
    putStrLn "1. Register Loan"
    putStrLn "2. Search Loan by ID"
    putStrLn "3. List Borrowed Books"
    putStrLn "4. Register Return"
    putStrLn "5. List Available Books"
    putStrLn "6. List Loan Durations"
    putStrLn "7. Exit"
    putStrLn "Choose an option:"
    opt <- getLine
    case opt of
        "1" -> registerLoan loans >>= mainMenu books
        "2" -> searchLoanById loans >> mainMenu books loans
        "3" -> listBorrowedBooks loans >> mainMenu books loans
        "4" -> registerReturn loans >>= mainMenu books
        "5" -> listAvailableBooks books loans >> mainMenu books loans
        "6" -> listLoanDurations loans >> mainMenu books loans
        "7" -> putStrLn "Goodbye! :)"
        _   -> putStrLn "Invalid option. :(" >> mainMenu books loans

main :: IO ()
main = do
    books <- loadBooks "Library.txt"
    loans <- loadLoans
    mainMenu books loans
