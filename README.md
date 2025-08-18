# Library Loan System

## Group Members
- Daniela Giraldo Salas
- Sebastian Urrego Ramirez

## Platform Used
- Windows 10/11
- Developed using [Visual Studio Code](https://code.visualstudio.com/) with Haskell extensions

## Project Description
This is a simple library loan management system developed 100% in Haskell.  
It allows users to:
- Register book loans
- Search for active loans by book ID
- List currently borrowed books
- Register book returns
- View available books
- Display the duration each book was on loan

All loan data is persisted in a text file (`loans.txt`).  
The program loads existing records at startup and saves changes automatically.

## Files Included
- `Main.hs` — Main source code for the application
- `loans.txt` — Sample records for book loans (created automatically by the program)
- `Library.txt` — Contains sample book records (see below)

## Sample Book Records (`Library.txt`)
```
B001,The Hobbit
B002,1984
B003,Pride and Prejudice
B004,To Kill a Mockingbird
B005,The Catcher in the Rye
```

## How to Run
1. Clone the repository:
    ```
    git clone https://github.com/Dannyntt/library-loan-system.git

    ```
2. Open the folder in Visual Studio Code.
3. Make sure you have the Haskell extension installed.
4. Run the program using:
    ```
    runghc Main.hs
    ```
5. Follow the menu prompts in the terminal.
