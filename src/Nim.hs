import           Data.Char

-- Board utils

type Board = [Int]

initial :: Board
initial = reverse [1..5]

finished :: Board -> Bool
finished = all (== 0)

valid :: Board -> Int -> Int -> Bool
valid board row num = board !! (row - 1) >= num

move :: Board -> Int -> Int -> Board
move board row num = [ update r n | (r,n) <- zip [1..] board ]
                where update r n = if r == row then n - num else n

-- IO utils

newline :: IO ()
newline = putChar '\n'

stars :: Int -> String
stars n = concat (replicate n "* ")

putRow :: Int -> Int -> IO ()
putRow row num = do
    putStr (show row)
    putStr ": "
    putStrLn (stars num)

putBoard :: Board -> IO ()
putBoard ns = sequence_ [putRow r n | (r,n) <- zip [1..] ns]

getDigit :: String -> IO Int
getDigit msg = do
    putStr msg
    newline
    x <- getChar
    newline
    if isDigit x then
        return (digitToInt x)
    else do
        newline
        putStrLn "Error: Invalid digit"
        getDigit msg

-- Nim game
next :: Int -> Int
next 1 = 2
next 2 = 1

play :: Board -> Int -> IO ()
play board player = do
    newline
    putBoard board
    if finished board then do
        newline
        putStr "Player"
        putStr (show (next player))
        putStrLn " wins!"
    else do
        newline
        putStr "Player"
        putStrLn (show player)
        r <- getDigit "Enter a row number: "
        n <- getDigit "Enter stars to remove: "
        if valid board r n then
            -- update the board and player
            play (move board r n) (next player)
        else do
            newline
            putStrLn "Error: Invalid move"
            play board player

nim :: IO ()
nim = play initial 1
