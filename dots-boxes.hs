import System.Random


-- DATA STRUCTURES


type Point = (Int, Int)
type Edge = (Point, Point)
type Box = ([Edge], Int)
type Board = [[Box]]
type Strategy = (Board -> [Edge] -> IO Edge)


-- GAME LOGIC

-- Switch player
changePlayer :: Int -> Int
changePlayer 1 = 2
changePlayer 2 = 1

changeStrategy :: (Strategy, Strategy) -> (Strategy, Strategy)
changeStrategy (s1,s2) = (s2,s1)

-- Change the input format to the game format
normalizeEdge :: [String] -> String -> Edge
normalizeEdge [c1,c2] d
  | d == "v"  = ((x,y),(x+1,y))
  | d == "h"  = ((x,y),(x,y+1))
  | otherwise = ((x,y),(x,y))
    where x = read c1 :: Int
          y = read c2 :: Int


-- BOARD UPDATING

-- Put the edge in the board, if that edge makes an square, the boolean is set to true
updateBoard :: Board -> Edge -> Int -> IO (Board, Bool)
updateBoard b e p
  | isHorizontal e = return (moveHorizontal b e p)
  | otherwise      = return (moveVertical b e p)

moveHorizontal :: Board -> Edge -> Int -> (Board, Bool)
moveHorizontal b e@((p1,p2),(q1,q2)) p
  | p1 == 0        = (y1 ++ tail b, y2)
  | p1 == length b = (init b ++ x1, x2)
  | otherwise      = (take (p1-1) b ++ x1 ++ y1 ++ drop (p1+1) b, x2 || y2)
    where x  = fst $ (b !! (p1-1)) !! p2
          x1 = [take p2 (b !! (p1-1)) ++ [(e:x, if x2 then p else 0)] ++ drop (p2+1) (b !! (p1-1))]
          x2 = length x == 3
          y  = fst $ (b !! p1) !! p2
          y1 = [take p2 (b !! p1) ++ [(e:y, if y2 then p else 0)] ++ drop (p2+1) (b !! p1)]
          y2 = length y == 3

moveVertical :: Board -> Edge -> Int -> (Board, Bool)
moveVertical b e@((p1,p2),(q1,q2)) p
  | p2 == 0               = (take (max 0 p1) b ++ [a1] ++ drop (min (length (head b)) (p1+1)) b, y1)
  | p2 == length (head b) = (take (max 0 p1) b ++ [a2] ++ drop (min (length (head b)) (p1+1)) b, x1)
  | otherwise             = (take (max 0 p1) b ++ [a3] ++ drop (min (length (head b)) (p1+1)) b, x1 || y1)
    where x  = fst $ (b !! p1) !! (p2-1)
          x1 = length x == 3
          y  = fst $ (b !! p1) !! p2
          y1 = length y == 3
          z  = b !! p1
          a1 = (e:y, if y1 then p else 0) : tail z
          a2 = init z ++ [(e:x, if x1 then p else 0)]
          a3 = take (p2-1) z ++ [(e:x, if x1 then p else 0)] ++ [(e:y, if y1 then p else 0)] ++ drop (p2+1) z


-- BOARD CHECKS


isAvailable :: Edge -> [Edge] -> Bool
isAvailable = elem

isHorizontal :: Edge -> Bool
isHorizontal ((_,p1),(_,p2)) = abs (p1-p2) == 1

isValid :: Edge -> Bool
isValid ((p1,p2),(q1,q2)) = x || y
  where x = abs (p1-q1) == 1 && p2 == q2
        y = abs (p2-q2) == 1 && p1 == q1

scores :: Board -> (Int,Int)
scores b = (length p1, length p2)
  where p1 = foldl (\x y -> x ++ filter (\z -> snd z == 1) y) [] b
        p2 = foldl (\x y -> x ++ filter (\z -> snd z == 2) y) [] b


-- GAME INIT


buildBoard :: Int -> Int -> Board
buildBoard 0 _ = []
buildBoard v h = buildRow h : buildBoard (v-1) h 

buildRow :: Int -> [Box]
buildRow 0 = []
buildRow h = ([],0) : buildRow (h-1)

buildAvailables :: Int -> Int -> [Edge]
buildAvailables v h = horizontal ++ vertical
  where horizontal = [((x,y),(x,y+1)) | x <- [0..v], y <- [0..(h-1)]]
        vertical = [((x,y),(x+1,y)) | x <- [0..(v-1)], y <- [0..h]]

deleteAvailable :: Edge -> [Edge] -> [Edge]
deleteAvailable e = filter (/= e)


-- BOARD REPRESENTATION


boardToString :: Board -> String
boardToString b = x ++ "\n" ++ fst (foldl printBoxes ("",(0,0)) b)
  where x = fst $ foldl printHorizontal ("*",(0,0)) $ head b

printBoxes :: (String,Point) -> [Box] -> (String,Point)
printBoxes (s,(p1,p2)) lb = (s++x++"\n"++y++"\n",(p1+1,p2))
  where z = if ((p1,p2),(p1+1,p2)) `elem` fst (head lb) then "-" else " "
        x = fst $ foldl printVertical (z,(p1,p2)) lb
        y = fst $ foldl printHorizontal ("*",(p1+1,p2)) lb

printHorizontal :: (String,Point) -> Box -> (String,Point)
printHorizontal (s,(p1,p2)) (le,_) = if ((p1,p2),(p1,p2+1)) `elem` le then (s++" - *",(p1,p2+1)) else (s++"   *",(p1,p2+1))

printVertical :: (String,Point) -> Box -> (String,Point)
printVertical (s,(p1,p2)) (le,n) = if ((p1,p2+1),(p1+1,p2+1)) `elem` le then (s++" "++show n++" -",(p1,p2+1)) else (s++" "++show n++"  ",(p1,p2+1))

printBoard :: Board -> IO ()
printBoard b = do putStrLn ""
                  putStrLn $ boardToString b
                  putStrLn ""


-- GAME


main :: IO ()
main = do intro
          putStrLn "Game definition:"
          putStrLn "What is the size of the board in boxes? (vertical horizontal)"
          size <- getLine
          let v = read (head $ words size) :: Int
          let h = read (last $ words size) :: Int
          let board = buildBoard v h
          let availables = buildAvailables v h
          player1 <- selectPlayer 1
          player2 <- selectPlayer 2
          putStrLn ""
          run 1 (player1,player2) board availables

intro :: IO ()
intro = do  putStrLn "********************"
            putStrLn "**  DOTS & BOXES! **"
            putStrLn "********************"
            putStrLn ""
            putStrLn "How to play:"
            putStrLn "- Each turn you have to specify the line you want to draw"
            putStrLn "- Each line is defined by the starting point and the direction of the line"
            putStrLn "- Each point it's defined as two integers indicating it's coordinates"
            putStrLn "- The coordinates of the board start at (0,0) in the upper left corner"
            putStrLn "- The direction of the line could be horizontal (h) or vertical (v) (in lowercase)"
            putStrLn "- Input example: 1 0 v (vertical line starting at coordinate (1,0))"
            putStrLn "- When a player completes a box, he moves again"
            putStrLn "- The game ends when all the boxes are completed"
            putStrLn ""
            example

example :: IO ()
example = do  putStrLn "This is an example of a board and it's coordinates"
              putStrLn " (0,0) (0,1) (0,2)"
              putStrLn " (1,0) (1,1) (1,2)"
              putStrLn " (2,0) (2,1) (2,2)"
              putStrLn ""
              putStrLn "Board representation:"
              putStrLn " *   It's a point"
              putStrLn " -   It's a line made by a player"
              putStrLn " 0   This box isn't complete yet"
              putStrLn " 1   Player 1 completed that box"
              putStrLn " 2   Player 2 completed that box"
              putStrLn ""

selectPlayer :: Int -> IO Strategy
selectPlayer n = do putStrLn $ "Choose the player " ++ show n
                    putStrLn "1. Human"
                    putStrLn "2. CPU easy"
                    putStrLn "3. CPU medium"
                    putStrLn "4. CPU hard"
                    n <- getLine
                    getPlayer (read n)

getPlayer :: Int -> IO Strategy
getPlayer 1 = return human
getPlayer 2 = return easy
getPlayer 3 = return medium
getPlayer 4 = return hard

-- The strategy is used to select the edge to be played
run :: Int -> (Strategy,Strategy) -> Board -> [Edge] -> IO ()
run _ _ b [] = gameFinished b
run p str b le = do putStrLn $ "Turn to Player " ++ show p
                    edge <- fst str b le
                    putStrLn $ "Player " ++ show p ++ " moves to " ++ show edge
                    nb <- updateBoard b edge p
                    if snd nb then do putStrLn "SQUARE!!!"
                                      run p str (fst nb) (deleteAvailable edge le )
                    else run (changePlayer p) (changeStrategy str) (fst nb) (deleteAvailable edge le)

gameFinished :: Board -> IO ()
gameFinished b = do let (p1,p2) = scores b
                    putStrLn ""
                    putStrLn "GAME FINISHED!"
                    putStrLn ""
                    printBoard b
                    putStrLn "SCORES:"
                    putStr "Player 1: "
                    print p1
                    putStr "Player 2: "
                    print p2
                    putStrLn ""


-- STRATEGIES

-- Human plays. Input consist in 2 integers representing the starting coordinate
-- and a character (h or v) indicating if the line is horizontal or vertical
human :: Strategy
human b le = do printBoard b
                input <- getLine
                let p = take 2 $ words input
                let d = last $ words input
                let e = normalizeEdge p d
                if not $ isValid e  then do putStrLn "The selected coordinates can't make a line, please enter a correct ones"
                                            human b le
                else if not $ isAvailable e le  then do putStrLn "The selected line is not available, please enter another"
                                                        human b le
                else return e

-- Random choice in the given edge list
easy :: Strategy
easy b le = do  r <- randomRIO (0, length le - 1)
                return (le !! r)

-- Tries to complete a square, if not possible makes a random choice
medium :: Strategy
medium b le = do  l <- findSquareEdges b le
                  easy b (if null l then le else l)

-- Tries to complete a square, if not possible tries to avoid to make
-- the third line in a square, if not possible makes a random choice
hard :: Strategy
hard b le = do  l <- findSquareEdges b le
                if null l then do l2 <- findGoodEdges b le
                                  easy b (if null l2 then le else l2)
                else easy b l


-- STRATEGY HELPERS


isEdgeN :: Int -> Board -> Edge -> Bool
isEdgeN n b e@((p1,p2),(q1,q2))
  | h && p1 == 0          = length y == n
  | h && p1 == length b   = length x == n
  | h                     = (length x == n) || (length y == n)
  | p2 == 0               = length y == n
  | p2 == length (head b) = length z == n
  | otherwise             = (length z == n) || (length y == n)
    where h = isHorizontal e
          x  = fst $ (b !! (p1-1)) !! p2
          y  = fst $ (b !! p1) !! p2
          z  = fst $ (b !! p1) !! (p2-1)


findSquareEdges :: Board -> [Edge] -> IO [Edge]
findSquareEdges b le = return (filter (isEdgeN 3 b) le)

findGoodEdges :: Board -> [Edge] -> IO [Edge]
findGoodEdges b le = return (filter (not . isEdgeN 2 b) le)
