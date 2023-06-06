type Location = (Char, Int)
data Player = White | Black deriving (Show, Eq)
data Piece = P Location | N Location | K Location | Q Location | R Location | B Location deriving (Show, Eq)
type Board = (Player, [Piece], [Piece])


setBoard :: Board
setBoard = (White, whitePieces, blackPieces)
  where
    whitePieces = [ R ('h',1),N ('g',1),B ('f',1),K ('e',1),Q ('d',1),B ('c',1),N ('b',1),R ('a',1),
                    P ('h',2),P ('g',2),P ('f',2),P ('e',2),P ('d',2),P ('c',2),P ('b',2),P ('a',2) ]
    blackPieces = [ R ('h',8),N ('g',8),B ('f',8),K ('e',8),Q ('d',8),B ('c',8),N ('b',8),R ('a',8),
                    P ('h',7),P ('g',7),P ('f',7),P ('e',7),P ('d',7),P ('c',7),P ('b',7),P ('a',7) ]


filterPieces :: [Piece] -> Location -> [Piece]
filterPieces [] _ = []
filterPieces (piece : pieces) (letter, number) =
  case piece of
    P (pieceLetter, pieceNumber) -> filterPiece P pieceLetter pieceNumber
    N (pieceLetter, pieceNumber) -> filterPiece N pieceLetter pieceNumber
    K (pieceLetter, pieceNumber) -> filterPiece K pieceLetter pieceNumber
    Q (pieceLetter, pieceNumber) -> filterPiece Q pieceLetter pieceNumber
    R (pieceLetter, pieceNumber) -> filterPiece R pieceLetter pieceNumber
    B (pieceLetter, pieceNumber) -> filterPiece B pieceLetter pieceNumber
  where
    filterPiece constructor pieceLetter pieceNumber =
      if pieceLetter == letter && pieceNumber == number
        then constructor (pieceLetter, pieceNumber) : filterPieces pieces (letter, number)
        else filterPieces pieces (letter, number)


visualizeSquare :: [Piece] -> Char -> String
visualizeSquare (piece:_) colour =
  case piece of
    P (_, _) -> " P" ++ [colour] ++ " |"
    N (_, _) -> " N" ++ [colour] ++ " |"
    K (_, _) -> " K" ++ [colour] ++ " |"
    Q (_, _) -> " Q" ++ [colour] ++ " |"
    R (_, _) -> " R" ++ [colour] ++ " |"
    B (_, _) -> " B" ++ [colour] ++ " |"
    _ -> "  |"


visualizeRow :: [Piece] -> [Piece] -> (Int, Int) -> String
visualizeRow _ _ (_, 8) = ""
visualizeRow [] [] _ = ""
visualizeRow whitePieces blackPieces (row, index) =
    if(filteredWhitePieces /= [])
        then visualizeSquare filteredWhitePieces 'W' ++ visualizeRow whitePieces blackPieces (row, index+1)
    else if(filteredBlackPieces /= [])
        then visualizeSquare filteredBlackPieces 'B' ++ visualizeRow whitePieces blackPieces (row, index+1)
        else "    |" ++ visualizeRow whitePieces blackPieces (row, index+1)
    where
        letters = ['a','b','c','d','e','f','g','h']
        filteredWhitePieces = filterPieces whitePieces (letters!!index, row)
        filteredBlackPieces = filterPieces blackPieces (letters!!index, row)


visualizeBoard :: Board -> String
visualizeBoard (player, whitePieces, blackPieces) = 
  "    a    b    c    d    e    f    g    h\n" ++
  "8 |" ++ visualizeRow whitePieces blackPieces (8, 0) ++ "\r\n" ++
  "7 |" ++ visualizeRow whitePieces blackPieces (7, 0) ++ "\r\n" ++
  "6 |" ++ visualizeRow whitePieces blackPieces (6, 0) ++ "\r\n" ++
  "5 |" ++ visualizeRow whitePieces blackPieces (5, 0) ++ "\r\n" ++
  "4 |" ++ visualizeRow whitePieces blackPieces (4, 0) ++ "\r\n" ++
  "3 |" ++ visualizeRow whitePieces blackPieces (3, 0) ++ "\r\n" ++
  "2 |" ++ visualizeRow whitePieces blackPieces (2, 0) ++ "\r\n" ++
  "1 |" ++ visualizeRow whitePieces blackPieces (1, 0) ++ "\r\n" ++
  "\nTurn: " ++ show player


indexof :: Eq a => a -> [a] -> Int
indexof _ [] = 0
indexof x (y:ys) = if x == y then 0 else 1 + indexof x ys


isDiagonalMoveLegal :: Piece -> Location -> Location -> Board -> Bool
isDiagonalMoveLegal piece (startLetter, startNumber) (endLetter, endNumber) (player, whitePieces, blackPieces)
  | endNumber > 8 || endNumber < 1 = False
  | startLetter == endLetter && startNumber == endNumber = True
  | startLetter == endLetter || startNumber == endNumber = False
  | filterPieces whitePieces (startLetter, startNumber) /= [] && filterPieces whitePieces (startLetter, startNumber) /= [piece] = False 
  | filterPieces blackPieces (startLetter, startNumber) /= [] && filterPieces blackPieces (startLetter, startNumber) /= [piece] = False
  | nextLetterIndex > 7 || nextLetterIndex < 0 = False
  | otherwise = isDiagonalMoveLegal piece (letters!!nextLetterIndex, nextNumber) (endLetter, endNumber) (player, whitePieces, blackPieces)
  where
    letters = ['a','b','c','d','e','f','g','h']
    nextLetterIndex 
      | indexof endLetter letters > indexof startLetter letters = indexof startLetter letters + 1 
      | indexof endLetter letters < indexof startLetter letters = indexof startLetter letters - 1
      | otherwise = indexof startLetter letters
    nextNumber
      | endNumber > startNumber = startNumber + 1
      | endNumber < startNumber = startNumber - 1
      | otherwise = startNumber


isHorizontalMoveLegal :: Piece -> Location -> Location -> Board -> Bool
isHorizontalMoveLegal piece (startLetter, startNumber) (endLetter, endNumber) (player, whitePieces, blackPieces)
  | endNumber > 8 || endNumber < 1 = False
  | startLetter == endLetter && startNumber == endNumber = True
  | startLetter /= endLetter && startNumber /= endNumber = False
  | filterPieces whitePieces (startLetter, startNumber) /= [] && filterPieces whitePieces (startLetter, startNumber) /= [piece] = False 
  | filterPieces blackPieces (startLetter, startNumber) /= [] && filterPieces blackPieces (startLetter, startNumber) /= [piece] = False
  | nextLetterIndex > 7 || nextLetterIndex < 0 = False
  | otherwise = isHorizontalMoveLegal piece (letters!!nextLetterIndex, nextNumber) (endLetter, endNumber) (player, whitePieces, blackPieces)
  where
    letters = ['a','b','c','d','e','f','g','h']
    nextLetterIndex 
      | indexof endLetter letters > indexof startLetter letters = indexof startLetter letters + 1 
      | indexof endLetter letters < indexof startLetter letters = indexof startLetter letters - 1
      | otherwise = indexof startLetter letters
    nextNumber
      | endNumber > startNumber = startNumber + 1
      | endNumber < startNumber = startNumber - 1
      | otherwise = startNumber


isLegal:: Piece -> Board -> Location -> Bool
isLegal piece (turn, whitePieces, blackPieces) (targetLetter,targetNumber) = 
      if filterPieces allyPieces (targetLetter, targetNumber) /= [] 
        then False 
      else
      case piece of
      P (letter, number) ->  
          if filterPieces enemyPieces (targetLetter,targetNumber) == []
            then if letters!!targetLetterIndex == letter && number + 1 == targetNumber && filterPieces whitePieces (letter, number) /= [] then True  
            else if letters!!targetLetterIndex == letter && number - 1 == targetNumber && filterPieces blackPieces (letter, number) /= [] then True  
            else if letters!!(targetLetterIndex) == letter && number + 2 == targetNumber && isLegal piece (turn, whitePieces, blackPieces) (targetLetter,3) && filterPieces whitePieces (letter, number) /= [] then True
            else if letters!!(targetLetterIndex) == letter && number - 2 == targetNumber && isLegal piece (turn, whitePieces, blackPieces) (targetLetter,6) && filterPieces blackPieces (letter, number) /= [] then True
            else False
          else 
            if targetLetterIndex /= 7 && letters!!(targetLetterIndex+1) == letter && number + 1 == targetNumber && filterPieces whitePieces (letter, number) /= []  then True
            else if targetLetterIndex /= 7 && letters!!(targetLetterIndex+1) == letter && number - 1 == targetNumber  && filterPieces blackPieces (letter, number) /= [] then True
            else if targetLetterIndex /= 0 && letters!!(targetLetterIndex-1) == letter && number + 1 == targetNumber && filterPieces whitePieces (letter, number) /= [] then True
            else if targetLetterIndex /= 0 && letters!!(targetLetterIndex-1) == letter && number - 1 == targetNumber  && filterPieces blackPieces (letter, number) /= [] then True  
            else False
      N (letter, number) ->
          if targetLetterIndex /= 0 && letters!!(targetLetterIndex-1) == letter && (number + 2 == targetNumber || number - 2 == targetNumber) then True
          else if targetLetterIndex /= 7 && letters!!(targetLetterIndex+1) == letter && (number + 2 == targetNumber || number - 2 == targetNumber) then True
          else if targetLetterIndex >= 2 && letters!!(targetLetterIndex-2) == letter && (number + 1 == targetNumber || number - 1 == targetNumber) then True
          else if targetLetterIndex <= 5 && letters!!(targetLetterIndex+2) == letter && (number + 1 == targetNumber || number - 1 == targetNumber) then True
          else False
      K (letter, number) -> 
          if targetLetterIndex /= 0 && letters!!(targetLetterIndex-1) == letter && number + 1 == targetNumber then True
          else if targetLetterIndex /= 0 && letters!!(targetLetterIndex-1) == letter && number == targetNumber then True
          else if targetLetterIndex /= 0 && letters!!(targetLetterIndex-1) == letter && number -1 == targetNumber then True
          else if letters!!(targetLetterIndex) == letter && number + 1 == targetNumber then True
          else if letters!!(targetLetterIndex) == letter && number - 1 == targetNumber then True
          else if targetLetterIndex /= 7 && letters!!(targetLetterIndex+1) == letter && number + 1 == targetNumber then True
          else if targetLetterIndex /= 7 && letters!!(targetLetterIndex+1) == letter && number == targetNumber then True
          else if targetLetterIndex /= 7 && letters!!(targetLetterIndex+1) == letter && number - 1 == targetNumber then True
          else False
      Q (letter,number) -> 
          isDiagonalMoveLegal piece (letter, number) (targetLetter, targetNumber) (player, whitePieces, blackPieces) ||
          isHorizontalMoveLegal piece (letter, number) (targetLetter, targetNumber) (player, whitePieces, blackPieces)
      R (letter,number) -> 
          (letter == targetLetter || number == targetNumber) && isHorizontalMoveLegal piece (letter, number) (targetLetter, targetNumber) (player, whitePieces, blackPieces)
      B (letter,number) -> 
          (letter /= targetLetter && number /= targetNumber) && isDiagonalMoveLegal piece (letter, number) (targetLetter, targetNumber) (player, whitePieces, blackPieces)
      
      where
        player = if elem piece whitePieces then White else Black
        allyPieces = if player == White then whitePieces else blackPieces
        enemyPieces = if player == White then blackPieces else whitePieces
        letters = ['a','b','c','d','e','f','g','h']
        targetLetterIndex = indexof targetLetter letters


suggestMove:: Piece -> Board -> [Location]
suggestMove piece (player, whitePieces, blackPieces) = 
    filter (isLegal piece (player, whitePieces, blackPieces)) [(letter,number) | letter <- ['a'..'h'], number <- [1..8]]


removePiece:: Location -> [Piece] -> [Piece]
removePiece _ [] = []
removePiece (targetletter, targetnumber) (piece:pieces) = 
    case piece of
      P (letter, number) -> if letter == targetletter && number == targetnumber then pieces else piece : removePiece (targetletter, targetnumber) pieces
      N (letter, number) -> if letter == targetletter && number == targetnumber then pieces else piece : removePiece (targetletter, targetnumber) pieces
      K (letter, number) -> if letter == targetletter && number == targetnumber then pieces else piece : removePiece (targetletter, targetnumber) pieces
      Q (letter, number) -> if letter == targetletter && number == targetnumber then pieces else piece : removePiece (targetletter, targetnumber) pieces
      R (letter, number) -> if letter == targetletter && number == targetnumber then pieces else piece : removePiece (targetletter, targetnumber) pieces
      B (letter, number) -> if letter == targetletter && number == targetnumber then pieces else piece : removePiece (targetletter, targetnumber) pieces


changeLocation :: Piece -> Location -> Piece
changeLocation piece (endLetter, endNumber) =
  case piece of
    P _ -> P (endLetter, endNumber)
    N _ -> N (endLetter, endNumber)
    K _ -> K (endLetter, endNumber)
    Q _ -> Q (endLetter, endNumber)
    R _ -> R (endLetter, endNumber)
    B _ -> B (endLetter, endNumber)


getPieceLocation :: Piece -> Location
getPieceLocation piece =
  case piece of
    P (letter, number) -> (letter, number)
    N (letter, number) -> (letter, number)
    K (letter, number) -> (letter, number)
    Q (letter, number) -> (letter, number)
    R (letter, number) -> (letter, number)
    B (letter, number) -> (letter, number)

getPieceLetter :: Piece -> Char
getPieceLetter piece =
  case piece of
    P (letter, _) -> letter
    N (letter, _) -> letter
    K (letter, _) -> letter
    Q (letter, _) -> letter
    R (letter, _) -> letter
    B (letter, _) -> letter


getPieceNumber :: Piece -> Int
getPieceNumber piece =
  case piece of
    P (_, number) -> number
    N (_, number) -> number
    K (_, number) -> number
    Q (_, number) -> number
    R (_, number) -> number
    B (_, number) -> number


insertPieceSorted :: Piece -> [Piece] -> [Piece]
insertPieceSorted piece [] = [piece]
insertPieceSorted piece (p:pieces) = 
    if number > afterNumber
      then piece : p : pieces
    else if indexof letter letters < indexof afterLetter letters && number == afterNumber
      then piece : p : pieces
    else p : insertPieceSorted piece pieces
    where
        letters = ['a','b','c','d','e','f','g','h']
        letter = getPieceLetter piece
        number = getPieceNumber piece
        afterNumber = getPieceNumber p
        afterLetter = getPieceLetter p
        

move:: Piece -> Location -> Board -> Board
move piece location (player, whitePieces, blackPieces) =
    if player == White && (elem piece blackPieces) then
      error "This is White player's turn, Black can't move." 
    else if player == Black && (elem piece whitePieces) then
      error "This is Black player's turn, White can't move."
    else if isLegal piece (player, whitePieces, blackPieces) location == False
      then error ("Illegal move for piece " ++ show piece)
    else
      (
        enemy,
        newWhitePieces,
        newBlackPieces
      )
    where
        enemy = if player == White then Black else White
        editedWhitePieces = removePiece location (removePiece (getPieceLocation piece) whitePieces) 
        editedBlackPieces = removePiece location (removePiece (getPieceLocation piece) blackPieces)
        newPiece = changeLocation piece location
        newWhitePieces =  if player == White then insertPieceSorted newPiece editedWhitePieces else editedWhitePieces
        newBlackPieces = if player == Black then insertPieceSorted newPiece editedBlackPieces else editedBlackPieces