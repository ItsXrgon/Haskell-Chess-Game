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
    _ -> "      |"


visualizeRow :: [Piece] -> [Piece] -> (Int, Int) -> String
visualizeRow _ _ (_, 8) = ""
visualizeRow [] [] _ = ""
visualizeRow whitePieces blackPieces (row, index) =
    if(filteredWhitePieces /= [])
        then visualizeSquare filteredWhitePieces 'W' ++ visualizeRow whitePieces blackPieces (row, index+1)
    else if(filteredBlackPieces /= [])
        then visualizeSquare filteredBlackPieces 'B' ++ visualizeRow whitePieces blackPieces (row, index+1)
        else "   |" ++ visualizeRow whitePieces blackPieces (row, index+1)
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
  "\n Turn: " ++ show player


indexof :: Eq a => a -> [a] -> Int
indexof _ [] = 0
indexof x (y:ys) = if x == y then 0 else 1 + indexof x ys

isLinemoveLegal :: Location -> Location -> Board -> Bool
isLinemoveLegal (startLetter, startNumber) (endLetter, endNumber) (player, whitePieces, blackPieces) = 

    if endNumber > 8 || endNumber < 1 then False
    else if startLetter == endLetter && startNumber == endNumber then True
    else if filterPieces whitePieces (startLetter, startNumber) /= [] then False 
    else if filterPieces blackPieces (startLetter, startNumber) /= [] then False
    else if nextLetterIndex > 7 || nextLetterIndex < 0 then False
    else isLinemoveLegal (letters!!nextLetterIndex, nextNumber) (endLetter, endNumber) (player, whitePieces, blackPieces)

    where
      letters = ['a','b','c','d','e','f','g','h']
      nextLetterIndex = 
        if indexof endLetter letters > indexof startLetter letters 
          then indexof startLetter letters + 1 
          else if indexof endLetter letters > indexof startLetter letters 
            then indexof startLetter letters - 1
            else indexof startLetter letters

      nextNumber = 
        if startNumber > endNumber 
          then startNumber - 1
          else if startNumber < endNumber 
            then startNumber + 1
            else startNumber


isLegal:: Piece -> Board -> Location -> Bool
isLegal piece (_, whitePieces, blackPieces) (targetLetter,targetNumber) = 

    if filterPieces allyPieces (targetLetter, targetNumber) /= [] 
      then False 
    else
      case piece of
      P (letter, number) ->  
          if filterPieces enemyPieces (targetLetter,targetNumber) == []
            then if letters!!targetLetterIndex == letter && number + 1 == targetNumber then True  
            else if letters!!targetLetterIndex == letter && number - 1 == targetNumber then True  
            else if letters!!(targetLetterIndex) == letter && number + 2 == targetNumber then True
            else if letters!!(targetLetterIndex) == letter && number - 2 == targetNumber then True
            else False
          else 
            if targetLetterIndex /= 7 && letters!!(targetLetterIndex+1) == letter && number + 1 == targetNumber then True
            else if targetLetterIndex /= 7 && letters!!(targetLetterIndex+1) == letter && number - 1 == targetNumber then True
            else if targetLetterIndex /= 0 && letters!!(targetLetterIndex-1) == letter && number + 1 == targetNumber then True
            else if targetLetterIndex /= 0 && letters!!(targetLetterIndex-1) == letter && number - 1 == targetNumber then True  
            else False
      N (letter, number) ->
          if letter == targetLetter || number == targetNumber then False
          else if targetLetterIndex /= 0 && letters!!(targetLetterIndex-1) == letter && number + 2 == targetNumber then True
          else if targetLetterIndex /= 0 && letters!!(targetLetterIndex-1) == letter && number - 2 == targetNumber then True
          else if targetLetterIndex /= 7 && letters!!(targetLetterIndex+1) == letter && number + 2 == targetNumber then True
          else if targetLetterIndex /= 7 && letters!!(targetLetterIndex+1) == letter && number - 2 == targetNumber then True
          else if targetLetterIndex >= 2 && letters!!(targetLetterIndex-2) == letter && number + 1 == targetNumber then True
          else if targetLetterIndex >= 2 && letters!!(targetLetterIndex-2) == letter && number - 1 == targetNumber then True
          else if targetLetterIndex <= 5 && letters!!(targetLetterIndex+2) == letter && number + 1 == targetNumber then True
          else if targetLetterIndex <= 5 && letters!!(targetLetterIndex+2) == letter && number - 1 == targetNumber then True
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
          isLinemoveLegal (letter, number) (targetLetter, targetNumber) (player, whitePieces, blackPieces) 
      R (letter,number) -> 
          isLinemoveLegal (letter, number) (targetLetter, targetNumber) (player, whitePieces, blackPieces)
      B (letter,number) -> 
          isLinemoveLegal (letter, number) (targetLetter, targetNumber) (player, whitePieces, blackPieces)
      
      where
        player = if filterPieces whitePieces (targetLetter, targetNumber) /= [] then White else Black
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
      P (letter, number) -> if letter == targetletter && number == targetnumber then pieces else piece : removePiece (letter, number) pieces
      N (letter, number) -> if letter == targetletter && number == targetnumber then pieces else piece : removePiece (letter, number) pieces
      K (letter, number) -> if letter == targetletter && number == targetnumber then pieces else piece : removePiece (letter, number) pieces
      Q (letter, number) -> if letter == targetletter && number == targetnumber then pieces else piece : removePiece (letter, number) pieces
      R (letter, number) -> if letter == targetletter && number == targetnumber then pieces else piece : removePiece (letter, number) pieces
      B (letter, number) -> if letter == targetletter && number == targetnumber then pieces else piece : removePiece (letter, number) pieces


changeLocation :: Piece -> Location -> Piece
changeLocation piece (endLetter, endNumber) =
  case piece of
    P _ -> P (endLetter, endNumber)
    N _ -> N (endLetter, endNumber)
    K _ -> K (endLetter, endNumber)
    Q _ -> Q (endLetter, endNumber)
    R _ -> R (endLetter, endNumber)
    B _ -> B (endLetter, endNumber)


editBoard :: Piece -> Location -> [Piece] -> [Piece]
editBoard  _ _ [] = []
editBoard targetPiece (endLetter, endNumber) (piece:pieces) = 
  if targetPiece == piece
    then
      changeLocation targetPiece (endLetter, endNumber) : editBoard targetPiece (endLetter, endNumber) pieces
    else
      piece : editBoard targetPiece (endLetter, endNumber) pieces


move:: Piece -> Location -> Board -> Board
move piece location (player, whitePieces, blackPieces) = 
      (
        enemy,
        editBoard piece location removedWhitePieces,
        editBoard piece location removedBlackPieces
      )
    where
        ally = if filterPieces whitePieces location /= [] then White else Black  
        enemy = if player == White then Black else White
        removedWhitePieces = removePiece location whitePieces
        removedBlackPieces = removePiece location blackPieces
