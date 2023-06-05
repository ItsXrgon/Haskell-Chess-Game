# Haskell-Chess-Game
Playable chess game made with Haskell


> setBoard

Create a new board, White goes first!

> visualizeBoard board 

Displays the board

> isLegal piece board location

Returns true if the piece can move to location on the board and false otherwise

> suggestMove piece board

Shows all possible legal moves for the piece

> move piece location board 

Returns a new board with the piece moved to the location chosen, or an error if the move is illegal or if it's not the correct turn
