# Connect4

A player will interact with our program by inputing a number into the terminal that would represent the column number that the player would like to drop their Connect4 piece into. 
Against an AI player, the human player would repeatedly do this until one has won the game. Aganist another human player, both would input a number on their turn until someone has won.

This program begins with stringOfPlayer, stringOfMove, stringOfSlot, and stringOfList
which converts a whichPlayer, a move, a slot, and a list to a string. stringOfState uses 
stringOfBoard to convert the lists into a board. createBoard uses the helper create to make
a baord given 2 ints. initialState creates the state that has P1 going first. legalMoves outputs
all of the possible moves that are possible with a given state. gameStatus outputs the status given
a state. The procedure drop takes in the column that the user wants to drop the piece into and 
drops it in if it is a legal move. checkWinCol checks if there is 4 in a row in a column.
checkWinRow uses checkWinCol on a transposed board to check if there is 4 in a row in a row.
checkDiaUp and checkDiaDown checks to see if there is a diagonal win. These are all used in 
nextState to check if a player has won. If no player has won, it outputs the board with the new move
and the next player's turn. convertBoardList converts a board to all possible lists. 
calculatePointsHelper assigns a value for each possibe combination of players for a list of 
length 4. estimateValue takes in a State and outputs the total value of the board using 
calculatePointsHelper and convertBoardList. AI Player uses a max and a min function which find
the max and min in a list. The maxmin procedure with find a check to see what the index of the 
max or min value is and return that index. nextAI takes in the state and an int, which represents
the depth, and runs the minimax algorthim. nextMove takes in a state and outputs the correct
move that the AI must make using legalMoves, AIplayer, and maxmin.
