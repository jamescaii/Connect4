open!  CS17SetupGame;   
open SigGame; 
module Connect4 = {
   let initialcolumn=8;
   let initialrow=8;
    /* specifies a player */
    type whichPlayer = P1 | P2;

    /* status of game: if it's over (and who won) or ongoing (and who's turn) */
    type status =
       | Win(whichPlayer)
       | Draw
       | Ongoing(whichPlayer);
   /* outputs the other player*/ 
   let otherPlayer: whichPlayer => whichPlayer = play =>
      switch(play){
      | P1 => P2
      | P2 => P1
       };
    type slot = 
    | Empty
    | Player(whichPlayer);
    /* the state of the game: the position, status, anything else associated
    with the game at a given turn * TODO */
    type state = State(status,list(list(slot)));

    /* describes a move that a player can make TODO */
    type move = Move(int);

    /* printing functions */
   /*Input: stp, a whichPlayer 
    *Output: the string version of the player */ 
   let stringOfPlayer: whichPlayer => string = stp =>switch(stp)
   {
   | P1=>"P1"
   | P2=>"P2"
   };
    
   /*Input: (Move(x)), a move 
    *Output: the move represented as a string */ 
   let stringOfMove: move => string = (Move(x)) => string_of_int(x);

   /*Input: mat, a board represented as list(list(slot))
    *Output: the board transposed*/ 
   /*Recursion Diagram: TODO*/ 
   let rec transpose : list(list(slot)) => list(list(slot)) = mat => 
   switch (mat) 
   {
   | []
   | [[], ..._] => failwith("Board cannot be 0-dimensional.")
   | [[_], ..._] => [List.concat(mat)]  
   | [[_, ..._], ..._] => [List.map (List.hd, mat),... transpose 
   (List.map (List.tl, mat))]  
   }; 
   
   /*Input: slt, a slot
    *Output: the slot represented as a string*/ 
   let stringOfSlot: slot=> string= slt => switch(slt){
   |Empty=>"NA"
   |Player(P1)=>"P1"
   |Player(P2)=>"P2" };
   
   /*Input: lst, a list of slot, which represents a column 
    *Output: the column represented as a string*/ 
   let stringOfList: list(slot)=>string= lst => switch(lst){
   |[]=>failwith("empty column does not exist")
   |[hd,...tl]=>  "[" ++String.concat(" ", List.map(stringOfSlot,[hd,...tl]))++
     "]"
   };
   
   /*Input: brd, a list(list(slot)), representing the board 
    *Output: the board converted to a string*/ 
   let stringOfBoard: list(list(slot))=>string= brd=> switch(transpose(brd)){
   |[]=>failwith("cannot convert empty board to string because it does not exist")
   |[hd,...tl]=> "   " ++ String.concat("
   ", List.map(stringOfList,[hd,...tl]))++ " "};
  
  /* Input: (State(_,x)), the state of the game
   * Output: a string version of the current board*/ 
   let stringOfState: state => string= (State(_,x))=> 
   stringOfBoard(x);  
 
   
    /* Input: an alpha a and an integer b
       Output: a list of length b filled with a */
   /* Recursion Diagram: TODO */ 
    let rec create: ('a, int) => list('a) = (a, b) =>
     switch(b){
    | 0 => []
    | _ => [a ,...create(a, b - 1)]}; 
    /* Input: columnns, an integer indicating the number of columns the initial board has 
              rows, an integer indicating the number of rows 
       Output: a list of list of slots, representing the initial board */ 
   let createBoard: (int, int) => list(list(slot)) = (columns, rows) =>
        create(create(Empty, columns),rows) ;

    /* Game Logic */

    /* the state of the game when it begins */
   let initialState= State((Ongoing(P1)), createBoard(initialcolumn, initialrow));

    
   /*Input: x, a list of slot, and y, an integer 
    *Output: a list of legal moves*/ 
   let rec movesHelper: (list(slot), int) => list(move) = (x, y) =>
      switch(x){
         | [] => []
         | [hd,...tl] => if(hd == Empty){[Move(y),...movesHelper(tl, (y + 1))]}
                         else {movesHelper(tl, (y+1))}
      };
   /*Input: (State(_,b)), the state of the game 
    *Output: a list of legal moves at that state*/ 
   let legalMoves: state => list(move)= (State(_, b)) => 
      movesHelper(List.hd(transpose(b)), 1);

   
    /* returns the status of the game at the given state */
    let gameStatus: state => status= (State(s,_))=>s; 


   /* HELPER FUNCTIONS for nextstate*/ 
   /*helper function for drop*/ 
    let rec dropIn: (list(slot), whichPlayer) => list(slot) = (col, player) => 
      switch(col){
      | [] => failwith("passed a completly full column")
      | [Empty] => [Player(player)] 
      | [hd1, hd2,...tl] => switch(hd1, hd2){
                     | (Empty, Empty) => [hd1,...dropIn([hd2,...tl], player)]
                     | (Empty, Player(_)) => [Player(player), hd2,... tl]
      }
      }

   let rec drop: (int, list(list(slot)), whichPlayer) => list(list(slot)) = (i, board, p) => 
      switch(board){
      | [hd,...tl] when (i == 1) => [dropIn(hd, p),...tl]
      | [hd,...tl] => [hd,...drop(i - 1, tl, p)]
      | _ => failwith("failed")}


  let rec checkWinColhelper: (list(slot), whichPlayer) => bool = (column, p) =>
         switch(column) {
         | [hd1, hd2, hd3, hd4] => if((hd1 == hd2) && (hd2== hd3) && (hd3== hd4) && (hd4== Player(p)))
                                   {true}
                                   else {false};
         | [hd1, hd2, hd3, hd4,...tl] => if((hd1 == hd2) && (hd2== hd3) && (hd3== hd4) && (hd4== 
                                             Player(p))){true}
                                         else {checkWinColhelper([hd2,hd3,hd4,...tl], p)}
      };
   
let rec checkWinCol: (list(list(slot)), whichPlayer) => status = (board, p) =>
      switch(board){
       | []=>Ongoing(otherPlayer(p))
       | [hd,...tl] => if (checkWinColhelper(hd, p) == true)
                        {Win(p)}
                       else {checkWinCol(tl, p)}
      };
 
 let checkWinRow: (list(list(slot)), whichPlayer) => status = (board, p) =>
      checkWinCol(transpose(board), p);

let rec checkDiaUphelper: (list(list(slot)), whichPlayer, int) => bool = (board, p, i) =>
      switch(board){
         | [[a, b, c, d],[e, f, g, h],[x, j, k, l],[m, n, o, y]] => if (a == f && f == k && k == y && y == Player(p)) {true}
                                                                    else {false};
         | [[hd1,...tl1], [hd2,...tl2], [hd3,...tl3], [hd4,...tl4]] => if ((List.nth([hd1,...tl1], i) == List.nth([hd2,...tl2], i + 1)) && 
                                       (List.nth([hd2,...tl2], i + 1) == List.nth([hd3,...tl3], i + 2)) && 
                                       (List.nth([hd3,...tl3], i + 2) == List.nth([hd4,...tl4], i + 3)) && 
                                       (List.nth([hd4,...tl4], i + 3) == Player(p))){
                                          true
                                          }
                                   else {checkDiaUphelper([tl1, tl2, tl3, tl4] ,p, i)}
         };

    let rec checkDiaDownhelper: (list(list(slot)), whichPlayer, int) => bool = (board, p, i) =>
      switch(board){
         | [[a, b, c, d],[e, f, g, h],[x, j, k, l],[m, n, o, y]] => if (d == g && g == j && j == m && m == Player(p)) {true}
                                                                    else {false};
         | [[hd1,...tl1], [hd2,...tl2], [hd3,...tl3], [hd4,...tl4]] => if ((List.nth([hd1,...tl1], i) == List.nth([hd2,...tl2], i - 1)) && 
                                       (List.nth([hd2,...tl2], i - 1) == List.nth([hd3,...tl3], i - 2)) && 
                                       (List.nth([hd3,...tl3], i - 2) == List.nth([hd4,...tl4], i - 3)) && 
                                       (List.nth([hd4,...tl4], i - 3) == Player(p))){
                                          true
                                          }
                                   else {checkDiaDownhelper([tl1, tl2, tl3, tl4] ,p, i)}
         };

    let rec checkDiaUp: (list(list(slot)), whichPlayer) => status = (board, p) =>
      switch(board){
      | [hd1, hd2, hd3, hd4] => if (checkDiaUphelper(board, p, 1) == true){Win(p)}
                                else {Ongoing(otherPlayer(p))};
      | [hd1, hd2, hd3, hd4,...tl] => if(checkDiaUphelper([hd1, hd2, hd3, hd4], p, 1) == true){Win(p)}
                                      else {checkDiaUp([hd2, hd3, hd4,...tl], p)};
      };

    let rec checkDiaDown: (list(list(slot)), whichPlayer) => status = (board, p) =>
      switch(board){
      | [hd1, hd2, hd3, hd4] => if (checkDiaDownhelper(board, p, 3) == true){Win(p)}
                                else {Ongoing(otherPlayer(p))};
      | [hd1, hd2, hd3, hd4,...tl] => if(checkDiaDownhelper([hd1, hd2, hd3, hd4], p, 3) == true) {Win(p)}
                                      else {checkDiaDown([hd2, hd3, hd4,...tl], p)};
      };


    /* given a state and a legal move, yields the next state */
    let nextState: (state, move) => state= (State(s, i), Move(m)) => 
      switch(s, m){
      | (Win(_), _) | (Draw, _) => State(s, i)
      | (Ongoing(p), m) => if (checkWinCol(drop(m, i, p), p) == Win(p)|| checkWinRow(drop(m, i, p), p) == Win(p) || 
                               checkDiaDown(drop(m, i, p), p) == Win(p) || checkDiaUp(drop(m, i, p), p) == Win(p))
                           {State(Win(p), drop(m, i, p))}
                           else {State(Ongoing(otherPlayer(p)), drop(m, i, p))}
      };

    /* for transforming human player input into
    internal representation of move */
    let moveOfString: string => move= str =>
      Move(int_of_string(str));

/*helpers for estimateValue*/ 


let rec convertBoardDiagonalsUp: list(list(slot))=> list(list(slot))=board=> {let rec zip2: (('a,'b)=>'b,list('a),list('b) )=> list('b) =(f,items,things)=>switch(items,things){
|([],_)=>things
|([hd1,...tl1],[hd2,...tl2])=>[(f(hd1,hd2)),...zip2(f,tl1,tl2)]
|(_,[])=>failwith("Domain error")};switch(board) {
|[]=>failwith("board cannot be empty")
|[row]=>List.map(x=>[x],row)
|[[],..._]=>failwith("Domain error: cannot have zero rows")
|[[hd,...tl],...otherRows]=>[[hd],...zip2((x,y)=>[x,...y],tl,convertBoardDiagonalsUp(otherRows))]
}}


let convertBoardlist: list(list(slot))=>list(list(slot))=board=> {let vertflip: list(list (slot))=> list(list (slot)) = mat => {List.map (List.rev, mat)};board@transpose(board)@convertBoardDiagonalsUp(board)@vertflip(convertBoardDiagonalsUp(vertflip(board)))}

let calculatePointsHelper: list(slot)=> int = list => switch(list)
{
|[]=>failwith("board cannot be empty")
|[Player(P1),Empty,Empty,Empty]=>2
|[Empty,Player(P1),Empty,Empty]=>2
|[Empty,Empty,Player(P1),Empty]=>2
|[Empty,Empty,Empty,Player(P1)]=>2
|[Player(P1),Player(P1),Empty,Empty] =>4
|[Player(P1),Empty,Player(P1),Empty]=>4
|[Player(P1),Empty,Empty,Player(P1)]=>4
|[Empty,Player(P1),Player(P1),Empty]=>4
|[Empty,Empty,Player(P1),Player(P1)]=>4
|[Empty,Player(P1),Empty,Player(P1)]=>4
|[Player(P1),Player(P1),Player(P1),Empty]=>16
|[Player(P1),Player(P1),Empty,Player(P1)]=> 16
|[Player(P1),Empty,Player(P1),Player(P1)]=> 16
|[Empty,Player(P1),Player(P1),Player(P1)]=> 16
|[Player(P2),Empty,Empty,Empty]=> -2
|[Empty,Player(P2),Empty,Empty]=> -2
|[Empty,Empty,Player(P2),Empty]=> -2
|[Empty,Empty,Empty,Player(P2)]=> -2
|[Player(P2),Player(P2),Empty,Empty] => -4
|[Player(P2),Empty,Player(P2),Empty]=> -4
|[Player(P2),Empty,Empty,Player(P2)]=> -4
|[Empty,Player(P2),Player(P2),Empty]=> -4
|[Empty,Empty,Player(P2),Player(P1)]=> -4
|[Empty,Player(P2),Empty,Player(P1)]=> -4
|[Player(P2),Player(P2),Player(P2),Empty]=> -16
|[Player(P2),Player(P2),Empty,Player(P2)]=> -16
|[Player(P2),Empty,Player(P2),Player(P2)]=> -16
|[Empty,Player(P2),Player(P2),Player(P2)]=> -16
|_=>0 
}


let rec calculateHelper: list(slot) => int = list =>  
    switch(list){
    | [hd1, hd2, hd3, hd4] => calculatePointsHelper([hd1, hd2, hd3, hd4])
    | [hd1, hd2, hd3, hd4,...tl] => calculatePointsHelper([hd1, hd2, hd3, hd4]) + calculateHelper([hd2, hd3, hd4,...tl])
    | _ => 0
    };

let rec calculatePoints: list(list(slot))=> int = list => 
    switch(list){
    | [] => 0
    | [hd,...tl] => calculateHelper(hd) + calculatePoints(tl)
    };
let estimateValue: state => float = (State(s,i)) => 
    switch(s) { 
    | Win(P1)=> infinity
    | Win(P2)=> neg_infinity
    | Ongoing(P1)=> float_of_int(calculatePoints(convertBoardlist(i)))
    | Ongoing(P2)=> float_of_int(calculatePoints(convertBoardlist(i)))
    | Draw=> 0. 
    }
    
    
};

module MyGame : Game = Connect4;
open Connect4;