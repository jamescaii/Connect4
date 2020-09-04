open! CS17SetupGame;
open SigGame;
open Connect4;

module AIPlayer = (MyGame: Game) => {
  module PlayerGame = MyGame;
  open PlayerGame;
  
  let rec max: list(float) => float =
    lst =>
      switch (lst) {
      | [item] => item
      | [hd1, hd2, ...tl] =>
        if (hd1 > hd2) {
          max([hd1, ...tl]);
        } else {
          max([hd2, ...tl]);
        };
      | _ => failwith("invaild input")
      };
  let rec min: list(float) => float =
    lst =>
      switch (lst) {
      | [item] => item
      | [hd1, hd2, ...tl] =>
        if (hd1 < hd2) {
          min([hd1, ...tl]);
        } else {
          min([hd2, ...tl]);
        };
      | _ => failwith("invaild input")
      };

  let rec maxmin: (list(float), float, int) => int = (lst, flt, v) => 
      switch(lst){
      | [] => failwith("cannot be empty")
      | [hd,...tl] => if (flt == hd) {v} 
                      else {maxmin(tl, flt, v + 1)}
      }

  let rec nextAI: (PlayerGame.state, int) => float =
    (s, v) =>
      switch (v) {
      | 0 => PlayerGame.estimateValue(s)
      | _ =>
        switch (PlayerGame.gameStatus(s)) {
        | PlayerGame.Win(PlayerGame.P1) => infinity
        | PlayerGame.Win(PlayerGame.P2) => neg_infinity
        | PlayerGame.Draw => 0.0
        | PlayerGame.Ongoing(PlayerGame.P1) =>
          max(
            List.map(
              x => nextAI(x, v - 1),
              List.map(
                x => PlayerGame.nextState(s, x),
                PlayerGame.legalMoves(s),
              ),
            ),
          )
        | PlayerGame.Ongoing(PlayerGame.P2) =>
          min(
            List.map(
              x => nextAI(x, v - 1),
              List.map(
                x => PlayerGame.nextState(s, x),
                PlayerGame.legalMoves(s),
              ),
            ),
          )
        }
      };
      
  let nextMove: PlayerGame.state => PlayerGame.move =
    s =>
    {
    let fltLst = List.map(x => nextAI(x, 3), List.map(x => PlayerGame.nextState(s, x), PlayerGame.legalMoves(s)))
    switch(PlayerGame.gameStatus(s)){
    | PlayerGame.Ongoing(PlayerGame.P1) => List.nth(PlayerGame.legalMoves(s), maxmin(fltLst, max(fltLst), 0))
    | PlayerGame.Ongoing(PlayerGame.P2) => List.nth(PlayerGame.legalMoves(s), maxmin(fltLst, min(fltLst), 0))
    | _ => failwith("cannot be this input")
    };
  };

};


module TestGame = Connect4;
module TestAIPlayer = AIPlayer(Connect4);
open TestAIPlayer;

/* insert test cases for any procedures that don't take in
 * or return a state here */