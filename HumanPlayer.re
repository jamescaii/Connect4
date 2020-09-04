open SigGame;
open Connect4;

module HumanPlayer = (MyGame: Game) => {
  module PlayerGame = MyGame;

  type getInput = {
    mutable m: PlayerGame.move
  }

  let getInputJSLine : unit => string = {
    [%bs.raw{|
      function() {
        const readlineSync = require('readline-sync');
        const rl = readlineSync;
        var ans = rl.question('What move do you want to make? ');
        return ans;
    }|}]
  }


let rec nextMove = s => {
    let myMove : getInput = {m: List.nth(PlayerGame.legalMoves(s), 0)};
    let input : string = getInputJSLine();
    switch (input) {
    | "exit" => failwith("Exiting Game REPL"); 
    | _ => {
      let mov = try(PlayerGame.moveOfString(input)) {
        | _ =>
          print_endline("not a valid move");
          nextMove(s);
        }
      if (List.mem(mov, PlayerGame.legalMoves(s))) {
        myMove.m = mov;
      } else {
        print_endline("Illegal move.");
        myMove.m = nextMove(s);
      }
      }
    }
  myMove.m;
  }
};

module TestGame = Connect4;
module TestHumanPlayer = HumanPlayer(Connect4); 
open TestHumanPlayer; 


/* Because next_move involves read input, you don't need to test it here.
 * If you use any helper procedures in TestHumanPlayer that don't take in 
 * or return a state, test them here. */
