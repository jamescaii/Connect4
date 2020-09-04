open SigGame;

/* describes either a human or AI player,
 * which selects the next move */
module type Player = {
  module PlayerGame: Game
  /* given a state, selects a move */
  let nextMove: PlayerGame.state => PlayerGame.move;
};