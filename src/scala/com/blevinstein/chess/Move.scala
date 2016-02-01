package com.blevinstein.chess

trait Move {
  // NOTE: To fully embed information about valid moves, we need to have access
  // to the entire history of the board. Consider en passant and castling.
  def apply(history: History, position: Position): Option[Position]
}
