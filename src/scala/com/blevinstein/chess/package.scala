package com.blevinstein;

import com.blevinstein.chess.{Color,Black,White}
import com.blevinstein.chess.Piece
import com.blevinstein.chess.Piece._

package object chess {
  type History = List[(Position, Move)]
}
