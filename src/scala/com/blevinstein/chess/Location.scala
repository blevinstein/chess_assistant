package com.blevinstein.chess

object Location {
  def values: List[Location] =
      List.tabulate(8)(i =>
          List.tabulate(8)(j =>
              Location(i, j))).flatten

  def rankStr(rank: Int): String = "12345678".substring(rank, rank + 1)

  def fileStr(file: Int): String = "abcdefgh".substring(file, file + 1)
}
case class Location(file: Int, rank: Int) {
  def isValid: Boolean =
      0 <= rank && rank < 8 &&
      0 <= file && file < 8

  def background: Color = if ((file + rank) % 2 == 0) Black else White

  def +(offset: (Int, Int)): Location = offset match {
    case (f, r) => Location(file + f, rank + r)
  }

  override def toString: String =
      s"${Location.fileStr(file)}${Location.rankStr(rank)}"
}
