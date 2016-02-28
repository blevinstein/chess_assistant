package com.blevinstein.chess

import com.blevinstein.util.BiMap

import scala.language.implicitConversions

object LocationImplicits {
  implicit def stringToLocation(s: String): Location = Location(s)
}

object Location {
  def apply(str: String): Location = Location(
      fileToStr.inverse(str.substring(0, 1)),
      rankToStr.inverse(str.substring(1)))

  def values: List[Location] =
      List.tabulate(8)(i =>
          List.tabulate(8)(j =>
              Location(i, j))).flatten

  val rankToStr: BiMap[Int, String] = BiMap(
      0 -> "1",
      1 -> "2",
      2 -> "3",
      3 -> "4",
      4 -> "5",
      5 -> "6",
      6 -> "7",
      7 -> "8")
  val strToRank = rankToStr.inverse

  val fileToStr: BiMap[Int, String] = BiMap(
      0 -> "a",
      1 -> "b",
      2 -> "c",
      3 -> "d",
      4 -> "e",
      5 -> "f",
      6 -> "g",
      7 -> "h")
  val strToFile = fileToStr.inverse
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
      s"${Location.fileToStr(file)}${Location.rankToStr(rank)}"
}
