package aoc2020

import scala.io.Source
import cats.instances.tailRec

def findSeat(s: String): (Int, Int) =
  val row = find((0,127),s.substring(0,7).toCharArray.toList)
  val column = find((0,7),s.substring(7).toCharArray.toList)
  (row, column)

def find(l: (Int, Int), s: List[Char]): Int =
  l match 
    case(min, max) =>
      s match
        case ('F' | 'L') :: Nil   => min
        case ('B' | 'R') :: Nil   => max
        case ('F' | 'L') :: tail  => find((min, max - ((max - min) / 2)-1), tail)
        case ('B' | 'R') :: tail  => find((min + ((max - min) / 2)+1, max), tail)
        case _            => -1
object AOC52020 extends App:
   println(Source
    .fromFile("fps/src/main/resources/input52020.txt")
    .getLines
    .map(findSeat)
    .map(_ match{ case (row, column) => row * 8 + column})
    .toList.max
    )