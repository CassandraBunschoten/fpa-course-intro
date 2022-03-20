package aoc 

import scala.io.Source

object AOC2 extends App:

  val move1 = 
    Source
    .fromFile("fps/src/main/resources/input02.txt")
    .getLines
    .toList
  
  def movement(l: List[String], w: String): List[Int] = 
    l.filter(_.startsWith(w))
     .flatMap(_.split(" "))
     .filter(_ != w)
     .map(_.toInt)
     .toList

  def depth(l: List[String], up: String, down: String): Int = 
    movement(move1, down).sum - movement(move1, up).sum
  
  val forward = movement(move1, "forward").sum  

  // Part 2
  // Need a new implementation, as the other splits the list, where now we need to walk over it

  val move2: List[String] = 
    Source
    .fromFile("fps/src/main/resources/input02.txt")
    .getLines
    .flatMap(_.split(" "))
    .toList

  def patTest(ls: List[String], hor: Int, depth: Int, aim: Int): Unit =
    ls match
      case "forward" :: y => patTest(y, hor + y.head.toInt, depth + (aim * y.head.toInt), aim)
      case "up" :: y      => patTest(y, hor, depth, (aim - y.head.toInt))
      case "down" :: y    => patTest(y, hor, depth, (aim + y.head.toInt))
      case _ :: y         => patTest(y, hor, depth, aim)
      case _              => println(s"final depth is $depth and final depth is $hor, together ${depth * hor}")

  patTest(move2, 0, 0, 0)
