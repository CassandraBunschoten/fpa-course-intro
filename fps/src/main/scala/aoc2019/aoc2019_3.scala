package AOC2019

import scala.io.Source

sealed trait Direction(distance: Int)

case class Right(distance: Int)  extends Direction(distance)
case class Left(distance: Int)   extends Direction(distance)
case class Up(distance: Int)     extends Direction(distance)
case class Down(distance: Int)   extends Direction(distance)

def makeDirection(input: String): Direction = 
  input.head match 
    case 'R' => Right(input.tail.toInt)
    case 'L' => Left(- input.tail.toInt)
    case 'U' => Up(input.tail.toInt)
    case 'D' => Down(- input.tail.toInt)


// make a variable which tracks location
case class Location(x: Int, y: Int):
  def updateLoc(d: Direction): List[Location] = 
    d match 
      case Right(v) if (v > 0) => copy(x + v, y) :: updateLoc(Right(v - 1))
      case Left(v)  if (v < 0) => copy(x + v, y) :: updateLoc(Left(v + 1))
      case Up(v)    if (v > 0) => copy(x, y + v) :: updateLoc(Up(v - 1))
      case Down(v)  if (v < 0) => copy(x, y + v) :: updateLoc(Down(v + 1))
      case _                   => Nil 

type ManDistance = Int    

def manhattanDistance(loc: Location): ManDistance =
  loc match 
    case Location(x,y) => Math.abs(x) + Math.abs(y)

object AOC2019_3 extends App: 
  val List(wire1, wire2) = 
        Source
        .fromFile("fps/src/main/resources/aoc2019/input2019_3.txt")
        .getLines
        .toList
        .map(_.split(",").toList.map(x => makeDirection(x)))
        .toList


  val locWire1: List[Location] = wire1.scanLeft(List(Location(0,0)))
                              ((acc, x) => acc.head.updateLoc(x) ::: acc).flatten
  val locWire2: List[Location] = wire2.scanLeft(List(Location(0,0)))
                              ((acc, x) => acc.head.updateLoc(x) ::: acc).flatten
  
  val crosses = locWire1.flatMap(loc => locWire2.filter(loc2 => loc2 == loc))
                        .filter(res => res != Location(0,0))

  val answer = crosses.map(x => manhattanDistance(x)).min

  println(answer)

  // def extract(d: List[Direction]): List[Int] = 
  //   d match 
  //     case Down(v) :: t  => (v) :: extract(t)
  //     case Right(v) :: t => (v) :: extract(t)
  //     case Left(v) :: t  => (v) :: extract(t)
  //     case Up(v) :: t    => (v) :: extract(t)
  //     case _             => Nil
