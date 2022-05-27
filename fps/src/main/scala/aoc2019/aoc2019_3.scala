package AOC2019

import scala.io.Source

sealed trait Direction(distance: Int)

case class Right(distance: Int)  extends Direction(distance)
case class Left(distance: Int)   extends Direction(distance)
case class Up(distance: Int)     extends Direction(distance)
case class Down(distance: Int)   extends Direction(distance)

object Direction: 
  def makeDirection(input: String): Direction = 
    input.head match 
      case 'R' => Right(input.tail.toInt)
      case 'L' => Left(- input.tail.toInt)
      case 'U' => Up(input.tail.toInt)
      case 'D' => Down(- input.tail.toInt)

case class Location(x: Int, y: Int)

case class Range(xStart: Int, xEnd: Int, yStart: Int, yEnd: Int):
  def updateRange(d: Direction): Range = 
    val Location(x, y) = this.getCurLoc
    d match 
      case Right(v) => copy(x, x + v, y, y)
      case Left(v)  => copy(x, x + v, y, y)
      case Up(v)    => copy(x, x, y, y + v)
      case Down(v)  => copy(x, x, y, y + v)

  def getCurLoc: Location = 
    this match
      case Range(_, x, _, y) => Location(x, y)

  def overlap(r2: Range): Option[Location] = 
    val overlapRangeX: List[Int] = 
      this match 
      case Range(x, xe, _, _) => r2 match 
        case Range(x2, xe2, _, _) => posRange(x2, xe2).filter(a => posRange(x, xe).contains(a)).toList
    
    if (overlapRangeX.isEmpty) None else 
      val overlapRangeY: List[Int] = 
        this match 
        case Range(_, _, y, ye) => r2 match 
          case Range(_, _, y2, ye2) => posRange(y2, ye2).filter(a => posRange(y, ye).contains(a)).toList
         
      if (overlapRangeY.isEmpty) None else Some(Location(overlapRangeX.max, overlapRangeY.max))

def posRange(v: Int, v2: Int): scala.collection.immutable.Range = 
  if (v <= v2) (v to v2) else (v2 to v)  

def overlappingWires(w1: List[Range], w2: List[Range]): List[Location] = 
  val lo = for {
    r  <- w1
    r2 <- w2
  } yield(r.overlap(r2)) 
  
  lo.flatMap(x => x)

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
        .map(_.split(",").toList.map(x => Direction.makeDirection(x)))
        .toList


  val wire1Range: List[Range] = wire1.foldLeft(List(Range(0,0,0,0)))
                              ((acc, x) => acc.head.updateRange(x) :: acc)
  val wire2Range: List[Range] = wire2.foldLeft(List(Range(0,0,0,0)))
                              ((acc, x) => acc.head.updateRange(x) :: acc)

  val crosses = overlappingWires(wire1Range, wire2Range)

  val man = crosses.filter(x => x != Location(0,0)).map(x => manhattanDistance(x))

  println(man.min)

