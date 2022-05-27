package AOC2019

import scala.io.Source

import AOC2019.Direction.*

case class Pos(x: Int, y: Int)

def path(ld: List[Direction], acc: List[Pos] = List(Pos(0,0))): List[Pos] = 

  def oneDirection(d: Direction, p: Pos): List[Pos] =
    d match 
      case Right(v) => (1 to v).map(step => p.copy(x = p.x + step)).toList
      case Left(v)  => (1 to -v).map(step => p.copy(x = p.x - step)).toList
      case Up(v)    => (1 to v).map(step => p.copy(y = p.y + step)).toList
      case Down(v)  => (1 to -v).map(step => p.copy(y = p.y - step)).toList

  ld match 
    case Nil    => acc
    case h :: t => path(t, acc ::: oneDirection(h, acc.last))

def crosses(wire: List[Direction], wire1: List[Direction]): List[Pos] = 
  path(wire).intersect(path(wire1))   

def manDistance(p: Pos): Int =
  p match 
    case Pos(x,y) => Math.abs(x) + Math.abs(y)

implicit val manhattanOrdering: Ordering[Pos] = 
  new Ordering[Pos]{
    def compare(p: Pos, p1: Pos): Int = 
      manDistance(p).compare(manDistance(p1))
   }   

// Revisit later, this is bad xD
// and hard to understand

def crossesTrackSteps(wire: List[Direction], wire1: List[Direction]) = 
  val tracked  = path(wire).zipWithIndex.toMap  
  val tracked1 = path(wire1).zipWithIndex.toMap 

  val crosses = tracked.keySet.intersect(tracked1.keySet)

  val cross   = tracked.filter{ case (key, value) => crosses.contains(key) && key != Pos(0,0)}
  val cross1  = tracked1.filter{ case (key, value) => crosses.contains(key) && key != Pos(0,0)}

  val steps = cross.flatMap(x => cross1.map(y => if (x._1 == y._1) then (x._2 + y._2) else 0))

  steps.filterNot(_ == 0).min


object AOC2019_32 extends App: 
  val List(wire, wire1) = 
        Source
        .fromFile("fps/src/main/resources/aoc2019/input2019_3.txt")
        .getLines
        .toList
        .map(_.split(",").toList.map(x => makeDirection(x)))
        .toList

  val closeCross = crosses(wire, wire1).filterNot(cross => cross == Pos(0,0)).min      

  println("Answer part 1 = " + manDistance(closeCross))

  println("Answer part 2 = " + crossesTrackSteps(wire, wire1))