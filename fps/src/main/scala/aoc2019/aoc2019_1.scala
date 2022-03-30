package AOC2019

import scala.io.Source

def fuel2(element: Double, acc: Double): Double = 
  val x = ((element/3).floor - 2)
  if (x > 0) fuel2(x, acc + x) else acc

object AOC2019_1 extends App:

  val modules: List[Double] = 
    Source
      .fromFile("fps/src/main/resources/aoc2019/input2019_1.txt")
      .getLines
      .map(_.toDouble)
      .toList

  println(modules.map(x => ((x/3).floor - 2)).sum)
  println(modules.map(fuel2(_, 0.0)).sum)