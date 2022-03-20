package aoc 

import scala.io.Source

object AOC3 extends App:
  val diagnostics = 
    Source
    .fromFile("fps/src/main/resources/input03.txt")
    .getLines
    .flatMap(_.split(""))
    .map(_.toInt)
    .toList
    .grouped(12)
    .toList

  val gamma = diagnostics
              .transpose
              .map(_.sum)
              .map(x => if(x > (diagnostics.length.toDouble/2)) "1" else "0" )
              .foldRight("")(_ + _)

  val epsilon = diagnostics
                .transpose
                .map(_.sum)
                .map(x => if(x < (diagnostics.length.toDouble/2)) "1" else "0" )
                .foldRight("")(_ + _)


  val gammaDecimal   = Integer.parseInt(gamma, 2)
  val epsilonDecimal = Integer.parseInt(epsilon, 2)

  println(s"Gamma is $gammaDecimal, epsilon is $epsilonDecimal, multiplied result is ${gammaDecimal * epsilonDecimal}")

  // Part 2
  
  def splitBin(l: List[String], nSplit: Int): List[List[Int]] = 
    l.flatMap(_.split(""))
    .map(_.toInt)
    .toList
    .grouped(nSplit)
    .toList

  def collapseBin(l: List[List[Int]]): List[String] = 
    l.map(x => x.foldRight("")(_.toString + _))

  def filterPrefix(l: List[List[Int]], p: String, acc: String): List[List[Int]] = 
    splitBin(collapseBin(l).filter(_.startsWith(p)), 12 - acc.length).map(_.drop(1))

  def oxyGen(l: List[List[Int]], acc: String): String = 
    if (l.length == 1) then (acc :: collapseBin(l)).head else
    l.transpose match {
      case Nil    => acc
      case h :: t => oxyGen(filterPrefix(l, 
                                        (if (h.foldLeft(0)(_ + _) >= h.length.toDouble/2) "1" else "0"), 
                                        acc),
                                        acc + (if (h.foldLeft(0)(_ + _) >= h.length.toDouble/2) "1" else "0"))
     } 
  

  def co2Scrub(l: List[List[Int]], acc: String): String = 
    if (l.length == 1) then (acc + collapseBin(l).head) else
    l.transpose match {
      case Nil    => acc
      case h :: t => co2Scrub(filterPrefix(l, 
                                        (if (h.foldLeft(0)(_ + _) < h.length.toDouble/2) "1" else "0"), 
                                         acc),
                                         acc + (if (h.foldLeft(0)(_ + _) < h.length.toDouble/2) "1" else "0"))
      } 

  val oxygen = oxyGen(diagnostics, "")
  val co2    = co2Scrub(diagnostics, "")

  val oxyDecimal   = Integer.parseInt(oxygen, 2)
  val co2Decimal = Integer.parseInt(co2, 2)

  println("oxy gen is " + oxygen + " which is " + oxyDecimal)
  println("CO2 scrubber is " + co2 + " which is " + co2Decimal)  
  println("together multiplied is " + oxyDecimal*co2Decimal)
