package aoc2020
import scala.io.Source

// Right 1, down 1.
// Right 3, down 1.
// Right 5, down 1.
// Right 7, down 1.
// Right 1, down 2.

object AOC32020 extends App:

    val input11 = Source
        .fromFile("fps/src/main/resources/input32020.txt")
        .getLines
        .toList
        .zipWithIndex
        .map(_ match 
          case (string, int) => string.charAt((int) % string.length) == '#')
        .count(_ == true).toLong

    val input31 = Source
        .fromFile("fps/src/main/resources/input32020.txt")
        .getLines
        .toList
        .zipWithIndex
        .map(_ match 
          case (string, int) => string.charAt((int * 3) % string.length) == '#')
        .count(_ == true).toLong

    val input51 = Source
        .fromFile("fps/src/main/resources/input32020.txt")
        .getLines
        .toList
        .zipWithIndex
        .map(_ match 
          case (string, int) => string.charAt((int * 5) % string.length) == '#')
        .count(_ == true).toLong

    val input71 = Source
        .fromFile("fps/src/main/resources/input32020.txt")
        .getLines
        .toList
        .zipWithIndex
        .map(_ match 
          case (string, int) => string.charAt((int * 7) % string.length) == '#')
        .count(_ == true).toLong

      val input12 = Source
        .fromFile("fps/src/main/resources/input32020.txt")
        .getLines
        .toList
        .zipWithIndex
        .map(_ match
          case (string, int) => if (int % 2 == 0) then string.charAt((int - (int / 2)) % string.length) == '#' else false)
        .count(_ == true).toLong

    println(s"$input11 * $input31 * $input51 * $input71 * $input12")
    println(input11 * input31 * input51 * input71 * input12)