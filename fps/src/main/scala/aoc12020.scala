package aoc 

import scala.io.Source

object AO12020 extends App:
    val expenses = Source
        .fromFile("fps/src/main/resources/testinput.txt").getLines.map(_.toInt).toList

    // def combo(l: List[Int]): List[Int] = 
      // for(a <- list; b <- list) println(a+" - "+b)


    for (
        a <- expenses;
        b <- expenses
    ) if (a + b == 2020) then println(s"$a - $b + ${a*b}")  

    println(expenses)


