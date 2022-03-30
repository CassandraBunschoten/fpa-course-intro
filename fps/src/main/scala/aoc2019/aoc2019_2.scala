package AOC2019

import scala.io.Source

sealed abstract class OpCode(code: Int)
case object Halt extends OpCode(99)
sealed abstract class Inst(code: Int, loc1: Int, loc2: Int, loc3: Int) extends OpCode(code)
case class Add(loc1: Int, loc2: Int, loc3: Int) extends Inst(1, loc1, loc2, loc3)
case class Mul(loc1: Int, loc2: Int, loc3: Int) extends Inst(2, loc1, loc2, loc3)

case class CPU(memory: List[Int], counter: Int = 0, halted: Boolean = false):
  def run: Int =
    step match
      case CPU(m, c, true) => m(0)
      case cpu: CPU        => cpu.run

  def step: CPU =
    program match
      case Add(l1, l2, l3) => exec(l1, l2, l3, _ + _)
      case Mul(l1, l2, l3) => exec(l1, l2, l3, _ * _)
      case Halt            => copy(halted = true)

  def exec(l1: Int, l2: Int, l3: Int, f: (Int, Int) => Int): CPU =
      val v1 = memory(l1)
      val v2 = memory(l2)
      copy(memory = memory.updated(l3, f(v1, v2)), counter = counter + 1)

  def program: OpCode =
    val List(code, c1, c2, c3) = memory.grouped(4).toList(counter)
    code match
      case 1  => Add(c1, c2, c3)
      case 2  => Mul(c1, c2, c3)
      case 99 => Halt

object AOC2019_2 extends App:

  val cpu: CPU =
    CPU(
      Source
        .fromFile("fps/src/main/resources/aoc2019/input2019_2.txt")
        .mkString
        .split(",")
        .map(_.toInt)
        .toList)

  val output = (for{
    noun    <- Range(0, 99, 1)
    verb    <- Range(0, 99, 1)
    updated  = cpu.copy(memory = cpu.memory.updated(1, noun))
    finalv   = updated.copy(memory = updated.memory.updated(2, verb))
  } yield (noun, verb, finalv.run))
  .filter((x, y, z) => z == 19690720)

  println(output)
        