package fps6
package cas6

trait RNG:
  def nextInt: (Int, RNG)

type Rand[+A] = RNG => (A, RNG)


case class State[S,+A](run: S => (A,S)):
  def flatMap[B](g: A => State[S, B]): State[S, B] = 
    State(s =>
      val (a, s1) = this.run(s)
      g(a).run(s1))

  def map[B](f: A => B): State[S, B] = 
    flatMap(x => State.unit(f(x)))

  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] = 
    flatMap(a => sb.map(b => f(a, b)))

object State:
  def unit[S, A](a: A): State[S, A] = 
    State(s => (a, s))

  def sequence[S, A](ls: List[State[S, A]]): State[S, List[A]] = 
    ls.foldRight(unit(List[A]()))((state, acc) => state.map2(acc)(_ :: _))


val int: Rand[Int] = _.nextInt

def unit[A](a: A): Rand[A] =
  rng => (a, rng)

val randIntDouble2: Rand[(Int, Double)] = 
  both(int, double)

val randDoubleInt2: Rand[(Double, Int)] = 
  both(double, int)

def nonNegativeEven: Rand[Int] = 
  map(nonNegativeInt)(i => i - i % 2)

val double2: Rand[Double] = 
  map(nonNegativeInt)(i => i / (Int.MaxValue.toDouble + 1))

case class SimpleRNG(seed: Long) extends RNG:
  def nextInt: (Int, RNG) =
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)

def nonNegativeInt(rng: RNG): (Int, RNG) = 
  val (num, rng1) = rng.nextInt  
  (if num < 0 then (math.abs(num + 1)) else num, rng1)    

def double(rng: RNG): (Double, RNG) = 
  val (num, rng1) = nonNegativeInt(rng)
  (num / (Int.MaxValue.toDouble + 1), rng1)

def intDouble(rng: RNG): ((Int, Double), RNG) = 
  val (int_num, rng1) = nonNegativeInt(rng)
  val (dou_num, rng2) = double(rng1)
  ((int_num, dou_num), rng2)

def doubleInt(rng: RNG): ((Double, Int), RNG) = 
  val ((int_num, dou_num), rng1) = intDouble(rng)
  ((dou_num, int_num), rng1)

def double3(rng: RNG): ((Double, Double, Double), RNG) = 
  val (dou1, rng1) = double(rng)
  val (dou2, rng2) = double(rng1)
  val (dou3, rng3) = double(rng2)
  ((dou1, dou2, dou3), rng3)

def ints(count: Int)(rng: RNG): (List[Int], RNG) = 
  if (count < 0) then 
    (List(), rng) 
    else 
      val (a, r)  = rng.nextInt
      val (b, r2) = ints(count - 1)(r)
      (a :: b, r2)

def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = 
  fs.foldRight(unit(List[A]()))((rand, acc) => map2(rand, acc)(_ :: _))

def map[A, B](s: Rand[A])(f: A => B): Rand[B] = 
  rng => 
    val (a, rng2) = s(rng)
    (f(a), rng2)

def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = 
  rng => 
    val (a, rng1) = ra(rng)
    val (b, rng2) = rb(rng1)
    (f(a,b), rng2)

def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] = 
  map2(ra, rb)((_,_))

def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = 
   rng => 
    val (a, rng1) = f(rng)
    g(a)(rng1)

def nonNegativeLessThan(n: Int): Rand[Int] = 
  flatMap(nonNegativeInt) {i =>
    val mod = i % n
    if (i + (n - 1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
  }

def mapFlat[A, B](s: Rand[A])(f: A => B): Rand[B] = 
  flatMap(s)(x => unit(f(x)))

def map2Flat[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = 
  flatMap(ra)(a => map(rb)(b => f(a, b)))


def modify[S](f: S => S): State[S, Unit] = for {
  s <- get
  _ <- set(f(s))
} yield ()

def get[S]: State[S, S] = State(s => (s, s))

def set[S](s: S): State[S, Unit] = State(_ => ((), s))


sealed trait Input 
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

def updateMachine(input: Input, m: Machine): Machine = 
  (input, m) match
    case (_, Machine(_, 0, _))          => m 
    case (Coin, Machine(false, ca, co)) => m
    case (Coin, Machine(true, ca, co))  => Machine(false, ca, co + 1)
    case (Turn, Machine(false, ca, co)) => Machine(true, ca - 1, co)
    case (Turn, Machine(true, ca, co))  => m

def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] =
  State(machine => 
    val mac = inputs.foldLeft(machine)((acc, input) => updateMachine(input, acc))
    ((mac.coins, mac.candies), mac))


object Chap6 extends App:
  
  //println(updateMachine(Coin, Machine(true, 5, 5)))
  val x = simulateMachine(List(Coin, Coin, Turn, Turn, Coin)).run(Machine(true, 9, 5))
  println(x)

  val rng = SimpleRNG(42)
  def plus(i: Int)(i2: Int): Int =
  i + i2

  val p = plus(2)
  val p2 = plus(2)(3)
  val p3 = p(3)

  println(p)
  println(p2)
  println(p3)

  // println(nonNegativeInt(SimpleRNG(6)))
  // println(double(rng))
  // println(intDouble(rng))
  // println(doubleInt(rng))
  // println(ints(5)(SimpleRNG(6)))
  // println(double2(rng))
  // println(nonNegativeEven(rng))
  // println(map2(nonNegativeEven, nonNegativeEven)(_ + _)(rng))
  // println(sequence(List(unit(1), unit(2), unit(3)))(rng)._1)
  // println(nonNegativeLessThan(5)(rng))
  // println(map(nonNegativeLessThan(4))(_ * 2)(rng))
  // println(mapFlat(nonNegativeLessThan(4))(_ * 2)(rng))
  // println(map2Flat(nonNegativeLessThan(10), nonNegativeLessThan(10))(_ + _)(rng))
  // println(unit(4)(rng))
  // println(State.unit(4))
  