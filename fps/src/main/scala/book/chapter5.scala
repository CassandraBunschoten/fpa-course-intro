package fps5

sealed trait Stream[+A]: 
  def headOption: Option[A] = 
    this match
      case Empty      => None
      case Cons(h, t) => Some(h())

  def toList: List[A] = 
    this match 
      case Empty => Nil
      case Cons(h, t) => h() :: t().toList

  def take(n: Int): Stream[A] = 
    this match 
      case Cons(h, t) if (n > 1)  => Stream.cons(h(), t().take(n - 1))
      case Cons(h, _) if (n == 1) => Stream.cons(h(), Stream.empty)
      case _                      => Stream.empty

  def drop(n: Int): Stream[A] = 
    this match 
      case Cons(_, t) if (n > 0)   => t().drop(n - 1)
      case _                       => this

  def takeWhile(p: A => Boolean): Stream[A] = 
    this match 
      case Cons(h, t) if (p(h())) => Stream.cons(h(), t().takeWhile(p))
      case _                      => Stream.empty


case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]


object Stream:
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = 
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] = 
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))




object Chap5 extends App: 
  val testStream = Stream(1, 2, 3, 4)

  println(testStream.toList)
  println(testStream.take(2).toList)
  println(testStream.drop(2).toList)
  println(testStream.takeWhile(x => x < 2).toList)