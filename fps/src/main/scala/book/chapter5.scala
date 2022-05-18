package fps5

sealed trait Stream[+A]: 
  import Stream.*
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
      case Cons(h, t) if (n > 1)  => cons(h(), t().take(n - 1))
      case Cons(h, _) if (n == 1) => cons(h(), empty)
      case _                      => empty

  def takeUnfold(n: Int): Stream[A] = 
    unfold((this, n)) { case (Cons(h, t), 1) => Some(h(), (empty, n - 1))
                        case (Cons(h, t), n) => Some(h(), (t(), n - 1))
                        case _               => None
    }

  def drop(n: Int): Stream[A] = 
    this match 
      case Cons(_, t) if (n > 0)   => t().drop(n - 1)
      case _                       => this

  def takeWhile(p: A => Boolean): Stream[A] = 
    this match 
      case Cons(h, t) if (p(h())) => cons(h(), t().takeWhile(p))
      case _                      => empty

  def takeWhileUnfold(p: A => Boolean): Stream[A] = ???

  def forAll(p: A => Boolean): Boolean = 
    this match 
      case Empty      => true
      case Cons(h, t) => (p(h())) && t().forAll(p)

  def foldRight[B](z: => B)(f: (A, => B) => B): B = 
    this match 
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _          => z

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b)

  def takeWhileFold(p: A => Boolean): Stream[A] = 
    this.foldRight(empty)((a, z) => if p(a) then cons(a, z) else empty)

  def headOptionFold: Option[A] = 
    this.foldRight(None: Option[A])((a, z) => Some(a))

  def filter(f: A => Boolean): Stream[A] =
    this.foldRight(empty: Stream[A])((a, z) => if f(a) then cons(a, z) else z)

  def map[B](f: A => B): Stream[B] = 
    this.foldRight(empty: Stream[B])((a, z) => cons(f(a), z))

  def mapUnfold[B](f: A => B): Stream[B] = 
    unfold(this){ case Cons(h, t) => Some(f(h()), t())
                  case empty      => None }

  // A1 is a subtype of A  
  def append[A1 >: A](s: => Stream[A1]): Stream[A1] =
    this.foldRight(s)((a, z) => cons(a, z))

  def flatMap[B](f: A => Stream[B]): Stream[B] = 
    this.foldRight(empty: Stream[B])((a, z) => f(a).append(z))

  def zipWith[B, C](s: Stream[B])(f: (A, B) => C): Stream[C] = 
    unfold((this, s)){ case (Cons(h, t), Cons(h1, t1)) => Some(f(h(), h1()), (t(), t1()))
                       case _                          => None
                      }

  def zipAll[B](s: Stream[B]): Stream[(Option[A], Option[B])] = 
    unfold((this, s)) { case (Cons(h, t), Cons(h1, t1)) => Some(((Some(h()), Some(h1())), (t(), t1())))
                        case (Cons(h, t), Empty)        => Some(((Some(h()), None), (t(), empty)))
                        case (Empty, Cons(h, t))        => Some((None, Some(h())), (empty, t()))
                        case (Empty, Empty)             => None
  }

  def startsWith[A](s: Stream[A]): Boolean = 
    val together = this.zipAll(s) 

    together.takeWhile(streams => streams(1).isDefined).forAll { case (a,b) => a == b }
  
  def tails: Stream[Stream[A]] = 
    unfold(this){ case Cons(h, t)    => Some((cons(h(), t()), t()))
                  case Empty         => None
    }.append(Stream.empty)

  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] = 
    foldRight(z -> Stream(z)) { (a, b0) =>
      lazy val b1 = b0
      val b2 = f(a, b1._1)
      (b2, cons(b2, b1._2))
    }._2



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

  def constant[A](a: A): Stream[A] = 
    cons(a, constant(a))

  def from(n: Int): Stream[Int] = 
    val newN: Int = n + 1
    cons(newN, from(newN))

  def fibs(start: Int = 0, next: Int = 1): Stream[Int] = 
    cons(start, fibs(next, (next + start)))

  def unfold[A, S](curState: S)(f: S => Option[(A, S)]): Stream[A] = 
    f(curState) match 
      case Some((a,s)) => cons(a, unfold(s)(f))
      case None        => empty

  def fibsFold(start: Int = 0, next: Int = 1): Stream[Int] = 
    unfold((start, next)){ case (start, next) => 
      Some(start, (next, start + next))}

  def fromFold(n: Int): Stream[Int] = 
    unfold(n)(n => Some(n + 1, n + 1))

  def constantFold[A](a: A): Stream[A] = 
    unfold(a)(a => Some(a, a))


object Chap5 extends App: 
  val testStream = Stream(1, 2, 3, 4)

  println(testStream.toList)
  println(testStream.take(2).toList)
  println(testStream.drop(2).toList)
  println(testStream.takeWhile(x => x < 2).toList)
  println(testStream.takeWhileFold(x => x < 2).toList)
  println(testStream.headOption)
  println(testStream.headOptionFold)
  println(testStream.forAll(_ < 2))

  println(testStream.filter((_ % 2 == 0)).toList)

  println(Stream.constant("5").take(7).toList)
  println(Stream.from(5).take(4).toList)
  println(Stream.fromFold(5).take(4).toList)
  println(Stream.fibs().take(10).toList)
  println(Stream.fibsFold().take(10).toList)

  println(testStream.map(_ + 10).toList)
  println(testStream.mapUnfold(_ + 10).toList)

  println(testStream.tails.map(_.toList).toList)