package fps8

// def listOfN[A](n: Int, a: Gen[A]): Gen[List[A]] = ???

// def forAll[A](a: Gen[A])(f: A => Boolean): Prop = ???

object Prop:
  type FailedCase = String
  type SuccessCount = Int

trait Prop:
  import Prop.*
  def check: Boolean
  def &&(p: Prop): Prop = 
    new Prop:
      def check = this.check && p.check
  
  