
package cas
package fps8

//package fps6

import fps6.*


// def listOfN[A](n: Int, a: Gen[A]): Gen[List[A]] = ???

// def forAll[A](a: Gen[A])(f: A => Boolean): Prop = ???

object Prop:
  type FailedCase = String
  type SuccessCount = Int

trait Prop:
  import Prop.*
  //def check: Either[(FailedCase, SuccessCount), SuccessCount]
  def check: Boolean
    def &&(p: Prop): Prop = 
      new Prop:
        def check = this.check && p.check

// case class Gen[A](sample: State[RNG, A]) : 
//   def choose(start: Int, stopExclusive: Int): Gen[Int] = 
//     val (rng: RNG, a: Int) = sample
//     nonNegativeInt(rng)
    
    // def checkRange(rng: RNG): (Int, RNG)  =  
    //   rng =>   
    //     val (num: Int, rng1: SimpleRNG) = rng.nextInt  
    //       (if num > start & num <= stopExclusive then (num, rng1) else checkRange(rng1))
  
// object chap8 extends App: 
//   println(Gen.choose(5, 60)(SimpleRNG(20)))  
