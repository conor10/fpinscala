package fpinscala.testing

import fpinscala.laziness.Stream
import fpinscala.state.RNG.Simple
import fpinscala.state._
import fpinscala.parallelism._
import fpinscala.parallelism.Par.Par
import Gen._
import Prop._
import java.util.concurrent.{Executors,ExecutorService}

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
*/

trait SimpleProp {
  def check: Boolean
  def &&(p: SimpleProp): SimpleProp = new SimpleProp {
    def check: Boolean = SimpleProp.this.check && p.check
  }
}


case class Prop(run: (TestCases, RNG) => Result) {
  def &&(p: Prop): Prop = Prop((tc, rng) => run(tc, rng) match {
    case Passed => {
      p.run(tc, rng) match {
        case Falsified(msg, n) => Falsified(msg + " (RightProp)", n)
        case Passed => Passed
      }
    }
    case Falsified(msg, n) => Falsified(msg + " (LeftProp)", n)
  })

  def ||(p: Prop): Prop = Prop((tc, rng) => run(tc, rng) match {
    case Passed => Passed
    case Falsified(msg, n) => p.run(tc, rng)
  })
}


object Prop {
  type SuccessCount = Int
  type TestCases = Int
  type FailedCase = String
//  type Result = Option[(FailedCase, SuccessCount)]

  sealed trait Result {
    def isFalsified: Boolean
  }
  case object Passed extends Result {
    def isFalsified = false
  }
  case class Falsified(failure: FailedCase,
                       successes: SuccessCount) extends Result {
    def isFalsified = true
  }


  /* Produce an infinite random stream from a `Gen` and a starting `RNG`. */
  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
    (n,rng) => randomStream(as)(rng).zip(Stream.from(0)).take(n).map {
      case (a, i) => try {
        if (f(a)) Passed else Falsified(a.toString, i)
      } catch { case e: Exception => Falsified(buildMsg(a, e), i) }
    }.find(_.isFalsified).getOrElse(Passed)
  }

  // String interpolation syntax. A string starting with `s"` can refer to
  // a Scala value `v` as `$v` or `${v}` in the string.
  // This will be expanded to `v.toString` by the Scala compiler.
  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
      s"generated an exception: ${e.getMessage}\n" +
      s"stack trace:\n ${e.getStackTrace.mkString("\n")}"
}

object Gen {
  def unit[A](a: => A): Gen[A] = Gen(State(s => (a, s)))

  def boolean: Gen[Boolean] = Gen(State(s => RNG.boolean(s)))

  def double: Gen[Double] = Gen(State(s => RNG.double(s)))

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
//    Gen(State(RNG.sequence(List.fill(n)(g.sample.run))))
    Gen(State.sequence(List.fill(n)(g.sample)))

  def choose(start: Int, stopExclusive: Int): Gen[Int] = {
    // my initial solution
    Gen(State(RNG.flatMap(RNG.nonNegativeInt)(i => {rng => {
      val r = start + i % (stopExclusive - start)
        (r, rng)
    }})))
//    Gen(State(RNG.int).map(
//      i => start + i % (stopExclusive - start)
//    ))
  }

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    boolean.flatMap(b => if (b) g1 else g2)

  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
    val limit = g1._2 / (g1._2 + g2._2)

    double.flatMap(d => if (d < limit) g1._1 else g2._1)
  }
}


case class Gen[A](sample: State[RNG, A]) {
  def map[B](f: A => B): Gen[B] = Gen(sample.map(f))
  def flatMap[B](f: A => Gen[B]): Gen[B] =
    Gen(sample.flatMap(a => f(a).sample))

  def listOfN(n: Int): Gen[List[A]] = Gen.listOfN(n, this)

  def listOfN(size: Gen[Int]): Gen[List[A]] =
    size.flatMap(s => listOfN(s))

  def unsized: SGen[A] = SGen(_ => this)
}


object SGen {
  def unit[A](a: A): SGen[A] = SGen(x => Gen.unit(a))
}

case class SGen[+A](forSize: Int => Gen[A]) {
  def map[B](f: A => B): SGen[B] = SGen(x => forSize(x).map(f))
  def flatMap[B](f: A => SGen[B]): SGen[B] =
    SGen(x => forSize(x).flatMap(y => f(y).forSize(x)))

  def listOfN(n: Int): SGen[List[A]] = SGen(x => forSize(x).listOfN(n))

  def listOfN(size: SGen[Int]): SGen[List[A]] =
    size.flatMap(s => listOfN(s))

  def listOf[A](g: Gen[A]): SGen[List[A]] =
    SGen(n => g.listOfN(n))

}

