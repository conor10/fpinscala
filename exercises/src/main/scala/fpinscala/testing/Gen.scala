package fpinscala.testing

import fpinscala.laziness.Stream
import fpinscala.state._
import fpinscala.parallelism._
import fpinscala.parallelism.Par.Par
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

case class Prop(run: (MaxSize,TestCases,RNG) => Result) {
  def &&(p: Prop): Prop = Prop((max, tc, rng) => run(max, tc, rng) match {
    case Passed => {
      p.run(max, tc, rng) match {
        case Falsified(msg, n) => Falsified(msg + " (RightProp)", n)
        case x => x
      }
    }
    case Falsified(msg, n) => Falsified(msg + " (LeftProp)", n)
  })

  def ||(p: Prop): Prop = Prop((max, tc, rng) => run(max, tc, rng) match {
    case Falsified(msg, n) => p.run(max, tc, rng)
    case x => x
  })

}


object Prop {
  type SuccessCount = Int
  type TestCases = Int
  type MaxSize = Int
  type FailedCase = String

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
  case object Proved extends Result {
    def isFalsified = false
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

  // These all need to be added in section 8.4.1 - not clear in the book
  def apply(f: (TestCases,RNG) => Result): Prop =
    Prop { (_,n,rng) => f(n,rng) }

  def forAll[A](g: SGen[A])(f: A => Boolean): Prop = forAll(g(_))(f)

  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
    (max,n,rng) =>
      val casesPerSize = (n - 1) / max + 1
      val props: Stream[Prop] =
        Stream.from(0).take((n min max) + 1).map(i => forAll(g(i))(f))
      val prop: Prop =
        props.map(p => Prop { (max, n, rng) =>
          p.run(max, casesPerSize, rng)
        }).toList.reduce(_ && _)
      prop.run(max,n,rng)
  }

  def run(p: Prop,
          maxSize: Int = 100,
          testCases: Int = 100,
          rng: RNG = RNG.Simple(System.currentTimeMillis)): Unit =
    p.run(maxSize, testCases, rng) match {
      case Falsified(msg, n) =>
        println(s"! Falsified after $n passed tests:\n $msg")
      case Passed =>
        println(s"+ OK, passed $testCases tests.")
      case Proved =>
        println(s"+ OK, proved property.")
    }

  def check(p: => Boolean): Prop =
    Prop { (_, _, _) => if (p) Proved else Falsified("()", 0) }

}

object Gen {
  def unit[A](a: => A): Gen[A] = Gen(State(s => (a, s)))

  def boolean: Gen[Boolean] = Gen(State(s => RNG.boolean(s)))

  def double: Gen[Double] = Gen(State(s => RNG.double(s)))

  def int: Gen[Int] = Gen(State(s => RNG.int(s)))

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

  def listOf[A](g: Gen[A]): SGen[List[A]] =
    SGen(n => g.listOfN(n))

  def listOf1[A](g: Gen[A]): SGen[List[A]] =
    SGen(n => g.listOfN(n max 1))

  val smallInt = Gen.choose(-10,10)
  val maxProp = forAll(listOf1(smallInt)) { l =>
    val max = l.max
    !l.exists(_ > max) // No value greater than `max` should exist in `l`
  }

  val sortedProp = forAll(listOf(smallInt)) {
    l => l.isEmpty ||
      l.tail == Nil || {
        val sorted = l.sorted
        sorted.zip(sorted.tail).forall{ case (a, b) => a <= b}
      }
  }

  val ES: ExecutorService = Executors.newCachedThreadPool
  val p1 = Prop.forAll(Gen.unit(Par.unit(1)))(i =>
    Par.map(i)(_ + 1)(ES).get == Par.unit(2)(ES).get)

//  val p2 = Prop.check {
//    val p = Par.map(Par.unit(1))(_ + 1)
//    val p2 = Par.unit(2)
//    p(ES).get == p2(ES).get
//  }

  def equal[A](p: Par[A], p2: Par[A]): Par[Boolean] = Par.map2(p,p2)(_ == _)
  val p3 = check { equal(
    Par.map(Par.unit(1))(_ + 1),
    Par.unit(2)
  )(ES).get
  }

  val S = weighted(
    choose(1,4).map(Executors.newFixedThreadPool) -> .75,
    unit(Executors.newCachedThreadPool) -> .25)

  object ** {
    def unapply[A,B](p: (A,B)) = Some(p)
  }

  def forAllPar[A](g: Gen[A])(f: A => Par[Boolean]): Prop =
    forAll(S ** g) { case s ** a => f(a)(s).get }

  def checkPar(p: Par[Boolean]): Prop =
    forAllPar(Gen.unit(()))(_ => p)

  val p2 = checkPar {
    equal(
      Par.map(Par.unit(1))(_ + 1),
      Par.unit(2)
    )
  }

  val pint = Gen.choose(0,10) map (Par.unit(_))
  val p4 = forAllPar(pint)(n => equal(Par.map(n)(y => y), n))

  val forkProp = forAllPar(pint)(n => equal(Par.fork(n), n))

  val isEven = (i: Int) => i%2 == 0
  val takeWhileProp = forAll(listOf(smallInt)){
    l => l.takeWhile(isEven).forall(isEven)
  }

  val takeWhileDropWhileProp = forAll(listOf(smallInt)){
    l => ((l.takeWhile(isEven) ++ l.dropWhile(isEven)).sorted == l.sorted)
  }

  val takeProp = forAll(listOf(smallInt)){
    l => (l.take(l.size) == Nil) &&
      (l.take(0) == l) && {
      val n = l.size / 2
      (l.take(l.size min n).size == n)
    }
  }

  val dropProp = forAll(listOf(smallInt)){
    l => (l.drop(l.size) == Nil) &&
      (l.drop(0) == l) && {
      val n = l.size / 2
      (l.drop(l.size min n).size == l.size - n)
    }
  }

  val filterProp = forAll(listOf(smallInt)){
    l => l.takeWhile(isEven).forall(isEven)
  }
}


case class Gen[+A](sample: State[RNG, A]) {
  def map[B](f: A => B): Gen[B] = Gen(sample.map(f))
  def map2[B,C](other: Gen[B])(f: (A, B) => C): Gen[C] =
    Gen(sample.map2(other.sample)(f))
  def flatMap[B](f: A => Gen[B]): Gen[B] =
    Gen(sample.flatMap(a => f(a).sample))

  def listOfN(n: Int): Gen[List[A]] = Gen.listOfN(n, this)

  def listOfN(size: Gen[Int]): Gen[List[A]] =
    size.flatMap(s => listOfN(s))

  def unsized: SGen[A] = SGen(_ => this)

  def **[B](g: Gen[B]): Gen[(A,B)] = (this map2 g)((_,_))
}


case class SGen[+A](forSize: Int => Gen[A]) {

  def apply(n: Int): Gen[A] = forSize(n)
  def map[B](f: A => B): SGen[B] = SGen(forSize andThen (_ map f))

//  def map[B](f: A => B): SGen[B] = SGen(x => forSize(x).map(f))
  def flatMap[B](f: A => SGen[B]): SGen[B] =
    SGen(x => forSize(x).flatMap(y => f(y).forSize(x)))

  def listOfN(n: Int): SGen[List[A]] = SGen(x => forSize(x).listOfN(n))

  def listOfN(size: SGen[Int]): SGen[List[A]] =
    size.flatMap(s => listOfN(s))

  def listOf[A](g: Gen[A]): SGen[List[A]] =
    SGen(n => g.listOfN(n))
}

