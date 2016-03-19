package fpinscala.monoids

import fpinscala.parallelism.Nonblocking._
import fpinscala.parallelism.Nonblocking.Par.toParOps // infix syntax for `Par.map`, `Par.flatMap`, etc

trait Monoid[A] {
  def op(a1: A, a2: A): A
  def zero: A
}

object Monoid {

  val stringMonoid = new Monoid[String] {
    def op(a1: String, a2: String) = a1 + a2
    val zero = ""
  }

  def listMonoid[A] = new Monoid[List[A]] {
    def op(a1: List[A], a2: List[A]) = a1 ++ a2
    val zero = Nil
  }

  val intAddition: Monoid[Int] = new Monoid[Int] {
    def op(a1: Int, a2: Int): Int = a1 + a2
    def zero: Int = 0
  }

  val intMultiplication: Monoid[Int] = new Monoid[Int] {
    def op(a1: Int, a2: Int): Int = a1 * a2
    def zero: Int = 1
  }

  val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2
    def zero: Boolean = false
  }

  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2
    def zero: Boolean = true
  }

  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    def op(a1: Option[A], a2: Option[A]): Option[A] = a1.orElse(a2)
    def zero: Option[A] = None
  }

  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    def op(f1: A => A, f2: A => A): A => A = f1 andThen f2
    def zero: A => A = x => x
  }


  import fpinscala.testing._
  import Prop._
  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop =
    forAll(gen) {
      (x: A) => m.op(x, m.zero) == x &&
        m.op(m.zero, x) == x &&
        m.op(m.zero, m.zero) == m.zero
        m.op(m.op(x, m.zero), x) == m.op(x, m.op(m.zero, x))
    }

  def trimMonoid(s: String): Monoid[String] = ???

  def concatenate[A](as: List[A], m: Monoid[A]): A =
    as.foldLeft(m.zero)(m.op)

  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
    as.map(f).foldLeft(m.zero)(m.op)

  // Ex 10.6
  def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B = ???

  // Ex 10.6
  def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B = ???

  def foldMapV[A,B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): B =
    as.length match {
      case 0 => m.zero
      case 1 => f(as.head)
      case _ => {
        val (l1, l2) = as.splitAt(as.size / 2)
        m.op(foldMapV(l1, m)(f), foldMapV(l2, m)(f))
      }
    }

  def ordered(ints: IndexedSeq[Int]): Boolean = ???

  import fpinscala.parallelism.Nonblocking._
  def par[A](m: Monoid[A]): Monoid[Par[A]] =
    new Monoid[Par[A]] {
      def op(a1: Par[A], a2: Par[A]): Par[A] = a1.map2(a2)(m.op)
      val zero: Par[A] = Par.unit(m.zero)
    }

  def parFoldMap[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] = 
    foldMapV(v, par(m))(a => Par.unit(f(a)))

  sealed trait WC
  case class Stub(chars: String) extends WC
  case class Part(lStub: String, words: Int, rStub: String) extends WC

  val wcMonoid: Monoid[WC] = new Monoid[WC] {
    def op(a1: WC, a2: WC): WC = (a1, a2) match {
      case (a1: Stub, a2: Stub) => Part(a1.chars, 0, a2.chars)
      case (a1: Stub, a2: Part) => Part(a1.chars + a2.lStub, a2.words, a2.rStub)
      case (a1: Part, a2: Stub) => Part(a1.lStub, a1.words, a2.chars + a1.lStub)
      case (a1: Part, a2: Part) => Part(a1.lStub + a2.rStub, a1.words + a2.words, a1.rStub + a2.rStub)
    }
    def zero: WC = Stub("")
  }

  // Ex 10.11 - WIP
  def count(s: String): Int = {
    def process(c: Char): WC = {
      if (c.isWhitespace) Part("", 0, "")
      else Stub(c.toString)
    }
    val result = foldMapV(s.toIndexedSeq, wcMonoid)(process)
    result match {
      case Stub(x) => x.length
      case Part(l, count, r) => count
    }
  }

  def productMonoid[A,B](A: Monoid[A], B: Monoid[B]): Monoid[(A, B)] = new Monoid[(A, B)] {
    override def op(a1: (A, B), a2: (A, B)): (A, B) = (A.op(a1._1, a2._1), B.op(a1._2, a2._2))
    override def zero: (A, B) = (A.zero, B.zero)
  }

  def functionMonoid[A,B](B: Monoid[B]): Monoid[A => B] = new Monoid[(A) => B] {
    override def op(a1: A => B, a2: A => B): A => B = x => B.op(a1(x), a2(x))
    override def zero: A => B = _ => B.zero
  }

  def mapMergeMonoid[K,V](V: Monoid[V]): Monoid[Map[K, V]] = new Monoid[Map[K, V]] {
      def zero = Map[K,V]()
      def op(a: Map[K, V], b: Map[K, V]) =
        (a.keySet ++ b.keySet).foldLeft(zero) { (acc,k) =>
          acc.updated(k, V.op(a.getOrElse(k, V.zero),
            b.getOrElse(k, V.zero)))
        }
    }

  def bag[A](as: IndexedSeq[A]): Map[A, Int] = {
    // Can use foldMapV too
    val M: Monoid[Map[A,Int]] = mapMergeMonoid(intAddition)
    as.foldLeft(M.zero){ case (acc, a) => M.op(acc, Map(a -> 1))}
  }

}

trait Foldable[F[_]] {
  import Monoid._

  // Not stated as exercises for some reason
  def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B = ???

  def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B = ???

  def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B = ???

  def concatenate[A](as: F[A])(m: Monoid[A]): A =
    foldLeft(as)(m.zero)(m.op)

  def toList[A](as: F[A]): List[A] =
    foldRight(as)(Nil: List[A])(_ :: _)
}

object ListFoldable extends Foldable[List] {
  override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B) =
    as.foldRight(z)(f)
  override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B) =
    as.foldLeft(z)(f)
  override def foldMap[A, B](as: List[A])(f: A => B)(mb: Monoid[B]): B =
    as.foldLeft(mb.zero){case (acc, a) => mb.op(acc, f(a))}
}

object IndexedSeqFoldable extends Foldable[IndexedSeq] {
  override def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B) =
    as.foldRight(z)(f)
  override def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B) =
    as.foldLeft(z)(f)
  override def foldMap[A, B](as: IndexedSeq[A])(f: A => B)(mb: Monoid[B]): B =
    as.foldLeft(mb.zero){case (acc, a) => mb.op(acc, f(a))}
}

object StreamFoldable extends Foldable[Stream] {
  override def foldRight[A, B](as: Stream[A])(z: B)(f: (A, B) => B) =
    as.foldRight(z)(f)
  override def foldLeft[A, B](as: Stream[A])(z: B)(f: (B, A) => B) =
    as.foldLeft(z)(f)
}

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object TreeFoldable extends Foldable[Tree] {
  override def foldMap[A, B](as: Tree[A])(f: A => B)(mb: Monoid[B]): B = as match {
    case Leaf(x) => f(x)
    case Branch(l, r) => mb.op(foldMap(l)(f)(mb), foldMap(r)(f)(mb))
  }

  override def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B) = as match {
    case Leaf(x) => f(z, x)
    case Branch(l, r) => foldLeft(r)(foldLeft(l)(z)(f))(f)
  }

  override def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B) = as match {
    case Leaf(x) => f(x, z)
    case Branch(l, r) => foldRight(l)(foldRight(r)(z)(f))(f)
  }
}

object OptionFoldable extends Foldable[Option] {
  override def foldMap[A, B](as: Option[A])(f: A => B)(mb: Monoid[B]): B = as match {
    case Some(x) => f(x)
    case None => mb.zero
  }

  override def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B) = as match {
    case Some(x) => f(z, x)
    case None => z
  }

  override def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B) = as match {
    case Some(x) => f(x, z)
    case None => z
  }
}

