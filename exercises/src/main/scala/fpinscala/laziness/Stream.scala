package fpinscala.laziness

import Stream._
trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }
  def take(n: Int): Stream[A] = {
    def loop(a: => Stream[A], acc: => Stream[A], count: Int): Stream[A] = {
      if (count == n) acc
      else a match {
        case Empty => acc
        case Cons(h, t) => cons(h(), loop(t(), acc, count+1))
      }
    }
    loop(this, empty, 0)
  }

  def drop(n: Int): Stream[A] = {
    def loop(a: => Stream[A], count: Int): Stream[A] = {
      if (count == n) a
      else a match {
        case Empty => a
        case Cons(h, t) => loop(t(), count+1)
      }
    }
    loop(this, 0)
  }

  def takeWhile(p: A => Boolean): Stream[A] = {
    def loop(a: => Stream[A], acc: => Stream[A]): Stream[A] = {
      a match {
        case Empty => acc
        case Cons(h, t) => if (p(h())) cons(h(), loop(t(), acc)) else acc
      }
    }

    loop(this, empty)
  }

  def takeWhileFold(p: A => Boolean): Stream[A] =
    foldRight(empty: Stream[A])((a, b) => if (p(a)) cons(a, b) else empty)

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  def toList: List[A] = {
    def loop(a: Stream[A], acc: List[A]): List[A] = a match {
      case Cons(h, t) => loop(t(), acc :+ h())
      case Empty => acc
    }
    loop(this, List())
  }

  def headOption: Option[A] =
    foldRight(None: Option[A])((h, t) => Some(h))

  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])((h, t) => cons(f(h), t))

  // Model answer provides type of [B], it's not clear why
  def filter(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((h, t) => if (p(h)) cons(h, t) else t)

  // non-strict in its argument hence B>:A
  def append[B>:A](a: Stream[B]): Stream[B] =
    foldRight(a)((h, t) => cons(h, t))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((h, t) => f(h).append(t))

  def mapUnfold[B](f: A => B): Stream[B] = unfold(this)
  {
    case Cons(h, t) => Some(f(h()), t())
    case _ => None
  }

  def takeUnfold(n: Int): Stream[A] = unfold(this, 0)
  {
    case (Cons(h, t), count) =>
      if (count != n) Some(h(), (t(), count+1)) else None
    case (empty, count) => None
  }

  def takeWhileUnfold(p: A => Boolean): Stream[A] = unfold(this)
  {
    case Cons(h, t) => if (p(h())) Some(h(), t()) else None
    case _ => None
  }

  def zipWith[B,C](a2: Stream[B])(f: (A, B) => C): Stream[C] = unfold(this, a2)
  {
    case (Cons(h1, t1), Cons(h2, t2)) => Some(f(h1(), h2()), (t1(), t2()))
    case _ => None
  }

  // special case of `zip` - used in Gen.scala
  def zip[B](s2: Stream[B]): Stream[(A,B)] =
    zipWith(s2)((_,_))

  // TODO: Find out why this doesn't work - commenting out either of the two middle case statements gets rid of the compiler error
  // http://stackoverflow.com/questions/25585759/weird-type-mismatch-in-zipall-implementation-in-scala
//  def zipAll[B](a2: Stream[B]): Stream[(Option[A],Option[B])] = unfold(this, a2)
//  {
//    case (Cons(h1, t1), Cons(h2, t2)) => Some((Some(h1()), Some(h2())), (t1(), t2()))
//    case (Cons(h1, t1), Empty) => Some((Some(h1()), Option.empty[B]), (t1(), empty[B]))
//    case (Empty, Cons(h2, t2)) => Some((Option.empty[A], Some(h2())), (empty[A], t2()))
//    case _ => None
//  }

  // Authors solution
  def zipAll[B](s2: Stream[B]): Stream[(Option[A],Option[B])] =
    zipWithAll(s2)((_,_))

  def zipWithAll[B, C](s2: Stream[B])(f: (Option[A], Option[B]) => C): Stream[C] =
    Stream.unfold((this, s2)) {
      case (Empty, Empty) => None
      case (Cons(h, t), Empty) => Some(f(Some(h()), Option.empty[B]) -> (t(), empty[B]))
      case (Empty, Cons(h, t)) => Some(f(Option.empty[A], Some(h())) -> (empty[A] -> t()))
      case (Cons(h1, t1), Cons(h2, t2)) => Some(f(Some(h1()), Some(h2())) -> (t1() -> t2()))
    }


  def startsWith[A](s: Stream[A]): Boolean = {
    this.zipAll(s).takeWhile(x => !x._2.isEmpty).forAll( {case (h1, h2) => h1 == h2})
  }

  def tails: Stream[Stream[A]] = unfold(this){
    case Cons(h, t) => Some(cons(h(), t()), t())
    case empty => None
  }.append(Stream(empty))

  def scanRight[B](z: => B)(f: (A, => B) => B): Stream[B] =
    this.tails.map(_.foldRight(z)(f))
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)

  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  def from(n: Int): Stream[Int] = cons(n, from(n+1))

  def fibs: Stream[Int] = {
    def loop(m: Int, n: Int): Stream[Int] = {
      cons(m, loop(n, m+n))
    }
    loop(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case Some((x, y)) => cons(x, unfold(y)(f))
      case None => Empty
    }

  val onesUnfold: Stream[Int] = unfold(1)(_ => Some(1, 1))

  def constantUnfold[A](a: A): Stream[A] = unfold(a)(_ => Some(a, a))

  def fromUnfold(n: Int): Stream[Int] = unfold(n)(x => Some(x, x+1))

  def fibsUnfold: Stream[Int] =
    unfold(0, 1) {case (m, n) => Some(m, (n, m+n))}
}