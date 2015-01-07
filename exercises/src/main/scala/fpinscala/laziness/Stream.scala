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

  def startsWith[B](s: Stream[B]): Boolean = sys.error("todo")

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
  def from(n: Int): Stream[Int] = sys.error("todo")

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = sys.error("todo")
}