package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
case class Cons[+A](head: A, tail: List[A]) extends List[A] // Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`, which may be `Nil` or another `Cons`.

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  } 
  
  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }
  
  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42 
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101 
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }
  
  def sum2(ns: List[Int]) = 
    foldRight(ns, 0)((x,y) => x + y)
  
  def product2(ns: List[Double]) = 
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] =
    l match {
      case Nil => Nil
      case Cons(x, xs) => xs
    }

  def setHead[A](l: List[A], h: A): List[A] =
    Cons(h, tail(l))

  def drop[A](l: List[A], n: Int): List[A] = {
    def loop(l: List[A], i: Int): List[A] =
      if (i == n) l
      else loop(tail(l), i+1)

    loop(l, 0)
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
    def count(l: List[A], i: Int): Int = l match {
      case Nil => i
      case Cons(x, xs) => if (f(x)) count(xs, i + 1) else i
    }

    drop(l, count(l, 0))
  }

  def init[A](l: List[A]): List[A] = l match {
        case Nil => Nil
        case Cons(x, Nil) => Nil
        case Cons(x, Cons(y, Nil)) => Cons(x, Nil)
        case Cons(x, xs) => Cons(x, init(xs))
  }

  def length[A](l: List[A]): Int =
    foldRight(l, 0)((x, xs) => 1 + xs)

  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = {
    @annotation.tailrec
    def loop(l: List[A], acc: B): B =
      l match {
        case Nil => acc
        case Cons(x, xs) => loop(xs, f(acc, x))
      }

    loop(l, z)
  }

  def sum2L(ns: List[Int]) =
    foldLeft(ns, 0)((x,y) => x + y)

  def product2L(ns: List[Double]) =
    foldLeft(ns, 1.0)(_ * _)

  def lengthL[A](l: List[A]): Int =
    foldLeft(l, 0)((acc, x) => acc + 1)

  def reverse[A](l: List[A]): List[A] = {
    @annotation.tailrec
    def loop(l: List[A], acc: List[A]): List[A] =
      l match {
        case Nil => acc
        case Cons(x, xs) => loop(xs, Cons(x, acc))
      }

    loop(l, Nil)
  }

  def foldAppend[A](a1: List[A], a2: List[A]): List[A] =
    foldRight(a1, a2)(Cons(_, _))

  def addOne(l: List[Int]): List[Int] =
    foldRight(l, Nil: List[Int])((x, y) => Cons(x + 1, y))

  def convert(l: List[Double]): List[String] =
    foldRight(l, Nil: List[String])((x, y) => Cons(x.toString, y))

  def map[A,B](l: List[A])(f: A => B): List[B] =
    foldRight(l, Nil: List[B])((x, y) => Cons(f(x), y))

  def filter[A](l: List[A])(f: A => Boolean): List[A] =
    foldRight(l, Nil: List[A])((x, y) => if (f(x)) Cons(x, y) else y)

  def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] =
    foldRight(l, Nil: List[B])((x, y) => append(f(x), y))

  def filterFlatMap[A](l: List[A])(f: A => Boolean): List[A] =
    flatMap(l)(x => if (f(x)) Cons(x, Nil) else Nil)

  def add(a1: List[Int], a2: List[Int]): List[Int] =
    (a1, a2) match {
      case (Cons(x, xs), Cons(y, ys)) => Cons(x + y, add(xs, ys))
      case (_, _) => Nil
    }

  def zipWith[A](a1: List[A], a2: List[A])(f: (A, A) => A): List[A] =
    (a1, a2) match {
      case (Cons(x, xs), Cons(y, ys)) => Cons(f(x, y), zipWith(xs, ys)(f))
      case (_, _) => Nil
    }

}